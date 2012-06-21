{-# LANGUAGE TemplateHaskell, OverloadedStrings, ScopedTypeVariables #-}

import Prelude
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Text.XHtmlCombinators.Escape as XH
import qualified Text.XmlHtml as X
import Control.Exception (SomeException)
import qualified Control.Monad.CatchIO as CatchIO
import Control.Monad.Trans
import Control.Monad.State
import Control.Applicative
import Control.Monad.ST
import Data.STRef
import Data.Array.ST
import Data.Array.IArray
import Data.Array.Unboxed
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Util.FileServe
import Snap.Http.Server.Config
import Data.Lens.Template
import Data.Maybe
import Data.Ord
import qualified Data.List as List
import qualified Data.Map as Map
import Roguestar.Lib.Roguestar
import Roguestar.Lib.PlayerState
import Roguestar.Lib.DBErrorFlag
import Roguestar.Lib.Perception
import Roguestar.Lib.SpeciesData
import Roguestar.Lib.ToolData
import Roguestar.Lib.Substances as Substances
import Roguestar.Lib.TerrainData as TerrainData
import Roguestar.Lib.CreatureData
import Roguestar.Lib.Facing

data App = App {
    _heist :: Snaplet (Heist App),
    _app_game :: Game }

makeLenses [''App]

instance HasHeist App where heistLens = subSnaplet heist

appInit :: SnapletInit App App
appInit = makeSnaplet "taskflask" "Task Flask" Nothing $
    do hs <- nestSnaplet "heist" heist $ heistInit "templates"
       addRoutes [("/play", play),
                  ("/static", static),
                  ("/hidden", handle404),
                  ("/fail", handle500 (do error "my brain exploded")),
                  ("", heistServe)]
       game <- liftIO newGame
       wrapHandlers (<|> handle404)
       wrapHandlers handle500
       return $ App hs game

handle500 :: MonadSnap m => m a -> m ()
handle500 m = (m >> return ()) `CatchIO.catch` \(e::SomeException) -> do
    let t = T.pack $ show e
    putResponse r
    writeBS "<html><head><title>Internal Server Error</title></head>"
    writeBS "<body><h1>Internal Server Error</h1>"
    writeBS "<p>A web handler threw an exception. Details:</p>"
    writeBS "<pre>\n"
    writeText $ XH.escape t
    writeBS "\n</pre></body></html>"
  where
    r = setContentType "text/html" $
        setResponseStatus 500 "Internal Server Error" emptyResponse

handle404 :: Handler App App ()
handle404 =
    do modifyResponse $ setResponseCode 404
       render "404"

static :: Handler App App ()
static = serveDirectory "./static/"

play :: Handler App App ()
play =
    do g <- getGame
       player_state <- liftIO $ getPlayerState g
       case player_state of
           Right something ->
               routeRoguestar something
                     [("",method GET . displayCurrentState),
                      ("maptext",method GET . const (createMap >>= writeText)),
                      ("reroll",method POST . reroll),
                      ("accept",method POST . accept),
                      ("move",method POST . move)]

routeRoguestar :: PlayerState -> [(BS.ByteString,PlayerState -> Handler App App ())] -> Handler App App ()
routeRoguestar ps xs = route $ map (\(bs,f) -> (bs,f ps)) xs

displayCurrentState :: PlayerState -> Handler App App ()
displayCurrentState (SpeciesSelectionState Nothing) =
    render "/hidden/play/empty-game"
displayCurrentState (SpeciesSelectionState (Just creature)) =
    renderWithSplices "/hidden/play/character-creation"
    [("content",return $ [X.TextNode $ T.pack $ "You are a " ++ show (creature_species creature) ++ "."])]
displayCurrentState (PlayerCreatureTurn creature_ref) =
    do map_text <- createMap
       player_stats <- createStatsBlock
       renderWithSplices "/hidden/play/normal-play"
           [("map",return $ [X.Element "pre" [] [X.TextNode map_text]]),
            ("statsblock",return $ [X.Element "pre" [] [X.TextNode player_stats]])]
displayCurrentState _ = pass

reroll :: PlayerState -> Handler App App ()
reroll (SpeciesSelectionState _) =
    do g <- getGame
       liftIO $ rerollStartingSpecies g
       replay
reroll _ = pass

accept :: PlayerState -> Handler App App ()
accept (SpeciesSelectionState (Just _)) =
    do g <- getGame
       liftIO $ beginGame g
       replay
accept _ = pass

move :: PlayerState -> Handler App App ()
move (PlayerCreatureTurn {}) =
    do g <- getGame
       behavior <- moveBehavior
       result <- liftIO $ behave g behavior
       case result of
           Right () -> return ()
       redirect "/play"

moveBehavior :: Handler App App Behavior
moveBehavior =
    do g <- getGame
       direction <- liftM (fromMaybe $ error "No direction identifier.") $ getPostParam "direction"
       mode <- liftM (fromMaybe $ error "No mode identifier.") $ getPostParam "mode"
       let facing = fromMaybe (error "Not a valid direction identifier.") $ stringToFacing direction
       action <- case mode of
                      _ | direction == "wait" -> return $ const Wait
                      "normal" ->
                          do result <- liftIO $ facingBehavior g facing
                             case result of
                                 Right x -> return $ const x
                      "step" -> return Step
                      "attack" -> return Attack
                      "fire" -> return Fire
                      "jump" -> return Jump
                      "turn" -> return TurnInPlace
       return $ action facing

replay :: Handler App App ()
replay = redirect "/play"

oops :: DBError -> Handler App App ()
oops db_error = writeBS $ "FIXME: this error message is useless."

getGame :: Handler App App Game
getGame = gets _app_game

data MapData = MapData {
    md_visible_terrain :: [(TerrainPatch,Position)],
    md_visible_objects :: Map.Map Position [VisibleObject],
    md_position_info :: (Facing,Position) }

createMap :: Handler App App T.Text
createMap =
    do let (x,y) = (21,21) --we'll probably want to let the player customize this later
       g <- getGame
       map_data <- liftIO $ perceive g $
           do visible_terrain <- visibleTerrain
              visible_objects <- liftM stackVisibleObjects $ visibleObjects (const $ return True)
              my_position <- whereAmI
              return $ MapData visible_terrain visible_objects my_position
       case map_data of
           Right map_data_ -> return $ constructMapText (x,y) map_data_

constructMapText :: (Integer,Integer) -> MapData -> T.Text
constructMapText (width,height) _ | width `mod` 2 == 0 || height `mod` 2 == 0 = error "Map widths and heights must be odd numbers"
constructMapText (width,height) (MapData visible_terrain visible_objects (_,Position (center_x,center_y))) = T.unfoldr f (False,0)
    where f :: (Bool,Int) -> Maybe (Char, (Bool,Int))
          f (False,i) = if i > snd (bounds char_array)
                            then Nothing
                            else Just (char_array ! i,(succ i `mod` fromInteger width == 0,succ i))
          f (True,i)  = Just ('\n',(False,i))
          x_adjust = center_x - (width-1) `div` 2
          y_adjust = center_y - (height-1) `div` 2
          array_length = fromInteger $ width*height
          char_array :: UArray Int Char
          char_array = runSTUArray $
              do ax <- newArray (0,array_length-1) ' '
                 forM_ visible_terrain $ \(tp,Position (x,y)) ->
                     do let i = fromInteger $ (x-x_adjust) + (height-(y-y_adjust)-1)*width
                        when (i >= 0 && i < array_length-1) $
                            writeArray ax i $ charcodeOf tp
                 forM_ (Map.assocs visible_objects) $ \(Position (x,y), vobs) ->
                     do let i = fromInteger $ (x-x_adjust) + (height-(y-y_adjust)-1) * width
                        when (i >= 0 && i < array_length-1) $
                            writeArray ax i $ charcodeOf vobs 
                 return ax

data StatsData = StatsData {
    stats_health :: CreatureHealth,
    stats_compass :: Facing }

createStatsBlock :: Handler App App T.Text
createStatsBlock =
    do g <- getGame
       stats <- liftIO $ perceive g $
           do health <- myHealth
              facing <- compass
              return $ StatsData {
                  stats_health = health,
                  stats_compass = facing }
       case stats of
           Right stats_ ->
               return $ T.concat [
                   "Health: ",
                   T.pack $ show $ creature_absolute_health $ stats_health stats_,
                   "/",
                   T.pack $ show $ creature_max_health $ stats_health stats_,
                   "\n",
                   "Compass: ",
                   T.pack $ show $ stats_compass stats_]

class Charcoded a where
    charcodeOf :: a -> Char

instance Charcoded a => Charcoded [a] where
    charcodeOf (a:as) = charcodeOf a
    charcodeOf [] = ' '

instance Charcoded VisibleObject where
    charcodeOf (VisibleTool { visible_tool = t }) = charcodeOf t
    charcodeOf (VisibleCreature { visible_creature_species = s }) = charcodeOf s
    charcodeOf (VisibleBuilding{}) = '#'
    
instance Charcoded Tool where
    charcodeOf (Sphere {}) = '%'
    charcodeOf (DeviceTool Gun _) = ')'
    charcodeOf (DeviceTool Sword _) = ')'

instance Charcoded Species where
    charcodeOf Anachronid = 'A'
    charcodeOf Androsynth = 'Y'
    charcodeOf Ascendant = 'V'
    charcodeOf Caduceator = 'C'
    charcodeOf DustVortex = 'v'
    charcodeOf Encephalon = 'E'    
    charcodeOf Goliath = 'G'
    charcodeOf Hellion = 'H'
    charcodeOf Kraken = 'K'
    charcodeOf Myrmidon = 'M'
    charcodeOf Perennial = 'f'
    charcodeOf Recreant = 'r'
    charcodeOf Reptilian = 'R'

instance Charcoded TerrainPatch where
    charcodeOf RockFace          = '#'
    charcodeOf Rubble            = '~'
    charcodeOf Ore               = '~'
    charcodeOf RockyGround       = '.'
    charcodeOf Dirt              = '.'
    charcodeOf Grass             = '.'
    charcodeOf Sand              = '~'
    charcodeOf Desert            = '~'
    charcodeOf Forest            = 'f'
    charcodeOf DeepForest        = 'f'
    charcodeOf TerrainData.Water = '~'
    charcodeOf DeepWater         = '~'
    charcodeOf Ice               = '.'
    charcodeOf Lava              = '~'
    charcodeOf Glass             = '.'
    charcodeOf RecreantFactory   = '_'
    charcodeOf Upstairs          = '>'
    charcodeOf Downstairs        = '<'

main :: IO ()
main = serveSnaplet defaultConfig appInit

