{-# LANGUAGE TemplateHaskell, OverloadedStrings, ScopedTypeVariables #-}

import Prelude
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import Data.Text.Read
import Data.Text.Encoding
import qualified Text.XHtmlCombinators.Escape as XH
import qualified Text.XmlHtml as X
import Text.Templating.Heist
import Control.Exception (SomeException)
import qualified Control.Monad.CatchIO as CatchIO
import Control.Monad.Trans
import Control.Monad.State
import Control.Applicative
import Control.Monad.ST
import Control.Concurrent.STM
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
import Roguestar.Lib.Logging
import Roguestar.Lib.DBData (Reference,ToolRef,toUID)
import Data.UUID
import qualified System.UUID.V4 as V4
import GHC.Stats

data App = App {
    _heist :: Snaplet (Heist App),
    _app_game_state :: GameState }

makeLenses [''App]

instance HasHeist App where heistLens = subSnaplet heist

appInit :: SnapletInit App App
appInit = makeSnaplet "roguestar-server-snaplet" "Roguestar Server" Nothing $
    do hs <- nestSnaplet "heist" heist $ heistInit "templates"
       addRoutes [("/start", start),
                  ("/play", play),
                  ("/static", static),
                  ("/hidden", handle404),
                  ("/fail", handle500 (do error "my brain exploded")),
                  ("/feedback", feedback),
                  ("/options", options),
                  ("", heistServe)]
       game <- liftIO createGameState
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

feedback :: Handler App App ()
feedback = method POST $
    do feedback <- liftM (fromMaybe $ error "No feedback.") $ getPostParam "feedback"
       liftIO $
           do uuid <- V4.uuid
              BS.writeFile ("./feedback/" ++ show uuid) feedback
       redirect "/feedback-thanks/"

options :: Handler App App ()
options =
    do stats <- liftIO $ getGCStats
       game_state <- gets _app_game_state
       number_of_games <- liftIO $ getNumberOfGames game_state 
       renderWithSplices "/hidden/options"
           [("serverstats",
            return $ [X.Element "p" [] [X.TextNode $ T.pack $ show stats],
                      X.Element "p" [] [X.TextNode "# of games ", X.TextNode $ T.pack $ show number_of_games]])]

play :: Handler App App ()
play =
    do resolveSnapshots
       g <- getGame
       player_state <- oops $ liftIO $ getPlayerState g
       route [("",ifTop $ method GET $ displayCurrentState player_state),
              ("maptext",method GET $ createMap >>= writeText),
              ("reroll",method POST $ reroll player_state),
              ("accept",method POST $ accept player_state),
              ("move",method POST $ move),
              ("inventory",method GET $ displayInventory),
              ("pickup",method POST $ pickup),
              ("drop",method POST $ Main.drop),
              ("wield",method POST $ wield),
              ("unwield",method POST $ unwield)]

resolveSnapshots :: Handler App App ()
resolveSnapshots =
    do g <- getGame
       b <- oops $ liftIO $ hasSnapshot g
       case b of
           True ->
               do oops $ liftIO $ popSnapshot g
                  resolveSnapshots
           False ->
               do return ()

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
       messages <- liftM (reverse . take 5) $ liftIO . getMessages =<< getGame
       renderWithSplices "/hidden/play/normal"
           [("map",return $ [X.Element "pre" [] [X.TextNode map_text]]),
            ("statsblock",return $ map (\x -> X.Element "p" [] [X.TextNode x]) player_stats),
            ("messages",return $ map (\x -> X.Element "p" [] [X.TextNode x]) messages)]
displayCurrentState (GameOver PlayerIsDead) =
    do render "/hidden/play/failure"
displayCurrentState (GameOver PlayerIsVictorious) =
    do render "/hidden/play/success"
displayCurrentState _ = pass

data Inventory = Inventory {
    inventory_wielded :: Maybe VisibleObject,
    inventory_carried :: [VisibleObject],
    inventory_ground :: [VisibleObject] }

collectInventory :: Game -> Handler App App (Either DBError Inventory)
collectInventory g = liftIO $ perceive g $
    do visible_objects <- liftM stackVisibleObjects $ visibleObjects (const $ return True)
       (_,my_position) <- whereAmI
       let vobs_at_my_position = Map.lookup my_position visible_objects
       my_inventory <- myInventory
       return $ Inventory {
           inventory_wielded =
               do me <- List.find isVisibleCreature $ fromMaybe [] vobs_at_my_position
                  visible_creature_wielding me,
           inventory_ground = filter isVisibleTool $ fromMaybe [] vobs_at_my_position,
           inventory_carried = my_inventory }

displayInventory :: Handler App App ()
displayInventory =
    do g <- getGame
       inventory_result <- collectInventory g
       inventory <- case inventory_result of
           Right inventory -> return inventory
       renderWithSplices "/hidden/play/inventory"
           [("wielded", return $ inventoryList [("Unwield","carried","/play/unwield"),("Drop","ground","/play/drop")] $ maybeToList $ inventory_wielded inventory),
            ("carried", return $ inventoryList [("Wield","wielded","/play/wield"),("Drop","ground","/play/drop")] $ inventory_carried inventory),
            ("ground",  return $ inventoryList [("Wield","wielded","/play/wield"),("Pickup","carried","/play/pickup")] $ inventory_ground inventory)]

inventoryList :: [(T.Text,T.Text,T.Text)] -> [VisibleObject] -> Template
inventoryList inventory_actions = map inventoryItem
    where inventoryItem (VisibleTool { visible_tool_ref = tool_ref, visible_tool = tool }) = 
                X.Element "div" [("class","inventoryitem")] $ [X.Element "p" [] [X.TextNode (toolName tool)]] ++ concatMap (inventoryAction tool_ref) inventory_actions

inventoryAction :: ToolRef -> (T.Text,T.Text,T.Text) -> Template
inventoryAction tool_ref (action_name,css_class,action_path) =
    [X.Element "form" [("action",action_path),("method","post")]
        [X.Element "button" [("type","submit"),("class",css_class)] [X.TextNode action_name],
         X.Element "input" [("type","hidden"),("name","uid"),("value",T.pack $ show $ toUID tool_ref)] []]]

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

move :: Handler App App ()
move = commitBehavior =<< moveBehavior

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
     
pickup :: Handler App App ()
pickup = commitBehavior =<< inventoryBehavior Pickup

drop :: Handler App App ()
drop = commitBehavior =<< inventoryBehavior Drop

wield :: Handler App App ()
wield = commitBehavior =<< inventoryBehavior Wield

unwield :: Handler App App ()
unwield = commitBehavior Unwield

start :: Handler App App ()
start = on_get <|> on_post
    where on_get = method GET $ render "/hidden/start"
          on_post = method POST $ 
              do game_state <- gets _app_game_state
                 cookie <- liftIO $ createGame game_state
                 modifyResponse $ addResponseCookie (Cookie "game-uuid" cookie Nothing Nothing Nothing False False)
                 replay

inventoryBehavior :: (ToolRef -> Behavior) -> Handler App App Behavior
inventoryBehavior f =
    do g <- getGame
       uid <- liftM (either error fst . decimal . decodeUtf8 . fromMaybe (error "No UID")) $ getPostParam "uid"
       inventory <- oops $ collectInventory g
       let all_items = map visible_tool_ref $ concat [maybeToList $ inventory_wielded inventory, inventory_carried inventory, inventory_ground inventory]
           my_item = fromMaybe (error "No match in inventory.") $ List.find ((uid ==) . toUID) all_items
       return $ f my_item

commitBehavior :: Behavior -> Handler App App ()
commitBehavior behavior =
    do g <- getGame
       result <- oops $ liftIO $ behave g behavior
       replay

replay :: Handler App App ()
replay = redirect "/play"

oops :: Handler App App (Either DBError a) -> Handler App App a
oops action =
    do result <- action
       case result of
           Right good -> return good
           Left (DBErrorFlag flag) ->
               do g <- getGame
                  liftIO $ putMessage g $ unpackError flag
                  replay
                  return $ error "oops:  Unreachable code."
           Left (DBError bad) -> 
               do putResponse r
                  writeText "<html><head><title>Gameplay Error</title></head>"
                  writeText "<body><h1>Gameplay Error</h1>"
                  writeText "<p>Roguestar returned an error condition. Details:</p>"
                  writeText "<pre>\n"
                  writeText $ XH.escape $ T.pack bad
                  writeText "\n</pre></body></html>"
                  finishWith =<< getResponse
  where
    r = setContentType "text/html" $
        setResponseStatus 500 "Internal Server Error" emptyResponse

getGame :: Handler App App Game
getGame = 
    do game_session_cookie <- getsRequest $ List.find ((== "game-uuid") . cookieName) . rqCookies
       game_state <- gets _app_game_state
       case game_session_cookie of
           Just cookie ->
                   do result <- liftIO $ retrieveGame (cookieValue cookie) game_state
                      case result of
                          Just g -> return g
                          Nothing -> redirect "/start"
           Nothing -> redirect "/start"

data MapData = MapData {
    md_visible_terrain :: [(TerrainPatch,Position)],
    md_visible_objects :: Map.Map Position [VisibleObject],
    md_position_info :: (Facing,Position) }

createMap :: Handler App App T.Text
createMap =
    do let (x,y) = (21,21) --we'll probably want to let the player customize this later
       g <- getGame
       map_data <- oops $ liftIO $ perceive g $
           do visible_terrain <- visibleTerrain
              visible_objects <- liftM stackVisibleObjects $ visibleObjects (const $ return True)
              my_position <- whereAmI
              return $ MapData visible_terrain visible_objects my_position
       return $ constructMapText (x,y) map_data

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

createStatsBlock :: Handler App App [T.Text]
createStatsBlock =
    do g <- getGame
       stats <- oops $ liftIO $ perceive g $
           do health <- myHealth
              facing <- compass
              return $ StatsData {
                  stats_health = health,
                  stats_compass = facing }
       return $ [
           T.concat ["Health: ",
                     T.pack $ show $ creature_absolute_health $ stats_health stats,
                     "/",
                     T.pack $ show $ creature_max_health $ stats_health stats],
           T.concat ["Compass: ",
                     T.pack $ show $ stats_compass stats]]

class Charcoded a where
    charcodeOf :: a -> Char

instance Charcoded a => Charcoded [a] where
    charcodeOf (a:as) = charcodeOf a
    charcodeOf [] = ' '

instance Charcoded VisibleObject where
    charcodeOf (VisibleTool { visible_tool = t }) = charcodeOf t
    charcodeOf (VisibleCreature { visible_creature_species = s }) = charcodeOf s
    charcodeOf (VisibleBuilding{}) = 'X'

instance Charcoded Tool where
    charcodeOf (Sphere {}) = '%'
    charcodeOf (DeviceTool Gun _) = ')'
    charcodeOf (DeviceTool Sword _) = ')'

instance Charcoded Species where
    charcodeOf RedRecreant = 'r'
    charcodeOf BlueRecreant = '@'

instance Charcoded TerrainPatch where
    charcodeOf RockFace          = '#'
    charcodeOf Rubble            = '.'
    charcodeOf Ore               = '.'
    charcodeOf RockyGround       = '.'
    charcodeOf Dirt              = '.'
    charcodeOf Grass             = '.'
    charcodeOf Sand              = '.'
    charcodeOf Desert            = '.'
    charcodeOf Forest            = 'f'
    charcodeOf DeepForest        = 'f'
    charcodeOf TerrainData.Water = '~'
    charcodeOf DeepWater         = '~'
    charcodeOf Ice               = '.'
    charcodeOf Lava              = '~'
    charcodeOf Glass             = '.'
    charcodeOf RecreantFactory   = '_'
    charcodeOf Upstairs          = '<'
    charcodeOf Downstairs        = '>'

main :: IO ()
main =
    do initLogging WARNING
       serveSnaplet defaultConfig appInit

