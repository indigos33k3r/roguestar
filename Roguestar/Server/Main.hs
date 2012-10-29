{-# LANGUAGE TemplateHaskell, OverloadedStrings, ScopedTypeVariables, PatternGuards #-}

import Prelude
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.Read
import Data.Text.Encoding
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
import Roguestar.Lib.Data.PlayerState
import Roguestar.Lib.DBErrorFlag
import Roguestar.Lib.Perception
import Roguestar.Lib.SpeciesData
import Roguestar.Lib.ToolData
import Roguestar.Lib.Substances as Substances
import Roguestar.Lib.TerrainData as TerrainData
import Roguestar.Lib.CreatureData
import Roguestar.Lib.Facing
import Roguestar.Lib.Logging
import Roguestar.Lib.UnitTests
import Roguestar.Lib.DBData (Reference,ToolRef,toUID)
import Roguestar.Lib.HTML.Mustache
import Data.UUID
import qualified System.UUID.V4 as V4
import GHC.Stats
import Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import Text.Hastache

data App = App {
    _app_game_state :: GameState,
    _globals :: Aeson.Value }

makeLenses [''App]

appInit :: SnapletInit App App
appInit = makeSnaplet "roguestar-server-snaplet" "Roguestar Server" Nothing $
    do globals <- liftIO makeGlobals
       addRoutes [("/start", start),
                  ("/play", play),
                  ("/static", static),
                  ("/hidden", handle404),
                  ("/fail", handle500 (do error "my brain exploded")),
                  ("/help-actions", staticTemplate "static/help-actions.mustache"),
                  ("/help-map", staticTemplate "static/help-map.mustache"),
                  ("/help", staticTemplate "static/help.mustache"),
                  ("/participate", staticTemplate "static/participate.mustache"),
                  ("/feedback", postFeedback <|> staticTemplate "static/feedback.mustache"),
                  ("/feedback-thanks", staticTemplate "static/feedback-thanks.mustache"),
                  ("/options", options),
                  ("/start", start),
                  ("", staticTemplate "static/index.mustache")]
       config <- liftIO $ getConfiguration default_timeout
       game <- liftIO $ createGameState config
       wrapSite (<|> handle404)
       wrapSite handle500
       return $ App game globals

makeGlobals :: IO Aeson.Value
makeGlobals =
    do (unit_test_result,unit_tests_passed) <- liftIO runTests
       return $ object $ concat $ [
           (if not unit_tests_passed then ["failed-unit-tests" .= object ["text-content" .= String unit_test_result]] else [])
           ]

handle500 :: MonadSnap m => m a -> m ()
handle500 m = (m >> return ()) `CatchIO.catch` \(e::SomeException) -> do
    let t = LBS8.pack $ show e
    putResponse r
    writeBS "<html><head><title>Internal Server Error</title></head>"
    writeBS "<body><h1>Internal Server Error</h1>"
    writeBS "<p>A web handler threw an exception. Details:</p>"
    writeBS "<pre>\n"
    writeLBS $ htmlEscape t
    writeBS "\n</pre></body></html>"
  where
    r = setContentType "text/html" $
        setResponseStatus 500 "Internal Server Error" emptyResponse

getBasicState :: Handler App App Aeson.Value
getBasicState = gets _globals

handle404 :: Handler App App ()
handle404 =
    do modifyResponse $ setResponseCode 404
       staticTemplate "static/404.mustache"

static :: Handler App App ()
static = serveDirectory "./static/"

staticTemplate :: FilePath -> Handler App App ()
staticTemplate filepath = method GET $ ifTop $
    do globals <- getBasicState
       renderThemedPage filepath $ object [
           "static"          .= True,
           "server-globals"  .= globals,
           "path"            .= filepath ]

renderThemedPage :: FilePath -> Aeson.Value -> Handler App App ()
renderThemedPage filepath value =
    do theme <- liftM (fromMaybe "default") $ getQueryParam "theme"
       case theme of
           "default" -> writeLazyText =<< liftIO (renderPage filepath value)
           "json"    -> writeLBS $ Aeson.encode value

postFeedback :: Handler App App ()
postFeedback = method POST $ ifTop $
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
       let server_statistics = object [
               "server-statistics" .= show stats,
               "number-of-games"   .= number_of_games ]
       renderThemedPage "static/options.mustache" server_statistics

play :: Handler App App ()
play =
    do --resolveSnapshots
       g <- getGame
       player_state <- oops $ liftIO $ getPlayerState g
       route [("",ifTop $ method GET $ displayGameState player_state),
              ("reroll",method POST $ reroll player_state),
              ("accept",method POST $ accept player_state),
              ("move",method POST $ move),
              ("pop",method POST $ resolveOneSnapshot)]
              --("inventory",method GET $ displayInventory),
              --("pickup",method POST $ pickup),
              --("drop",method POST $ Main.drop),
              --("wield",method POST $ wield),
              --("unwield",method POST $ unwield)]

resolveAllSnapshots :: Handler App App ()
resolveAllSnapshots =
    do g <- getGame
       b <- oops $ liftIO $ hasSnapshot g
       case b of
           True ->
               do oops $ liftIO $ popSnapshot g
                  resolveAllSnapshots
           False ->
               do return ()

resolveOneSnapshot :: Handler App App ()
resolveOneSnapshot =
    do g <- getGame
       b <- oops $ liftIO $ hasSnapshot g
       when b $ oops $ liftIO $ popSnapshot g
       replay

routeRoguestar :: PlayerState -> [(BS.ByteString,PlayerState -> Handler App App ())] -> Handler App App ()
routeRoguestar ps xs = route $ map (\(bs,f) -> (bs,f ps)) xs

getGameState :: PlayerState -> Handler App App Aeson.Value
getGameState (SpeciesSelectionState Nothing) =
    do return $ object [
          "empty-game" .= True ]
getGameState (SpeciesSelectionState (Just creature)) =
    do return $ object [
          "rolled-creature" .= object [
              "species" .= show (creature_species creature)
              ]
           ]
getGameState (PlayerCreatureTurn creature_ref) =
    do g <- getGame
       map_content <- generateMapContent
       player_stats <- createStatsBlock
       messages <- liftM (reverse . take 5) $ liftIO $ getMessages g
       is_snapshot <- oops $ liftIO $ hasSnapshot g
       return $ object [ "play" .= object [
           "map" .= map_content,
           "statsblock" .= player_stats,
           "messages" .= messages,
           "is-snapshot" .= is_snapshot,
           "controls" .= not is_snapshot ]]
getGameState (GameOver PlayerIsDead) =
    do return $ object [
           "player-death" .= True ]
getGameState (GameOver PlayerIsVictorious) =
    do return $ object [
           "player-victory" .= True ]

displayGameState :: PlayerState -> Handler App App ()
displayGameState player_state =
    do game_state <- getGameState player_state
       renderThemedPage "static/play.mustache" game_state

data Inventory = Inventory {
    inventory_wielded :: Maybe VisibleObject,
    inventory_carried :: [VisibleObject],
    inventory_ground :: [VisibleObject] }

{-
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
-}
{-
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
-}

reroll :: PlayerState -> Handler App App ()
reroll (SpeciesSelectionState _) =
    do g <- getGame
       _ <- liftIO $ rerollStartingSpecies g
       replay
reroll _ = pass

accept :: PlayerState -> Handler App App ()
accept (SpeciesSelectionState (Just _)) =
    do g <- getGame
       _ <- liftIO $ beginGame g
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

{-
pickup :: Handler App App ()
pickup = commitBehavior =<< inventoryBehavior Pickup

drop :: Handler App App ()
drop = commitBehavior =<< inventoryBehavior Drop

wield :: Handler App App ()
wield = commitBehavior =<< inventoryBehavior Wield

unwield :: Handler App App ()
unwield = commitBehavior Unwield
-}

start :: Handler App App ()
start = on_get <|> on_post
    where on_get = method GET $ renderThemedPage "static/start.mustache" (object [])
          on_post = method POST $
              do game_state <- gets _app_game_state
                 config <- liftIO $ getConfiguration default_timeout
                 cookie <- liftIO $ createGame config game_state
                 modifyResponse $ addResponseCookie (Cookie "game-uuid" cookie Nothing Nothing Nothing False False)
                 replay

{-
inventoryBehavior :: (ToolRef -> Behavior) -> Handler App App Behavior
inventoryBehavior f =
    do g <- getGame
       uid <- liftM (either error fst . decimal . decodeUtf8 . fromMaybe (error "No UID")) $ getPostParam "uid"
       inventory <- oops $ collectInventory g
       let all_items = map visible_tool_ref $ concat [maybeToList $ inventory_wielded inventory, inventory_carried inventory, inventory_ground inventory]
           my_item = fromMaybe (error "No match in inventory.") $ List.find ((uid ==) . toUID) all_items
       return $ f my_item
-}

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
                  writeBS "<html><head><title>Gameplay Error</title></head>"
                  writeBS "<body><h1>Gameplay Error</h1>"
                  writeBS "<p>Roguestar returned an error condition. Details:</p>"
                  writeBS "<pre>\n"
                  writeLBS $ htmlEscape $ LBS8.pack bad
                  writeBS "\n</pre></body></html>"
                  finishWith =<< getResponse
  where
    r = setContentType "text/html" $
        setResponseStatus 500 "Internal Server Error" emptyResponse

-- Session timeout in seconds (should be 15 minutes)
default_timeout :: Integer
default_timeout = 60*15

getGame :: Handler App App Game
getGame =
    do game_session_cookie <- getsRequest $ List.find ((== "game-uuid") . cookieName) . rqCookies
       game_state <- gets _app_game_state
       config <- liftIO $ getConfiguration default_timeout
       case game_session_cookie of
           Just cookie ->
                   do result <- liftIO $ retrieveGame (cookieValue cookie) config game_state
                      case result of
                          Just g -> return g
                          Nothing -> redirect "/start"
           Nothing -> redirect "/start"

data MapData = MapData {
    md_visible_terrain :: Map.Map Position TerrainPatch,
    md_visible_objects :: Map.Map Position [VisibleObject],
    md_position_info :: (Facing,Position) }

generateMapContent :: Handler App App Aeson.Value
generateMapContent =
    do let (x,y) = (13,13) --we'll probably want to let the player customize this later
       g <- getGame
       player_state <- oops $ liftIO $ getSnapshotPlayerState g
       map_data <- oops $ liftIO $ perceiveSnapshot g $
           do visible_terrain <- liftM Map.fromList visibleTerrain
              visible_objects <- liftM stackVisibleObjects $ visibleObjects (const $ return True)
              my_position <- whereAmI
              return $ MapData visible_terrain visible_objects my_position
       return $ generateMapContent_ player_state (x,y) map_data

generateMapContent_ :: PlayerState -> (Integer,Integer) -> MapData -> Aeson.Value
generateMapContent_ _ (width,height) _ | width `mod` 2 == 0 || height `mod` 2 == 0 = error "Map widths and heights must be odd numbers"
generateMapContent_ player_state (width,height) (MapData visible_terrain visible_objects (_,Position (center_x,center_y))) = object [ "map-content" .= maplines ]
    where maplines =
              do y <- reverse $ [center_y - height `div` 2 .. center_y + width `div` 2]
                 return $ mapline y
          mapline y =
              do x <- [center_x - width `div` 2 .. center_x + width `div` 2]
                 return $ Aeson.toJSON $ mapstring x y
          mapstring x y =
                 let maybe_terrain = Map.lookup (Position (x,y)) visible_terrain
                     maybe_object = Map.lookup (Position (x,y)) visible_objects
                     rendered_json = case () of
                                () | Just (vob:_) <- maybe_object -> rendering player_state vob
                                () | Just terrain <- maybe_terrain -> rendering player_state terrain
                                () | otherwise -> object [ "t" .= ' ' ]
                     in rendered_json

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

data Style = Empty | Strong | Rocky | Icy | Plants | Dusty | Sandy | Wet | DeepWet | Molten | Gloomy | FaintMagic | StrongMagic | StrongDusty | WarpIn | Damage | Active

styleToCSS :: Style -> T.Text
styleToCSS Empty = ""
styleToCSS Strong = "B"
styleToCSS Rocky = "r"
styleToCSS Icy = "i"
styleToCSS Plants = "p"
styleToCSS Dusty = "d"
styleToCSS Sandy = "s"
styleToCSS Wet = "w"
styleToCSS Molten = "o"
styleToCSS Gloomy = "g"
styleToCSS FaintMagic = "a"
styleToCSS StrongMagic = "B A"
styleToCSS StrongDusty = "B d"
styleToCSS WarpIn = "B warpin"
styleToCSS Damage = "B damage"
styleToCSS Active = "B active"

class Charcoded a where
    codedRepresentation :: PlayerState -> a -> (Char,Style)
    rendering :: PlayerState -> a -> Aeson.Value
    rendering player_state a = object [ "t" .= t, "c" .= styleToCSS c ]
        where (t,c) = codedRepresentation player_state a

instance Charcoded a => Charcoded (Maybe a) where
    codedRepresentation player_state (Just a) = codedRepresentation player_state a
    codedRepresentation player_state Nothing = (' ',Empty)

instance Charcoded VisibleObject where
    codedRepresentation player_state (VisibleTool { visible_tool = t }) = codedRepresentation player_state t
    codedRepresentation player_state@(SnapshotEvent (TeleportEvent { teleport_event_creature = teleport_c }))
                                     (VisibleCreature { visible_creature_ref = this_c, visible_creature_species = s }) |
                                     teleport_c == this_c =
                                     (fst $ codedRepresentation player_state s, WarpIn)
    codedRepresentation player_state@(SnapshotEvent (SpawnEvent { spawn_event_creature = spawn_c }))
                                     (VisibleCreature { visible_creature_ref = this_c, visible_creature_species = s }) |
                                     spawn_c == this_c =
                                     (fst $ codedRepresentation player_state s, WarpIn)
    codedRepresentation player_state@(SnapshotEvent (AttackEvent { attack_event_target_creature = target_c }))
                                     (VisibleCreature { visible_creature_ref = this_c, visible_creature_species = s }) |
                                     target_c == this_c =
                                     (fst $ codedRepresentation player_state s, Damage)
    codedRepresentation player_state (VisibleCreature { visible_creature_ref = this_c, visible_creature_species = s }) |
                                     subjectOf player_state == Just this_c =
                                     (fst $ codedRepresentation player_state s, Active)
    codedRepresentation player_state (VisibleCreature { visible_creature_species = s }) = codedRepresentation player_state s
    codedRepresentation _            (VisibleBuilding{}) = ('#',StrongMagic)

instance Charcoded Tool where
    codedRepresentation _ (Sphere {}) = ('%',Strong)
    codedRepresentation _ (DeviceTool Gun _) = (')',Strong)
    codedRepresentation _ (DeviceTool Sword _) = (')',Strong)

instance Charcoded Species where
    codedRepresentation _ RedRecreant = ('r',Strong)
    codedRepresentation _ BlueRecreant = ('@',Strong)

instance Charcoded TerrainPatch where
    codedRepresentation _ RockFace          = ('#',Rocky)
    codedRepresentation _ Rubble            = ('.',Rocky)
    codedRepresentation _ Ore               = ('.',Rocky)
    codedRepresentation _ RockyGround       = ('.',Rocky)
    codedRepresentation _ Dirt              = ('.',Dusty)
    codedRepresentation _ Grass             = ('.',Plants)
    codedRepresentation _ Sand              = ('.',Sandy)
    codedRepresentation _ Desert            = ('.',Sandy)
    codedRepresentation _ Forest            = ('f',Plants)
    codedRepresentation _ DeepForest        = ('f',Plants)
    codedRepresentation _ TerrainData.Water = ('~',Wet)
    codedRepresentation _ DeepWater         = ('~',Gloomy)
    codedRepresentation _ Ice               = ('.',Icy)
    codedRepresentation _ Lava              = ('~',Molten)
    codedRepresentation _ Glass             = ('.',Gloomy)
    codedRepresentation _ RecreantFactory   = ('_',FaintMagic)
    codedRepresentation _ Upstairs          = ('<',StrongDusty)
    codedRepresentation _ Downstairs        = ('>',StrongDusty)

main :: IO ()
main =
    do initLogging WARNING
       config <- commandLineConfig emptyConfig
       serveSnaplet config appInit

