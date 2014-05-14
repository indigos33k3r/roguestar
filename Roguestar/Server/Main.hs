{-# LANGUAGE TemplateHaskell, OverloadedStrings, ScopedTypeVariables, PatternGuards #-}

import Prelude
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.Encoding
import Control.Exception (SomeException)
import qualified Control.Monad.CatchIO as CatchIO
import Control.Monad.Trans
import Control.Monad.State
import Control.Applicative
import Snap.Core
import Snap.Snaplet
import Snap.Util.FileServe
import Snap.Http.Server.Config
--import Data.Lens.Template
import Data.Maybe
import qualified Data.List as List
import qualified Data.Map as Map
import Roguestar.Lib.Roguestar
import Roguestar.Lib.Data.PlayerState
import Roguestar.Lib.Data.ErrorData
import Roguestar.Lib.Perception
import Roguestar.Lib.Data.SpeciesData
import Roguestar.Lib.Data.ToolData
import Roguestar.Lib.Data.TerrainData as TerrainData
import Roguestar.Lib.Data.FacingData
import Roguestar.Lib.Logging
import Roguestar.Lib.UnitTests
import Roguestar.Lib.HTML.Mustache
import Roguestar.Lib.Data.BehaviorData
import qualified System.UUID.V4 as V4
import GHC.Stats
import Data.Aeson as Aeson
import Text.Hastache

data App = App {
    _app_game_state :: GameState,
    _globals :: Aeson.Value }

appInit :: SnapletInit App App
appInit = makeSnaplet "roguestar-server-snaplet" "Roguestar Server" Nothing $
    do the_globals <- liftIO makeGlobals
       addRoutes [("/start", start),
                  ("/play", play),
                  ("/static", static),
                  ("/fail404", handle404),
                  ("/fail500", handle500 (do error "my brain exploded")),
                  ("/feedback", postFeedback),
                  ("/feedback-thanks", staticTemplate "static/templates/feedback-thanks.mustache"),
                  ("/options", options),
                  ("", staticTemplate "static/templates/index.mustache")]
       config <- liftIO $ getConfiguration default_timeout
       game <- liftIO $ createGameState config
       wrapSite (<|> handle404)
       wrapSite handle500
       return $ App game the_globals

makeGlobals :: IO Aeson.Value
makeGlobals =
    do (unit_test_result,unit_tests_passed) <- liftIO runTests
       return $ object $ concat $ [
           (if not unit_tests_passed then ["failed-unit-tests" .= object ["text-content" .= String unit_test_result]]
                                     else ["passed-unit-tests" .= object ["text-content" .= String unit_test_result]])
           ]

handle500 :: MonadSnap m => m a -> m ()
handle500 m = (m >> return ()) `CatchIO.catch` \(e::SomeException) -> do
    let t = LT.pack $ show e
    putResponse r
    writeText "<html><head><title>oh noes</title></head>"
    writeText "<body style=\"background:black; color:white;\"><h1>theirs a porblem</h1>"
    writeText "<p>i'm so sorry the website broke a little</p>"
    writeText "<img src=\"static/art/TabularMonstrosity.svg\"/>"
    writeText "<pre>\n"
    writeLazyText $ htmlEscape t
    writeText "\n</pre></body></html>"
  where
    r = setContentType "text/html" $
        setResponseStatus 500 "Internal Server Error" emptyResponse

getBasicState :: Handler App App Aeson.Value
getBasicState = gets _globals

handle404 :: Handler App App ()
handle404 =
    do modifyResponse $ setResponseCode 404
       staticTemplate "static/templates/404.mustache"

static_directory_config :: DirectoryConfig (Handler App App)
static_directory_config = fancyDirectoryConfig

-- | Serves the directory named "static".
static :: Handler App App ()
static = serveDirectoryWith static_directory_config "./static/"

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
           unrecognized_theme -> fail $ "Unrecognized theme " ++ (T.unpack $ decodeUtf8 unrecognized_theme)

postFeedback :: Handler App App ()
postFeedback = method POST $ ifTop $
    do feedback <- liftM (fromMaybe $ error "No feedback.") $ getPostParam "feedback"
       liftIO $
           do uuid <- V4.uuid
              BS.writeFile ("./feedback/" ++ show uuid) feedback
       redirect "/feedback-thanks/"

getMemoryStatistics :: IO String
getMemoryStatistics =
    do -- this requires ghc 7.6 but I'm still living in 7.4 world
       -- just disable for now
       -- ok <- getGCStatsEnabled
       let ok = False
       if ok then liftM show $ getGCStats
             else return "(Memory statistics disabled. Use +RTS -T -RTS to collect memory statistics.)"

options :: Handler App App ()
options =
    do stats <- liftIO $ getMemoryStatistics
       game_state <- gets _app_game_state
       globals <- gets _globals
       number_of_games <- liftIO $ getNumberOfGames game_state
       let server_statistics = object [
               "server-statistics" .= show stats,
               "number-of-games"   .= number_of_games,
               "server-globals"    .= globals ]
       renderThemedPage "static/templates/options.mustache" server_statistics

play :: Handler App App ()
play =
    do --resolveSnapshots
       g <- getGame
       player_state <- oops $ liftIO $ getSnapshotPlayerState g
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

resolveOneSnapshot :: Handler App App ()
resolveOneSnapshot =
    do g <- getGame
       b <- oops $ liftIO $ hasSnapshot g
       when b $ oops $ liftIO $ popSnapshot g
       replay

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
getGameState (SnapshotEvent _) = getGameStateWhileInPlay
getGameState (PlayerMonsterTurn _) = getGameStateWhileInPlay
getGameState (GameOver PlayerIsDead) =
    do return $ object [
           "player-death" .= True ]
getGameState (GameOver PlayerIsVictorious) =
    do return $ object [
           "player-victory" .= True ]

getGameStateWhileInPlay :: Handler App App Aeson.Value
getGameStateWhileInPlay =
    do g <- getGame
       map_content <- generateMapContent
       player_stats <- createStatsBlock
       valid_controls <- getValidControls
       messages <- liftM (reverse . take 5) $ liftIO $ getMessages g
       is_snapshot <- oops $ liftIO $ hasSnapshot g
       return $ object [ "play" .= object [
           "map" .= map_content,
           "statsblock" .= player_stats,
           "messages" .= messages,
           "is-snapshot" .= is_snapshot,
           "controls" .= if is_snapshot then toJSON False else valid_controls ]]

displayGameState :: PlayerState -> Handler App App ()
displayGameState player_state =
    do game_state <- getGameState player_state
       renderThemedPage "static/templates/play.mustache" game_state

{-
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
               do me <- List.find isVisibleMonster $ fromMaybe [] vobs_at_my_position
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

-- |
-- Determines the correct "Behavior" type for a direction (north/south/east/west) move.
-- This information is either implicit (moving into an enemy is assumed to be attacking)
-- or it can be explicit because the player set a movement type.
moveBehavior :: Handler App App Behavior
moveBehavior =
    do g <- getGame
       direction <- liftM (fromMaybe $ error "No direction identifier.") $ getPostParam "direction"
       mode <- liftM (fromMaybe $ error "No mode identifier.") $ getPostParam "mode"
       case direction of
           "wait" -> return Wait
           _ ->
               do let facing = fromMaybe (error "Not a valid direction identifier.") $ stringToFacing direction
                  action <- case mode of
                      "normal" -> oops $ liftIO $ Roguestar.Lib.Roguestar.facingBehavior g facing
                      "step" -> return Step
                      "attack" -> return Attack
                      "fire" -> return Fire
                      "jump" -> return Jump
                      "turn" -> return TurnInPlace
                      "holographic-trail" -> return HolographicTrailStep
                      "temporal-web" -> return TemporalWebStep
                      other  -> fail $ "moveBehavior: Didn't recognize: " ++ T.unpack (decodeUtf8 other)
                  return $ FacingBehavior action facing

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

-- |
-- GET:  Offers the player the various game modes they are allowed (such as the tutorial game).
-- POST: Initializes all of the data structures and cookies to start a new game.
--
start :: Handler App App ()
start = on_get <|> on_post
    where on_get = method GET $ renderThemedPage "static/templates/start.mustache" (object [])
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
       _ <- oops $ liftIO $ performBehavior g behavior
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
                  writeText "<html><head><title>gameplay error</title></head>"
                  writeText "<body style=\"background:black; color:white;\"><h1>oh oh the game is confused</h1>"
                  writeText "<p>some things happened and i didn't know what do</p>"
                  writeText "<img src=\"static/art/TabularMonstrosity.svg\"/>"
                  writeText "<pre>\n"
                  writeLazyText $ htmlEscape $ LT.pack bad
                  writeText "\n</pre></body></html>"
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
    _md_visible_terrain :: Map.Map Position Terrain,
    _md_visible_objects :: Map.Map Position [VisibleObject],
    _md_position_info :: (Facing,Position) }

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

createStatsBlock :: Handler App App Aeson.Value
createStatsBlock =
    do g <- getGame
       oops $ liftIO $ perceiveSnapshot g $
           do health <- myHealth
              facing <- compass
              return $ object [
                 "health" .= health,
                 "compass" .= facing ]

getValidControls :: Handler App App Aeson.Value
getValidControls =
    do g <- getGame
       can_teleport <- oops $ liftIO $ perceiveSnapshot g $ Roguestar.Lib.Perception.isBehaviorAvailable (FacingBehavior Jump Here)
       can_holographic_trail <- oops $ liftIO $ perceiveSnapshot g $ Roguestar.Lib.Perception.isBehaviorAvailable (FacingBehavior HolographicTrailStep Here)
       can_temporal_web <- oops $ liftIO $ perceiveSnapshot g $ Roguestar.Lib.Perception.isBehaviorAvailable (FacingBehavior TemporalWebStep Here)
       return $ object [
           "teleport" .= can_teleport,
           "holographic-trail" .= can_holographic_trail,
           "temporal-web" .= can_temporal_web ]

data Style = Empty | Strong | Rocky | Icy | Plants | Dusty | Sandy | Wet | Molten | Gloomy | Magic | StrongMagic | StrongDusty | WarpIn | Damage | Active | BlueIFF | RedIFF

styleToCSS :: Style -> T.Text
styleToCSS Empty = ""
styleToCSS Strong = "B"
styleToCSS Rocky = "r"
styleToCSS Icy = "i"
styleToCSS Plants = "p"
styleToCSS Dusty = "d"
styleToCSS Sandy = "s"
styleToCSS Wet = "w"
styleToCSS Molten = "m"
styleToCSS Gloomy = "g"
styleToCSS Magic = "a"
styleToCSS StrongMagic = "B a"
styleToCSS StrongDusty = "B d"
styleToCSS WarpIn = "B warpin"
styleToCSS Damage = "B damage"
styleToCSS Active = "B active"
styleToCSS BlueIFF = "blue"
styleToCSS RedIFF = "red"

class Charcoded a where
    codedRepresentation :: PlayerState -> a -> (Char,Style)
    rendering :: PlayerState -> a -> Aeson.Value
    rendering player_state a = object [ "t" .= t, "c" .= styleToCSS c ]
        where (t,c) = codedRepresentation player_state a

instance Charcoded a => Charcoded (Maybe a) where
    codedRepresentation player_state (Just a) = codedRepresentation player_state a
    codedRepresentation _            Nothing = (' ',Empty)

instance Charcoded VisibleObject where
    codedRepresentation player_state (VisibleTool { visible_tool = t }) = codedRepresentation player_state t
    codedRepresentation player_state@(SnapshotEvent (TeleportEvent { teleport_event_creature = teleport_c }))
                                     (VisibleMonster { visible_creature_ref = this_c, visible_creature_species = s }) |
                                     teleport_c == this_c =
                                     (fst $ codedRepresentation player_state s, WarpIn)
    codedRepresentation player_state@(SnapshotEvent (SpawnEvent { spawn_event_creature = spawn_c }))
                                     (VisibleMonster { visible_creature_ref = this_c, visible_creature_species = s }) |
                                     spawn_c == this_c =
                                     (fst $ codedRepresentation player_state s, WarpIn)
    codedRepresentation player_state@(SnapshotEvent (AttackEvent { attack_event_target_creature = target_c }))
                                     (VisibleMonster { visible_creature_ref = this_c, visible_creature_species = s }) |
                                     target_c == this_c =
                                     (fst $ codedRepresentation player_state s, Damage)
    codedRepresentation player_state (VisibleMonster { visible_creature_ref = this_c, visible_creature_species = s }) |
                                     subjectOf player_state == Just this_c =
                                     (fst $ codedRepresentation player_state s, Active)
    codedRepresentation player_state (VisibleMonster { visible_creature_species = s }) = codedRepresentation player_state s
    codedRepresentation _            (VisibleBuilding{}) = ('#',StrongMagic)

instance Charcoded Tool where
    codedRepresentation _ (Sphere {}) = ('%',Strong)
    codedRepresentation _ (DeviceTool Gun _) = (')',Strong)
    codedRepresentation _ (DeviceTool Sword _) = (')',Strong)

instance Charcoded Species where
    codedRepresentation _ RedRecreant =        ('r',Strong)
    codedRepresentation _ LavaLarva =          ('l',Strong)
    codedRepresentation _ Anachronid =         ('X',Strong)
    codedRepresentation _ TabularMonstrosity = ('m',Strong)

instance Charcoded Terrain where
    codedRepresentation _ RockFace          = ('#',Rocky)
    codedRepresentation _ ForceField        = ('#',Magic)
    codedRepresentation _ RockyGround       = ('.',Rocky)
    codedRepresentation _ Dirt              = ('.',Dusty)
    codedRepresentation _ Grass             = ('.',Plants)
    codedRepresentation _ Sand              = ('.',Sandy)
    codedRepresentation _ Forest            = ('f',Plants)
    codedRepresentation _ TerrainData.Water = ('~',Wet)
    codedRepresentation _ Ice               = ('.',Icy)
    codedRepresentation _ Lava              = ('~',Molten)
    codedRepresentation _ Glass             = ('.',Gloomy)
    codedRepresentation _ RecreantFactory   = ('_',Magic)
    codedRepresentation _ Upstairs          = ('<',StrongDusty)
    codedRepresentation _ Downstairs        = ('>',StrongDusty)

main :: IO ()
main =
    do initLogging DEBUG --WARNING
       config <- commandLineConfig emptyConfig
       serveSnaplet config appInit

