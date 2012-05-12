{-# LANGUAGE TemplateHaskell, OverloadedStrings, ScopedTypeVariables #-}

import Prelude
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Text.XHtmlCombinators.Escape as XH
import Control.Exception (SomeException)
import qualified Control.Monad.CatchIO as CatchIO
import Control.Monad.Trans
import Control.Applicative
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Util.FileServe
import Snap.Http.Server.Config
import Data.Lens.Template
import Data.Maybe
import Data.Ord
import Roguestar.Lib.Roguestar

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
play = ifTop $
    do writeBS "hello, world!"

main :: IO ()
main = serveSnaplet defaultConfig appInit

