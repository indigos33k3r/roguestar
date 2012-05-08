{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

import qualified Data.Text as T
import Snap
import Snap.Util.FileServe
import Data.Maybe
import Data.Ord

data App = App

makeLenses [''App]

appInit :: SnapletInit App App
appInit = makeSnaplet "taskflask" "Task Flask" Nothing $
    do addRoutes [("/static", static)]
       return App

static :: Handler b v ()
static = serveDirectory "./static/"

main :: IO ()
main = serveSnaplet defaultConfig appInit

