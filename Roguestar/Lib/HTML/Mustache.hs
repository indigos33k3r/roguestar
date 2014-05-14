
module Roguestar.Lib.HTML.Mustache (renderPage, roguestar_muconfig) where

import Text.Hastache
import Data.Text.Lazy as T
import Data.Aeson as Aeson
import Data.HashMap.Strict as Map
import Data.Vector as V
import Control.Monad.IO.Class

roguestar_muconfig :: (MonadIO m) => MuConfig m
roguestar_muconfig = defaultConfig {
    muEscapeFunc = htmlEscape,
    muTemplateFileDir = Just "static/templates/",
    muTemplateFileExt = Just ".mustache" }

mkAesonContext :: (Monad m) => Aeson.Value -> MuContext m
mkAesonContext (Object obj) key = return $ maybe MuNothing aesonToMu $ Map.lookup key obj
mkAesonContext x            _   = return $ aesonToMu x

aesonToMu :: (Monad m) => Aeson.Value -> MuType m
aesonToMu obj@(Object {}) = MuList [mkAesonContext obj]
aesonToMu (Array arr) = MuList $ Prelude.map mkAesonContext $ V.toList arr
aesonToMu (String txt) = MuVariable txt
aesonToMu (Number n) = MuVariable $ show n
aesonToMu (Bool bool) = MuBool bool
aesonToMu Null = MuNothing

renderPage :: FilePath -> Aeson.Value -> IO T.Text
renderPage filepath value = hastacheFile roguestar_muconfig filepath (mkAesonContext value)
