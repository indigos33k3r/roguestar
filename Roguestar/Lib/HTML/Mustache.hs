
module Roguestar.Lib.HTML.Mustache (renderPage, roguestar_muconfig) where

import Text.Hastache
import Data.Text.Lazy as T
import Data.Aeson as Aeson
import Data.Attoparsec.Number as AesonNumber
import Data.HashMap.Strict as Map
import Data.Vector as V
import Data.Text.Encoding as Encoding
import Data.Text.Lazy.Encoding as LazyEncoding
import Control.Monad

roguestar_muconfig :: MuConfig
roguestar_muconfig = MuConfig {
    muEscapeFunc = htmlEscape,
    muTemplateFileDir = Just "static/",
    muTemplateFileExt = Just ".mustache" }

mkAesonContext :: (Monad m) => Aeson.Value -> MuContext m
mkAesonContext (Object obj) key = maybe MuNothing aesonToMu $ Map.lookup (Encoding.decodeUtf8 key) obj
mkAesonContext x            _   = aesonToMu x

aesonToMu :: (Monad m) => Aeson.Value -> MuType m
aesonToMu obj@(Object {}) = MuList [mkAesonContext obj]
aesonToMu (Array arr) = MuList $ Prelude.map mkAesonContext $ V.toList arr
aesonToMu (String txt) = MuVariable txt
aesonToMu (Number (I num)) = MuVariable num
aesonToMu (Number (D num)) = MuVariable num
aesonToMu (Bool bool) = MuBool bool
aesonToMu Null = MuNothing

renderPage :: FilePath -> Aeson.Value -> IO T.Text
renderPage filepath value = liftM LazyEncoding.decodeUtf8 $ hastacheFile roguestar_muconfig filepath (mkAesonContext value)
