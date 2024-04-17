{-# LANGUAGE
    TypeApplications
  , OverloadedStrings
  #-}
module Text.Templating.Lua where

import Data.Foldable
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import System.Exit (die)
import System.IO (hPutStr, stderr)
import System.FilePath (takeExtension)

import HsLua qualified as Lua

import Text.Templating.Lua.Parse
import Text.Templating.Lua.Run
import Text.Templating.Lua.Types

runLuaFile :: FilePath -> IO LBS.ByteString
runLuaFile fp = LBS.readFile fp >>= \code -> Lua.run @Lua.Exception $ do
  Lua.openlibs
  -- Lua.liftIO $ LBS.putStr code
  Lua.OK <- Lua.loadstring (LBS.toStrict code)
  Lua.call
    (Lua.NumArgs 0)
    (Lua.NumResults 1)
  lua_getLBS

runTemplateFile :: FilePath -> IO LBS.ByteString
runTemplateFile path
  | takeExtension path == ".lua" = runLuaFile path
  | otherwise = do
      template <- loadTemplateFromFile path >>= either die pure
      runTemplate template >>= maybePrintErr (templateTransformedLua template)

maybePrintErr :: ByteString -> (Maybe String, a) -> IO a
maybePrintErr code (merr, x) = x <$
  for_ merr \err -> do
    hPutStr stderr (err <> "\n\nCode was:\n\n")
    BS.hPutStr stderr code
    hPutStr stderr "\n\n"

-----------------
-- Lua helpers --
-----------------

lua_getLBS :: Lua.Lua LBS.ByteString
lua_getLBS = do
  result <- Lua.runPeeker Lua.peekLazyByteString 1
  Lua.pop 1
  Lua.force result