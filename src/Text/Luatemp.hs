{-# LANGUAGE
    TypeApplications
  , OverloadedStrings
  #-}
module Text.Luatemp where

import Data.ByteString.Lazy qualified as LBS
import System.Exit (die)
import System.FilePath (takeExtension)

import HsLua qualified as Lua

import Text.Luatemp.Template
import Text.Luatemp.Template.Run

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
  | otherwise = loadTemplateFromFile path >>= either die pure >>= runTemplate

-----------------
-- Lua helpers --
-----------------

lua_getLBS :: Lua.Lua LBS.ByteString
lua_getLBS = do
  result <- Lua.runPeeker Lua.peekLazyByteString 1
  Lua.pop 1
  Lua.force result