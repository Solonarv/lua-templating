{-# LANGUAGE TypeApplications #-}
module Text.Luatemp where

import Data.ByteString.Lazy qualified as LBS

import HsLua qualified as Lua

runLuaFile :: FilePath -> IO LBS.ByteString
runLuaFile fp = LBS.readFile fp >>= \code -> Lua.run @Lua.Exception $ do
  Lua.openlibs
  Lua.dostring (LBS.toStrict code)
  Lua.runPeeker Lua.peekLazyByteString 0 >>= Lua.force