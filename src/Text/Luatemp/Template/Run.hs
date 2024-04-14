{-# LANGUAGE TypeApplications #-}
module Text.Luatemp.Template.Run where

import Control.Monad.IO.Class
import Data.IORef

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Builder (Builder, toLazyByteString, byteString)
import Data.Vector (Vector)
import Data.Vector qualified as Vector

import HsLua (Lua)
import HsLua qualified as Lua

import Text.Luatemp.Template

runTemplate_ :: Template -> Lua LazyByteString
runTemplate_ temp = do
  ref <- liftIO $ newIORef mempty
  setEmit ref
  setChunkGlobals ref (templateVerbatimChunks temp)
  Lua.OK <- Lua.dostring (templateTransformedLua temp)
  liftIO $ toLazyByteString <$> readIORef ref

runTemplate :: Template -> IO LazyByteString
runTemplate temp = Lua.run @Lua.Exception (runTemplate_ temp)

setChunkGlobals :: IORef Builder -> Vector ByteString -> Lua ()
setChunkGlobals ref chunks = flip Vector.imapM_ chunks \i chk -> do
  let name = Lua.Name (mkChunkName i)
      func = do liftIO (modifyIORef' ref (<> byteString chk)); pure 0
  Lua.pushHaskellFunction func
  Lua.setglobal name

setEmit :: IORef Builder -> Lua ()
setEmit ref = Lua.pushHaskellFunction emitFunc
  where
    emitFunc = do
      top <- Lua.gettop
      val <- Lua.tostring' top
      liftIO $ modifyIORef' ref (<> byteString val)
      Lua.pop 1
      pure 0
