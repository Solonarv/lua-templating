{-# LANGUAGE OverloadedStrings, TypeApplications #-}
module Text.Templating.Lua.Run where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Builder (Builder, toLazyByteString, byteString)
import Data.Vector (Vector)
import Data.Vector qualified as Vector

import HsLua (Lua)
import HsLua qualified as Lua

import Text.Templating.Lua.Types

runTemplate_ :: Template -> Lua (Maybe String, LazyByteString)
runTemplate_ temp = do
  Lua.openlibs
  ref <- liftIO $ newIORef mempty
  setEmit ref
  setChunkGlobals ref (templateVerbatimChunks temp)
  status <- Lua.dostring (templateTransformedLua temp)
  let err = "Lua returned non-OK status " <> show status <$ guard (status /= Lua.OK)
  liftIO $ (err,) . toLazyByteString <$> readIORef ref

runTemplate :: Template -> IO (Maybe String, LazyByteString)
runTemplate temp = do
  Lua.run @Lua.Exception (runTemplate_ temp)

setChunkGlobals :: IORef Builder -> Vector ByteString -> Lua ()
setChunkGlobals ref chunks = flip Vector.imapM_ chunks \i chk -> do
  let name = Lua.Name (mkChunkName i)
      func :: Lua ()
      func = atomicModifyIORef_' ref (<> byteString chk)
  Lua.registerHaskellFunction name func

setEmit :: IORef Builder -> Lua ()
setEmit ref = Lua.registerHaskellFunction "emit" emitFunc
  where
    emitFunc :: ByteString -> Lua ()
    emitFunc val = atomicModifyIORef_' ref (<> byteString val)

-- | the same as 'Data.IORef.atomicModifyIORef'', but without
-- extracting a result and lifted to MonadIO
atomicModifyIORef_' :: MonadIO m => IORef a -> (a -> a) -> m ()
atomicModifyIORef_' ref f = liftIO $ atomicModifyIORef' ref \old -> (f old, ())