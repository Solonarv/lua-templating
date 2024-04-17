{-# LANGUAGE OverloadedStrings, TypeApplications #-}
module Text.Templating.Lua.Run where

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent.MVar

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (LazyByteString, toStrict)
import Data.ByteString.Builder (Builder, toLazyByteString, byteString)
import Data.Vector (Vector)
import Data.Vector qualified as Vector

import HsLua (Lua)
import HsLua qualified as Lua

import Text.Templating.Lua.Types

runTemplate_ :: Template -> Lua (Maybe String, LazyByteString)
runTemplate_ temp = do
  Lua.openlibs
  ref <- liftIO $ newMVar mempty
  setEmit ref
  setChunkGlobals ref (templateVerbatimChunks temp)
  setIntrospect ref
  status <- Lua.dostring (templateTransformedLua temp)
  let err = "Lua returned non-OK status " <> show status <$ guard (status /= Lua.OK)
  liftIO $ (err,) . toLazyByteString <$> takeMVar ref

runTemplate :: Template -> IO (Maybe String, LazyByteString)
runTemplate temp = do
  Lua.run @Lua.Exception (runTemplate_ temp)

setChunkGlobals :: MVar Builder -> Vector ByteString -> Lua ()
setChunkGlobals ref chunks = flip Vector.imapM_ chunks \i chk -> do
  let name = Lua.Name (mkChunkName i)
      func :: Lua ()
      func = tryModifyMVar__ ref (<> byteString chk) >>= throwLeft
  Lua.registerHaskellFunction name func

setEmit :: MVar Builder -> Lua ()
setEmit ref = Lua.registerHaskellFunction "emit" emitFunc
  where
    emitFunc :: ByteString -> Lua ()
    emitFunc val = tryModifyMVar__ ref (<> byteString val) >>= throwLeft

setIntrospect :: MVar Builder -> Lua ()
setIntrospect ref = do
  Lua.pushHaskellFunction func
  Lua.setglobal "with_buffer"
  where
    func = do
      -- the function we want to call is `with_buffer`'s argument, so we just leave it on the stack
      liftIO $ putStrLn "introspecting!!"
      old <- liftIO $ readMVar ref
      Lua.pushstring (toStrict $ toLazyByteString old)
      Lua.call 1 1
      result <- Lua.force =<< Lua.runPeeker (Lua.peekNilOr Lua.peekByteString) (Lua.nth 1)
      case result of
        Nothing -> pure ()
        Just new -> tryModifyMVar__ ref (const $ byteString new) >>= throwLeft
      pure (Lua.NumResults 0)

-- dont need this anymore, we're on the MVar juice
-- | the same as 'Data.IORef.atomicModifyIORef'', but without
-- extracting a result and lifted to MonadIO
-- atomicModifyIORef_' :: MonadIO m => IORef a -> (a -> a) -> m ()
-- atomicModifyIORef_' ref f = liftIO $ atomicModifyIORef' ref \old -> (f old, ())

-- | Modify (take-then-put) an MVar, using the given pure function.
-- 
-- returns: error message or Right ()
tryModifyMVar__ :: MonadIO m => MVar a -> (a -> a) -> m (Either String ())
tryModifyMVar__ ref f = liftIO do
  old <- tryTakeMVar ref
  case old of
    Nothing -> pure (Left "Attempted to modify MVar while it's already being modified!")
    Just old' -> Right () <$ putMVar ref (f old')

throwLeft :: MonadFail m => Either String a -> m a
throwLeft = either fail pure