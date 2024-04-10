{-# LANGUAGE
    TypeApplications
  , OverloadedStrings
  #-}
module Text.Luatemp where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as Char8

import Data.Attoparsec.ByteString.Lazy as P

import HsLua qualified as Lua

runLuaFile :: FilePath -> IO LBS.ByteString
runLuaFile fp = LBS.readFile fp >>= \code -> Lua.run @Lua.Exception $ do
  Lua.openlibs
  -- Lua.liftIO $ LBS.putStr code
  Lua.loadstring (LBS.toStrict code)
  Lua.call
    (Lua.NumArgs 0)
    (Lua.NumResults 1)
  lua_getLBS

runTemplateFile :: FilePath -> IO LBS.ByteString
runTemplateFile = 

data Chunk = RawText !LBS.ByteString | LuaBlock !LBS.ByteString | LuaExpr !BS.ByteString

splitChunksP :: Parser [Chunk]
splitChunksP = do
  initial <- P.takeWhile (!= '<')
  (RawText initial :) <$> P.choice
    [ string "<%=" >> go_expr
    , string "<%" >> go_block
    , splitChunksP
    ]
  where
    go_expr = LuaExpr <$> nom_lua
    go_block = LuaBlock <$> nom_lua
    go_lua con = do
      code <- nom_lua
      (con lua :) <$> splitChunksP
    nom_lua = P.scan False step <* string "%>"
    step True  c | c == ord '>' = Nothing
    step False c | c == ord '%' = Just True
    step s _ = Just s

runChunks :: [Chunk] -> IO BS.ByteString
runChunks text = do
  vm <- Lua.newGCManagedstate
  Lua.withGCManagedState vm $ do
    Lua.openlibs
    BS.concat <$> traverse (runChunk vm)

runChunk :: Lua.GCManagedState -> Chunk -> Lua.Lua BS.ByteString
runChunk vm chk = case chk of
  RawChunk raw -> pure raw
  LuaExecChunk block -> do
    Lua.OK <- Lua.loadstring block
    Lua.call
      (Lua.NumArgs 0)
      (Lua.NumResults 1)
    lua_getLBS
  LuaExprChunk expr -> do
    let block = BS.concat ["return (", expr, ")"]
    Lua.OK <- Lua.loadstring block
    Lua.call
      (Lua.NumArgs 0)
      (Lua.NumResults 1)
    lua_getLBS


-----------------
-- Lua helpers --
-----------------

lua_getLBS :: Lua LBS.ByteString
lua_getLBS = do
  result <- Lua.runPeeker Lua.peekLazyByteString 1
  Lua.pop 1
  Lua.force result