{-# LANGUAGE OverloadedStrings #-}
module Text.Templating.Lua.Types where

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Char8 qualified as C8
import Data.Vector (Vector)


data Template = MkTemplate
  { templateTransformedLua :: !ByteString
  , templateVerbatimChunks :: !(Vector ByteString)
  } deriving (Eq, Show)


data Token
  = TText !Builder
  | TBeginLuaExpr
  | TBeginLua
  | TEndLua
  deriving Show

mkChunkName :: Int -> ByteString
mkChunkName i = "emit_TEMPLATE_CHUNK_" <> C8.pack (show i)