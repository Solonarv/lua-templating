module Text.Templating.Lua.Types where

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
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