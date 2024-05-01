{-# LANGUAGE OverloadedStrings, DeriveFunctor #-}
module Text.Templating.Lua.Types where

import Data.ByteString (ByteString)
import Data.Vector (Vector)


data Template = MkTemplate
  { templateTransformedLua :: !ByteString
  , templateVerbatimChunks :: !(Vector (ByteString, ByteString))
  } deriving (Eq, Show)


data Token b
  = TText !b
  | TBeginLuaExpr
  | TBeginLua
  | TEndLua
  deriving (Eq, Show, Functor)
