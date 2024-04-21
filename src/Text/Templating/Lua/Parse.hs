{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Text.Templating.Lua.Parse where

import Data.Foldable
import Control.Applicative

import Data.ByteString (ByteString, toStrict)
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder (Builder, byteString, toLazyByteString)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap

import Data.Attoparsec.ByteString as Parser

import Text.Templating.Lua.Types

catTexts :: [Token] -> [Token]
catTexts (TText x : TText y : rest) = catTexts (TText (x <> y) : rest)
catTexts (tok:toks) = tok : catTexts toks
catTexts [] = []

-- note: this parser always consumes input
ptoken :: Parser Token
ptoken = choice
  [ TText . byteString <$> Parser.takeWhile1 (not . isDelimiter)
  , TBeginLuaExpr <$ string "<%="
  , TBeginLua <$ string "<%"
  , TEndLua <$ string "%>"
  , TText . byteString <$> choice [string "<", string "%"]
  ]
  where
    isDelimiter 60 = True  -- '<'
    isDelimiter 37 = True  -- '%'
    isDelimiter _  = False

data PState = PState
  { luaBits :: !Builder
  , nextChunkId :: !Int
  , mode :: PMode
  , chunkMap :: IntMap Builder  -- invariant: for each key k, 0 ≤ k < nextChunkId
  }

data PMode = PText | PLuaBlock | PLuaExpr

nomToken :: Token -> PState -> PState
nomToken (TText txt) st = case mode st of
  PText     -> let
                 chunkId = nextChunkId st
                 chunkName = mkChunkName chunkId
                 code = byteString chunkName <> "() "
               in st { luaBits = luaBits st <> code, nextChunkId = chunkId + 1, chunkMap = IntMap.insert chunkId txt (chunkMap st) }
  PLuaBlock -> st { luaBits = luaBits st <> " " <> txt }
  PLuaExpr  -> st { luaBits = luaBits st <> "emit(" <> txt <> ") " }
nomToken TBeginLuaExpr st = case mode st of
  PText     -> st { mode = PLuaExpr }
  _         -> st { luaBits = luaBits st <> "<%=" }
nomToken TBeginLua st = case mode st of
  PText     -> st { mode = PLuaBlock }
  _         -> st { luaBits = luaBits st <> "%<" }
nomToken TEndLua st = case mode st of
  PText     -> let
                 chunkId = nextChunkId st - 1
                 chunkMap' = IntMap.insertWith (flip (<>)) chunkId "%>" (chunkMap st)
               in st { chunkMap = chunkMap' }
  PLuaBlock -> st { mode = PText }
  PLuaExpr  -> st { mode = PText }

initPState :: PState
initPState = PState
  { luaBits = mempty
  , nextChunkId = 0
  , mode = PText
  , chunkMap = IntMap.empty
  }

closePState :: MonadFail m => PState -> m Template
closePState st = do
  case mode st of
    PText -> pure ()
    PLuaBlock -> fail "unclosed <%="
    PLuaExpr -> fail "unclosed <%"
  let !code = toStrict $ toLazyByteString (luaBits st)
      !verbatims = toStrict . toLazyByteString <$> mkVerbatims (nextChunkId st) (chunkMap st)
  pure (MkTemplate code verbatims)

-- missing-index errors somewhere in this function indicate a programmer error,
-- so reporting them with a crash (which will be thrown by the vector functions,
-- or when accessing the resulting bottom) is perfectly adequate.
mkVerbatims :: Int -> IntMap Builder -> Vector Builder
mkVerbatims len entries = Vector.generate len (entries IntMap.!)

templateP :: Parser Template
templateP = closePState . foldl' (flip nomToken) initPState . catTexts =<< many ptoken

parseTemplate :: ByteString -> Either String Template
parseTemplate bs = parseOnly templateP bs

loadTemplateFromFile :: FilePath -> IO (Either String Template)
loadTemplateFromFile path = parseOnly templateP <$> ByteString.readFile path