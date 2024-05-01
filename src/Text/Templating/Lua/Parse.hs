{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Text.Templating.Lua.Parse where

import Data.Bifunctor (second)
import Data.Foldable
import Control.Applicative

import Data.ByteString (ByteString, toStrict)
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder (Builder, byteString, toLazyByteString)
import Data.ByteString.Char8 qualified as C8
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable

import Data.Attoparsec.ByteString as Parser

import Text.Templating.Lua.Types

catTexts :: [Token ByteString] -> [Token ByteString]
catTexts = fmap (fmap (toStrict . toLazyByteString)) . go . fmap (fmap byteString)
  where
    go :: [Token Builder] -> [Token Builder]
    go (TText x : TText y : rest) = go (TText (x <> y) : rest)
    go (tok:toks) = tok : go toks
    go [] = []

-- note: this parser always consumes input
ptoken :: Parser (Token ByteString)
ptoken = choice
  [ TText <$> Parser.takeWhile1 (not . isDelimiter)
  , TBeginLuaExpr <$ string "<%="
  , TBeginLua <$ string "<%"
  , TEndLua <$ string "%>"
  , TText <$> choice [string "<", string "%"]
  ]
  where
    isDelimiter 60 = True  -- '<'
    isDelimiter 37 = True  -- '%'
    isDelimiter _  = False

data PState = PState
  { luaBits :: !Builder
  , nextChunkId :: !Int
  , curChunkName :: !ByteString
  , mode :: !PMode
  , chunkMap :: !(HashMap ByteString Builder)
  }

data PMode = PText | PLuaBlock | PLuaExpr

nomToken :: Token ByteString -> PState -> PState
nomToken (TText txt) st = case mode st of
  PText     -> let
                 chunkId = nextChunkId st
                 h = hashWithSalt chunkId txt
                 chunkName = mkChunkName h chunkId
                 code = byteString chunkName <> "() "
                 builder = byteString txt
               in st { luaBits = luaBits st <> code, nextChunkId = chunkId + 1, chunkMap = HashMap.insert chunkName builder (chunkMap st) }
  PLuaBlock -> st { luaBits = luaBits st <> " " <> byteString txt }
  PLuaExpr  -> st { luaBits = luaBits st <> "emit(" <> byteString txt <> ") " }
nomToken TBeginLuaExpr st = case mode st of
  PText     -> st { mode = PLuaExpr }
  _         -> st { luaBits = luaBits st <> "<%=" }
nomToken TBeginLua st = case mode st of
  PText     -> st { mode = PLuaBlock }
  _         -> st { luaBits = luaBits st <> "%<" }
nomToken TEndLua st = case mode st of
  PText     -> let
                 nm = curChunkName st
                 chunkMap' = HashMap.insertWith (flip (<>)) nm "%>" (chunkMap st)
               in st { chunkMap = chunkMap' }
  PLuaBlock -> st { mode = PText }
  PLuaExpr  -> st { mode = PText }

initPState :: PState
initPState = PState
  { luaBits = mempty
  , nextChunkId = 0
  , curChunkName = "emit_BEGIN_0"  -- only gets used in files that begin with %>
  , mode = PText
  , chunkMap = HashMap.empty
  }

closePState :: MonadFail m => PState -> m Template
closePState st = do
  case mode st of
    PText -> pure ()
    PLuaBlock -> fail "unclosed <%="
    PLuaExpr -> fail "unclosed <%"
  let !code = toStrict $ toLazyByteString (luaBits st)
      !verbatims = second (toStrict . toLazyByteString) <$> mkVerbatims (chunkMap st)
  pure (MkTemplate code verbatims)

-- missing-index errors somewhere in this function indicate a programmer error,
-- so reporting them with a crash (which will be thrown by the vector functions,
-- or when accessing the resulting bottom) is perfectly adequate.
mkVerbatims :: HashMap ByteString Builder -> Vector (ByteString, Builder)
mkVerbatims entries = Vector.fromListN (HashMap.size entries) $ HashMap.toList entries

templateP :: Parser Template
templateP = closePState . foldl' (flip nomToken) initPState . catTexts =<< many ptoken

parseTemplate :: ByteString -> Either String Template
parseTemplate bs = parseOnly templateP bs

loadTemplateFromFile :: FilePath -> IO (Either String Template)
loadTemplateFromFile path = parseOnly templateP <$> ByteString.readFile path


mkChunkName :: Int -> Int -> ByteString
mkChunkName h i = "emit_TEMPLATE_CHUNK_" <> ps h <> "_" <> ps i
  where ps = C8.pack . show . abs