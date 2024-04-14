{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Text.Luatemp.Template where

import Control.Applicative

import Data.ByteString (ByteString, toStrict)
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder (Builder, byteString, toLazyByteString)
import Data.ByteString.Char8 qualified as C8
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.Mutable qualified as MVector
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap

import Data.Attoparsec.ByteString as Parser

data Template = MkTemplate
  { templateTransformedLua :: !ByteString
  , templateVerbatimChunks :: !(Vector ByteString)
  } deriving (Eq, Show)


data Token
  = TText !ByteString
  | TBeginLuaExpr
  | TBeginLua
  | TEndLua

ptoken :: Parser Token
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
  , mode :: PMode
  , chunkMap :: IntMap ByteString  -- invariant: for each key k, 0 ≤ k < nextChunkId
  }

data PMode = PText | PLuaBlock | PLuaExpr

nomToken :: Token -> PState -> PState
nomToken (TText txt) st = case mode st of
  PText     -> let
                 chunkId = nextChunkId st
                 chunkName = mkChunkName chunkId
                 code = byteString chunkName <> "() "
               in st { luaBits = luaBits st <> code, nextChunkId = chunkId + 1, chunkMap = IntMap.insert chunkId txt (chunkMap st) }
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
                 chunkId = nextChunkId st - 1
                 chunkMap' = IntMap.insertWith (flip (<>)) chunkId "%>" (chunkMap st)
               in st { chunkMap = chunkMap' }
  PLuaBlock -> st { mode = PText }
  PLuaExpr  -> st { mode = PText }

mkChunkName :: Int -> ByteString
mkChunkName i = "emit_LUATEMP_CHUNK_" <> C8.pack (show i)

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
      !verbatims = mkVerbatims (nextChunkId st) (chunkMap st)
  pure (MkTemplate code verbatims)

-- out-of-bounds errors somewhere in this function indicate a programmer error,
-- so reporting them with a crash (which will be thrown by the vector functions,
-- or when accessing the resulting bottom) is perfectly adequate. no need to do
-- bounds checks here.
mkVerbatims :: Int -> IntMap ByteString -> Vector ByteString
mkVerbatims len entries = Vector.create do
  vec <- MVector.new len
  _ <- IntMap.traverseWithKey (MVector.write vec) entries
  pure vec

templateP :: Parser Template
templateP = go initPState
  where
    go !st = (endOfInput >> closePState st)
          <|> (ptoken >>= \tok -> go (nomToken tok st))

loadTemplateFromFile :: FilePath -> IO (Either String Template)
loadTemplateFromFile path = parseOnly templateP <$> ByteString.readFile path