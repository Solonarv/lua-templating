module Main where

import System.IO
import System.Exit

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Options.Applicative

import Text.Templating.Lua
import Text.Templating.Lua.Parse
import Text.Templating.Lua.Run
import Text.Templating.Lua.Types


data File = FileAtPath FilePath | StdIoFile
  deriving (Eq, Show)

withH :: File -> IOMode -> (Handle -> IO a) -> IO a
withH (FileAtPath fp) = withFile fp
withH StdIoFile = \case
  ReadMode -> ($ stdin)
  WriteMode -> ($ stdout)
  AppendMode -> ($ stdout)
  ReadWriteMode -> error "withH: no stdio handle is both readable and writable"

showfile :: File -> String
showfile (FileAtPath fp) = fp
showfile StdIoFile = "<stdin>"

data Opts = Opts
  { o_inputFile :: File
  , o_outputFile :: File
  , o_shouldAppend :: Bool
  } deriving (Eq, Show)

fileRead :: ReadM File
fileRead = maybeReader \case
  "-" -> Just StdIoFile
  fp -> Just (FileAtPath fp)

optsP :: Parser Opts
optsP = Opts
  <$> argument fileRead
    (  value StdIoFile
    <> showDefaultWith (\_ -> "stdin")
    <> metavar "TEMPLATE"
    <> help "Render the code in TEMPLATE, - means stdin"
    )
  <*> option fileRead
    (  long "output"
    <> short 'o'
    <> value StdIoFile
    <> showDefaultWith (\_ -> "stdout")
    <> metavar "FILE"
    <> help "Write output to FILE, - means stdout"
    )
  <*> switch
    (  long "append"
    <> short 'a'
    <> showDefault
    <> help "Append to the output file instead of overwriting it"
    )

opts :: ParserInfo Opts
opts = info (optsP <**> helper)
  ( fullDesc
  <> progDesc "Render the given lua template"
  <> header "render-lua-template - runner for lua-templating"
  )

main :: IO ()
main = do
  Opts inf outf shouldAppend<- execParser opts
  indata <- withH inf ReadMode BS.hGetContents
  case parseTemplate indata of
    Left err -> die $ "Error parsing template at " <> showfile inf <> ":\n" <> err
    Right template -> do
      result <- runTemplate template >>= maybePrintErr (templateTransformedLua template)
      let outmode = if shouldAppend then AppendMode else WriteMode
      withH outf outmode \h -> LBS.hPut h result