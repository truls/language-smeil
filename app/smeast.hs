--{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Exception          (throwIO)
import           Control.Monad              (unless)
import           Data.ByteString.Lazy       as B (readFile)
import           Data.ByteString.Lazy.Char8 as C8 (pack, putStrLn, writeFile)
import           Data.Char                  (isLetter, toLower)
import           Data.Semigroup             ((<>))
import           Options.Applicative        hiding (value)
import qualified Options.Applicative        as O (value)
import           System.Directory           (doesFileExist)
import           Text.Show.Pretty           (ppShow)

import           Language.SMEIL.JSON
import           Language.SMEIL.Parser
import           Language.SMEIL.Pretty      (pprr)
import           Language.SMEIL.Syntax

data Format = JSON | Pretty | AST

instance Read Format where
  readsPrec _ input =
    let (tok, rest) = span isLetter input
    in case mapFormat tok of
         Just f  -> [(f, rest)]
         Nothing -> []
    where
      mapFormat =
        (`lookup` [("json", JSON), ("pretty", Pretty), ("ast", AST)]) .
        map toLower

instance Show Format where
  show JSON   = "json"
  show Pretty = "pretty"
  show AST    = "AST"

data Options = Options
  { inputFile    :: FilePath
  , outputFile   :: FilePath
  , inputFormat  :: Format
  , outputFormat :: Format
  }

optParser :: Parser Options
optParser =
  Options <$>
  strOption (long "input" <> metavar "IN" <> short 'i' <> help "Input file") <*>
  strOption
    (long "output" <> metavar "OUT" <> short 'o' <> O.value "-" <>
     help "Output file") <*>
  option
    auto
    (long "input-format" <> short 'f' <>
     help "Format of input file. ARG must be one of choices json pretty") <*>
  option
    auto
    (long "output-format" <> short 'g' <>
     help "Format of output file. ARG must be one of choices json pretty")

opts :: ParserInfo Options
opts =
  info
    (optParser <**> helper)
    (fullDesc <> progDesc "Converts between different representations of SMEIL" <>
     header "smeast - SMEIL representation converter")

raiseEither :: (Show a) => Either a b -> IO b
raiseEither (Right r) = pure r
raiseEither (Left l)  = throwIO $ userError (show l)

main :: IO ()
main = do
  o <- execParser opts
  let inf = inputFile o
  let ouf = outputFile o
  doesFileExist inf >>=
    flip unless (throwIO (userError $ "Input file not found " ++ inf))
  fc <- B.readFile inf
  ast <-
    case inputFormat o of
      JSON   -> raiseEither (readJSON fc :: Either String DesignFile)
      Pretty -> raiseEither $ parse fc
      AST    -> throwIO (userError "Cannot parse prettyAST")
  let out =
        case outputFormat o of
          JSON   -> genJSON ast
          Pretty -> C8.pack $ pprr ast
          AST    -> C8.pack $ ppShow ast
  if ouf == "-"
    then C8.putStrLn out
    else C8.writeFile ouf out
