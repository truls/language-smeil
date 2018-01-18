module Language.SMEIL.Parser
  ( parse
  , SrcSpan
  ) where


import           Control.Monad.State.Lazy
import qualified Text.Megaparsec             as P

import           Language.SMEIL.Syntax

import           Language.SMEIL.Parser.Impl
import           Language.SMEIL.Parser.Monad

parse :: String -> String  -> Either String (DesignFile SrcSpan)
parse f c =
  case P.runParser (runStateT designFile (P.initialPos f)) f c of
    Left err     -> Left $ P.parseErrorPretty err
    Right (r, _) -> Right r
