module Language.SMEIL.Parser.Monad where

import           Data.Void
import           Text.Megaparsec

type Parser = Parsec Void String
