module Language.SMEIL.Parser
  ( parse
  ) where


import qualified Text.Megaparsec            as P

import           Language.SMEIL.Syntax

import           Language.SMEIL.Parser.Impl

parse :: String -> String  -> Either String DesignFile
parse f c  = case P.parse designFile f c of
               Left err -> Left $ P.parseErrorPretty err
               Right r  -> Right r
