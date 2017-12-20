module Language.SMEIL.Parser
  ( parse
  ) where


import           Data.ByteString.Lazy       (ByteString)

import           Language.SMEIL.Syntax

import           Language.SMEIL.Parser.Impl

parse :: ByteString -> Either String DesignFile
parse = undefined
