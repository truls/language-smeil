-- | Contains the parser monad definition and related operations

{-# LANGUAGE DeriveDataTypeable #-}

module Language.SMEIL.Parser.Monad
  ( Parser
  , SrcSpan(..)
  , toLineFile
  , withPos
  , putPos
  , makePos
  , makePos'
  ) where

import           Control.Monad.State.Lazy
import           Data.Data
import           Data.Void
import           Text.Megaparsec

type Parser = StateT SourcePos (Parsec Void String)

data SrcSpan = SrcSpan
  { start :: SourcePos
  , end   :: SourcePos
  } deriving (Eq, Show, Data, Typeable)

toLineFile :: SrcSpan -> (Int, String)
toLineFile SrcSpan {start = s} = (unPos $ sourceLine s, sourceName s)

withPos :: Parser (SrcSpan -> a) -> Parser a
withPos p = do
  pos <- getPosition
  res <- p
  retSpan pos res

putPos :: Parser ()
putPos = do
  pos <- getPosition
  put pos
  return ()

makePos :: Parser (SrcSpan -> a) -> Parser a
makePos p = do
  res <- p
  pos <- get
  retSpan pos res

makePos' :: Parser SrcSpan
makePos' = do
  pos <- get
  pos' <- getPosition
  return $ SrcSpan pos pos'

retSpan :: SourcePos -> (SrcSpan -> a) -> Parser a
retSpan pos res = do
  pos' <- getPosition
  let srcSpan = SrcSpan pos pos'
  return (res srcSpan)
