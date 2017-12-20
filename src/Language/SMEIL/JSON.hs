-- | TH derived Data.Aeson instances for "Language.SMEIL.Syntax"

{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.SMEIL.JSON
  ( readJSON
  , genJSON
  ) where

import           Language.SMEIL.Syntax

import           Control.Monad         (mapM)
import           Data.Aeson            (FromJSON, ToJSON, eitherDecode, encode)
import           Data.Aeson.TH         (Options (..), SumEncoding (..),
                                        defaultOptions, deriveJSON, sumEncoding)
import           Data.ByteString.Lazy  (ByteString)

import           Data.Char             (isUpper, toLower)
import           Data.List             (intercalate)
import           Data.List.Split       (keepDelimsL, split, whenElt)

-- | Reads a bytestring of JSON and returns a SMEIL AST fragment
readJSON
  :: (FromJSON a)
  => ByteString -> Either String a
readJSON = eitherDecode

-- | Transforms a SMEIL AST to a JSON bytestringq
genJSON
  :: (ToJSON a)
  => a -> ByteString
genJSON = encode

concat <$>
  mapM
    (deriveJSON
       (let
           jsonName s =
             intercalate "-" $
             filter (not . null) $
             map (map toLower) ((split . keepDelimsL . whenElt) isUpper s)
        in
           defaultOptions
           { sumEncoding = ObjectWithSingleField
           , tagSingleConstructors = True
           , constructorTagModifier = jsonName
           , fieldLabelModifier = jsonName
           }))
    [ ''DesignFile
    , ''DesignUnit
    , ''Import
    , ''Instance
    , ''Network
    , ''NetworkDecl
    , ''Bus
    , ''BusSignal
    , ''Range
    , ''Process
    , ''Declaration
    , ''Variable
    , ''Constant
    , ''Function
    , ''Statement
    , ''Enumeration
    , ''Direction
    , ''Expr
    , ''BinOp
    , ''UnOp
    , ''Type
    , ''Literal
    ]
