module Language.SMEIL.Parser.Lexer
  ( lexeme
  , symbol
  , ident
  , reserved
  , literal
  , parens
  , braces
  , brackets
  , comma
  , colon
  , dot
  , semi
  , integer
  , stringLit
  , spaceConsumer
  , equal
  , direction
  ) where

import           Control.Applicative         (empty)
import           Control.Monad               (when)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer  as L

import           Language.SMEIL.Parser.Monad
import qualified Language.SMEIL.Syntax       as S

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment "//") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

parens, braces, brackets :: Parser a -> Parser a
parens    = between (symbol "(") (symbol ")")
braces    = between (symbol "{") (symbol "}")
brackets  = between (symbol "[") (symbol "]")

semi, comma, colon, dot, equal :: Parser String
semi  = symbol ";"
comma = symbol ","
colon = symbol ":"
dot   = symbol "."
equal   = symbol "="

reserved :: String -> Parser String
reserved w = do
  r <- lexeme (string w)
  when (r `notElem` reservedWords) $ fail (r ++ " is not a reserved word")
  return r

ident :: Parser String
ident = lexeme $ do
  i <- part <|> symbol "_"
  when (i `elem` reservedWords) $
    fail $ "Keyword " ++ i ++ " used as identifier"
  return i
  where
    part = (:) <$> letterChar <*> many (alphaNumChar <|> char '_')

integer :: Parser Integer
integer = lexeme $ L.signed spaceConsumer L.decimal

float :: Parser Double
float = lexeme $ L.signed spaceConsumer L.float

stringLit :: Parser String
stringLit = lexeme (char '\"' >> manyTill strChar (char '\"'))
  where
    strChar = notChar '\\' <|> (char '\\' >> char '"')

literal :: Parser S.Literal
literal =
  choice
    [ symbol "true" >> pure S.LitTrue
    , symbol "false" >> pure S.LitFalse
    , S.LitString <$> stringLit
    , try $ S.LitFloat <$> float
    , S.LitInt <$> integer
    ]

direction :: Parser S.Direction
direction =
  choice
    [ reserved "in"    >> pure S.In
    , reserved "out"   >> pure S.Out
    , reserved "const" >> pure S.Const
    ]

reservedWords :: [String]
reservedWords =
  [ "import"
  , "network"
  , "where"
  , "bus"
  , "range"
  , "clocked"
  , "simulation"
  , "var"
  , "const"
  , "func"
  , "if"
  , "elif"
  , "else"
  , "for"
  , "to"
  , "switch"
  , "instance"
  , "of"
  , "sync"
  , "async"
  , "proc"
  , "in"
  , "out"
  , "exposed"
  ]
