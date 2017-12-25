module Language.SMEIL.Parser.Impl where

import           Control.Monad               (void)

import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Expr

import           Language.SMEIL.Parser.Lexer
import           Language.SMEIL.Parser.Monad
import qualified Language.SMEIL.Syntax       as S

-- Top level constructs

designFile :: Parser S.DesignFile
designFile = spaceConsumer >> S.DesignFile <$> some designUnit

designUnit :: Parser S.DesignUnit
designUnit = S.DesignUnit <$> many importStm <*> some unitElement

unitElement :: Parser S.UnitElement
unitElement = choice [ S.UnitProc <$> process
                     , S.UnitNet <$> network
                     ]

-- Network Structure

importStm :: Parser S.Import
importStm = reserved "import" >> S.Import <$> name <* semi

network :: Parser S.Network
network =
  reserved "network" >>
  S.Network <$> ident <*> parens (param `sepBy` comma) <*>
  braces (some networkDecl)

networkDecl :: Parser S.NetworkDecl
networkDecl = choice [ S.NetInst <$> instanceDecl
                     , S.NetDecl <$> declaration
                     ]

process :: Parser S.Process
process = do
  sync <- synchrony
  void $ reserved "proc"
  S.Process <$> ident <*> parens (param `sepBy` comma) <*> many declaration <*>
    braces (many statement) <*>
    pure sync
  where
    synchrony =
      reserved "sync" *> pure True <|> reserved "async" *> pure False <|>
      pure False

param :: Parser (S.Direction, S.Ident)
param = (,) <$> direction <*> ident

-- Definitions

instanceDecl :: Parser S.Instance
instanceDecl =
  reserved "instance" >>
  S.Instance <$> ((transformIdent <$> ident) <* reserved "of") <*> ident <*>
  parens (paramMap `sepBy` comma) <*
  semi
  where
    paramMap = (,) <$> optional (try (ident <* colon)) <*> expression

    transformIdent "_" = Nothing
    transformIdent i   = Just i

enum :: Parser S.Enumeration
enum = reserved "enum" >> S.Enumeration <$> ident <*> braces (some enumField) <* semi
  where
    enumField = (,) <$> ident <*> optional (symbol "=" *> expression)

busDecl :: Parser S.Bus
busDecl =
  reserved "bus" >>
  S.Bus <$> ident <*> braces (signalDecl `sepBy1` comma) <* semi
  where
    signalDecl =
      S.BusSignal <$> (ident <* colon) <*> typeName <*>
      optional (symbol "=" *> expression) <*>
      optional range

declaration :: Parser S.Declaration
declaration = choice [ S.VarDecl <$> varDecl
                     , S.ConstDecl <$> constDecl
                     , S.BusDecl <$> busDecl
                     , S.FuncDecl <$> function
                     ]

varDecl :: Parser S.Variable
varDecl =
  reserved "var" >>
  S.Variable <$> ident <*> typeName <*> optional (symbol "=" *> expression) <*>
  optional range <*
  semi

range :: Parser S.Range
range =
  reserved "range" >> S.Range <$> expression <* reserved "to" <*> expression

constDecl :: Parser S.Constant
constDecl =
  reserved "const" >>
  S.Constant <$> ident <*> typeName <*> (symbol "=" *> expression) <* semi

function :: Parser S.Function
function =
  reserved "func" >> S.Function <$> ident <*> many ident <*> many statement

---------------------------------------------------------------------------------
-- Statements
---------------------------------------------------------------------------------

statement :: Parser S.Statement
statement =
  ifStm <|> forStm <|> switchStm <|> barrierStm <|> breakStm <|> returnStm <|>
  assignStm
  where
    assignStm = S.Assign <$> (name <* equal) <*> expression <* semi
    ifStm =
      reserved "if" >>
      S.If <$> parens expression <*> braces (some statement) <*>
      many
        (reserved "elif" *>
         parens ((,) <$> expression <*> braces (many statement))) <*>
      optional (reserved "else" *> braces (many statement))
    forStm =
      reserved "for" >>
      S.For <$> (ident <* equal) <*> (expression <* reserved "to") <*>
      expression <*>
      braces (many statement)
    switchStm =
      reserved "switch" >>
      S.Switch <$> (expression <* reserved "where") <*> braces (many switchCase) <*>
      optional defaultCase
    switchCase =
      reserved "case" >> (,) <$> (expression <* colon) <*> some statement
    defaultCase = reserved "default" >> colon *> some statement
    barrierStm = reserved "barrier" >> semi >> pure S.Barrier
    breakStm = reserved "break" >> semi >> pure S.Break
    returnStm = reserved "return" >> S.Return <$> optional expression <* semi

name :: Parser S.Name
name = (ident >>= rest) <?> "name"
  where
    rest context =
      choice
        [ dot >> S.HierAccess <$> ((:) context <$> ident `sepBy1` dot) >>= rest2
        , rest2 $ S.Ident context
        , pure $ S.Ident context
        ]
    rest2 context =
      choice
        [S.ArrayAccess <$> pure context <*> brackets expression, pure context]

expression :: Parser S.Expr
expression = makeExprParser term table <?> "expression"

term :: Parser S.Expr
term =
  choice
    [parens expression, S.PrimLit <$> literal, S.PrimName <$> name, funCall] <?>
  "term"
  where
    funCall = S.FunCall <$> name <*> parens (expression `sepBy1` comma)

table :: [[Operator Parser S.Expr]]
table =
  [ [ prefix "+" (S.Unary S.UnPlus)
    , prefix "-" (S.Unary S.UnMinus)
    , prefix "!" (S.Unary S.NotOp)
    ]
  , [ binary "*" (S.Binary S.MulOp)
    , binary "/" (S.Binary S.DivOp)
    , binary "%" (S.Binary S.ModOp)
    ]
  , [binary "+" (S.Binary S.PlusOp), binary "-" (S.Binary S.MinusOp)]
  , [binary "<<" (S.Binary S.SllOp), binary ">>" (S.Binary S.SrlOp)]
  , [ binary "<" (S.Binary S.LtOp)
    , binary ">" (S.Binary S.GtOp)
    , binary "<=" (S.Binary S.GeqOp)
    , binary ">=" (S.Binary S.LeqOp)
    ]
  , [binary "&" (S.Binary S.AndOp)]
  , [binary "^" (S.Binary S.XorOp)]
  , [binary "|" (S.Binary S.OrOp)]
  ]

binary :: String -> (a -> a -> a) -> Operator Parser a
binary n f = InfixL (f <$ symbol n)

prefix :: String -> (a -> a) -> Operator Parser a
prefix n f = Prefix (f <$ symbol n)

typeName :: Parser S.Type
typeName =
  choice
    [ char 'i' >> S.Signed <$> integer
    , char 'u' >> S.Unsigned <$> integer
    , char 'f' >>
      ((string "32" >> pure S.Single) <|> (string "64" >> pure S.Double))
    , string "bool" >> pure S.Bool
    , S.Array <$> brackets (optional integer) <*> typeName
    ]

