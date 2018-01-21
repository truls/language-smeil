module Language.SMEIL.Parser.Impl where

import           Control.Monad               (void)
import           Data.Maybe                  (isJust)

import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Expr
import           Text.Megaparsec.Perm

import           Language.SMEIL.Parser.Lexer
import           Language.SMEIL.Parser.Monad
import qualified Language.SMEIL.Syntax       as S

-- Top level constructs

designFile :: Parser (S.DesignFile SrcSpan)
designFile = withPos $ spaceConsumer >> S.DesignFile <$> some designUnit <* eof

designUnit :: Parser (S.DesignUnit SrcSpan)
designUnit = withPos $ S.DesignUnit <$> many importStm <*> some unitElement

unitElement :: Parser (S.UnitElement SrcSpan)
unitElement = choice [S.UnitProc <$> process, S.UnitNet <$> network]

-- Network Structure

importStm :: Parser (S.Import SrcSpan)
importStm = withPos $ reserved "import" >> S.Import <$> name <* semi

network :: Parser (S.Network SrcSpan)
network =
  withPos
    (reserved "network" >>
     S.Network <$> ident <*> parens (param `sepBy` comma) <*>
     braces (some networkDecl))

networkDecl :: Parser (S.NetworkDecl SrcSpan)
networkDecl = choice [S.NetInst <$> instanceDecl
                     , S.NetBus <$> busDecl
                     , S.NetConst <$> constDecl
                     , S.NetGen <$> genDecl
                     ]

process :: Parser (S.Process SrcSpan)
process =
  withPos $ do
    sync <- synchrony
    void $ reserved "proc"
    S.Process <$> ident <*> parens (param `sepBy` comma) <*> many declaration <*>
      braces (many statement) <*>
      pure sync
  where
    synchrony =
      reserved "sync" *> pure True <|> reserved "async" *> pure False <|>
      pure False

param :: Parser (S.Param SrcSpan)
param =
  withPos $
  S.Param <$> optional (brackets (optional expression)) <*> direction <*> ident

-- Definitions
instanceDecl :: Parser (S.Instance SrcSpan)
instanceDecl =
  withPos
    (reserved "instance" >>
     S.Instance <$> (transformIdent <$> ident) <*>
     (optional (brackets expression) <* reserved "of") <*>
     ident <*>
     parens (paramMap `sepBy` comma) <*
     semi)
  where
    paramMap = (,) <$> optional (try (ident <* colon)) <*> expression
    transformIdent "_" = Nothing
    transformIdent i   = Just i

enum :: Parser (S.Enumeration SrcSpan)
enum = withPos $
  reserved "enum" >>
  S.Enumeration <$> ident <*> braces (enumField `sepBy1` comma) <* semi
  where
    enumField = (,) <$> ident <*> optional (symbol "=" *> expression)

busDecl :: Parser (S.Bus SrcSpan)
busDecl =
  withPos
    (busProps <*> (reserved "bus" *> ident) <*>
     braces (some (withPos signalDecl)) <*
     semi <?> "bus declaration")
  where
    signalDecl =
      S.BusSignal <$> (ident <* colon) <*> typeName <*>
      optional (symbol "=" *> expression) <*>
      optional range <*
      semi <?> "bus signal declaration"
    busProps =
      makePermParser $
      S.Bus <$?> (False, reserved "exposed" *> pure True) <|?>
      (False, reserved "unique" *> pure True)

varDecl :: Parser (S.Variable SrcSpan)
varDecl =
  withPos
    (reserved "var" >>
     S.Variable <$> (ident <* colon) <*> typeName <*>
     optional (symbol "=" *> expression) <*>
     optional range <*
     semi <?> "variable declaration")

genDecl :: Parser (S.Generate SrcSpan)
genDecl =
  withPos
    (reserved "generate" >>
     S.Generate <$> (ident <* equal) <*> (expression <* reserved "to") <*>
     expression <*>
     braces (many networkDecl)) <?>
  "generate declaration"

range :: Parser (S.Range SrcSpan)
range =
  withPos
    (reserved "range" >>
     S.Range <$> expression <* reserved "to" <*>
     expression <?> "range constraint")

constDecl :: Parser (S.Constant SrcSpan)
constDecl =
  withPos
    (reserved "const" >>
     S.Constant <$> (ident <* colon) <*> typeName <*> (symbol "=" *> expression) <*
     semi <?> "constant declaration")

function :: Parser (S.Function SrcSpan)
function =
  withPos
    (reserved "func" >>
     S.Function <$> ident <*> many ident <*> many statement <?> "function")

declaration :: Parser (S.Declaration SrcSpan)
declaration =
  choice
    [ S.VarDecl <$> varDecl
    , S.ConstDecl <$> constDecl
    , S.BusDecl <$> busDecl
    , S.FuncDecl <$> function
    , S.EnumDecl <$> enum
    ] <?>
  "declaration"

---------------------------------------------------------------------------------
-- Statements
---------------------------------------------------------------------------------

statement :: Parser (S.Statement SrcSpan)
statement =
  withPos
    (ifStm <|> forStm <|> switchStm <|> barrierStm <|> breakStm <|> returnStm <|>
     assignStm <?> "statement")
  where
    assignStm = S.Assign <$> (name <* equal) <*> expression <* semi
    ifStm =
      reserved "if" >>
      S.If <$> parens expression <*> braces (some statement) <*>
      many
        (reserved "elif" *>
         ((,) <$> parens expression <*> braces (many statement))) <*>
      optional (reserved "else" *> braces (many statement)) <?> "if statement"
    forStm =
      reserved "for" >>
      S.For <$> (ident <* equal) <*> (expression <* reserved "to") <*>
      expression <*>
      braces (many statement) <?> "for statement"
    switchStm =
      reserved "switch" >>
      S.Switch <$> (expression <* reserved "where") <*> braces (many switchCase) <*>
      optional defaultCase <?> "switch statement"
      where
        switchCase =
          reserved "case" >>
          (,) <$> expression <*> braces (some statement) <?> "switch case"
        defaultCase = reserved "default" >> colon *> some statement
    barrierStm = reserved "barrier" >> semi >> pure S.Barrier
    breakStm = reserved "break" >> semi >> pure S.Break
    returnStm = reserved "return" >> S.Return <$> optional expression <* semi

name :: Parser (S.Name SrcSpan)
name = namePart >>= rest <?> "name"
  where
    rest context =
      choice
        [ dot >>
          makePos
            (S.HierAccess <$> ((:) <$> pure context <*> namePart `sepBy1` dot))
        , pure context
        ]
    namePart = do
      ident' <- withPos (S.Ident <$> ident)
      choice
        [makePos $ S.ArrayAccess ident' <$> brackets arrayIndex, pure ident']

arrayIndex :: Parser (S.ArrayIndex SrcSpan)
arrayIndex = (symbol "*" >> pure S.Wildcard) <|> S.Index <$> expression

-------------------------------------------------------------------------------
-- Expressions
-------------------------------------------------------------------------------

expression :: Parser (S.Expr SrcSpan)
expression = makeExprParser term table <?> "expression"

term :: Parser (S.Expr SrcSpan)
term =
  choice
    [ parens expression
    , withPos $ S.PrimLit <$> (literal <|> withPos arrayLit)
    , try (withPos funCall)
    , withPos $ S.PrimName <$> name
    ] <?>
  "term"
  where
    funCall = S.FunCall <$> name <*> parens (expression `sepBy1` comma)
    arrayLit = S.LitArray <$> brackets (expression `sepBy` comma)

table :: [[Operator Parser (S.Expr SrcSpan)]]
table =
  [ [ prefix "+" S.Unary S.UnPlus
    , prefix "-" S.Unary S.UnMinus
    , prefix "!" S.Unary S.NotOp
    ]
  , [ binary "*" S.Binary S.MulOp
    , binary "/" S.Binary S.DivOp
    , binary "%" S.Binary S.ModOp
    ]
  , [binary "+" S.Binary S.PlusOp, binary "-" S.Binary S.MinusOp]
  , [binary "<<" S.Binary S.SllOp, binary ">>" S.Binary S.SrlOp]
  , [ binary "<" S.Binary S.LtOp
    , binary ">" S.Binary S.GtOp
    , binary "<=" S.Binary S.GeqOp
    , binary ">=" S.Binary S.LeqOp
    , binary "==" S.Binary S.EqOp
    ]
  , [binary "&" S.Binary S.AndOp]
  , [binary "^" S.Binary S.XorOp]
  , [binary "|" S.Binary S.OrOp]
  ]

posSymbol :: String -> (SrcSpan -> a) -> Parser (a, SrcSpan)
posSymbol s f = do
  putPos
  _ <- symbol s
  pos <- makePos'
  return (f pos, pos)

binary :: String
       -> (a -> b -> b -> SrcSpan -> b)
       -> (SrcSpan -> a)
       -> Operator Parser b
binary n f g =
  InfixL
    (do (g', pos) <- posSymbol n g
        return (\s r -> f g' s r pos))

prefix :: String
       -> (a -> b -> SrcSpan -> b)
       -> (SrcSpan -> a)
       -> Operator Parser b
prefix n f g =
  Prefix
    (do (g', pos) <- posSymbol n g
        return (\s -> f g' s pos))

typeName :: Parser (S.Type SrcSpan)
typeName =
  withPos
    (choice
       [ char 'i' >> S.Signed <$> integer
       , char 'u' >> S.Unsigned <$> integer
       , char 'f' >>
         ((string "32" >> pure S.Single) <|> (string "64" >> pure S.Double))
       , string "bool" >> pure S.Bool
       , S.Array <$> brackets (optional expression) <*> typeName
       ])

-- Utility Functions

parses :: Parser a -> Parser Bool
parses p = isJust <$> optional p
