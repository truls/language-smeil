{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.SMEIL.Pretty
  ( pprr
  ) where

import           Language.SMEIL.Syntax

import           Text.PrettyPrint.Mainland
import           Text.PrettyPrint.Mainland.Class

instance Pretty DesignFile where
  ppr (DesignFile du) = stack $ map ppr du

instance Pretty DesignUnit where
  ppr (DesignUnit i es) = stack (map ppr i) </> stack (map ppr es)

instance Pretty UnitElement where
  ppr (UnitProc p) = ppr p
  ppr (UnitNet n)  = ppr n

instance Pretty Import where
  ppr (Import s) = text "import" <+> ppr s <> semi

instance Pretty Network where
  ppr (Network i ps is) =
      text "network" <+>
       ppr i <+>
       parens (commasep $ map param ps) </>
       hang' (lbrace </> stack (map ppr is)) </>
    rbrace
    where
      param (d, e) = ppr d <+> ppr e

instance Pretty NetworkDecl where
  ppr (NetInst i)  = ppr i
  ppr (NetBus b)   = ppr b
  ppr (NetConst c) = ppr c

instance Pretty Bus where
  ppr (Bus e n ss) =
    ppIf e (text "exposed") <+>
    text "bus" <+>
    ppr n <+> braces (stack $ map (\s -> ppr s <> semi) ss) <> semi

instance Pretty BusSignal where
  ppr (BusSignal n t v r) =
    ppr n <> colon <+> ppr t <+> ppr (catL (text "= ") v) <+> ppr r

instance Pretty Range where
  ppr (Range u l) = text "range" <+> ppr u <+> text "to" <+> ppr l

instance Pretty Process where
  ppr (Process n ps ds bs c) =
    hang'
      (ppIf c (text "sync") <+>
       text "proc" <+>
       ppr n <+> parens (commasep (map param ps)) </> stack (map ppr ds)) </>
    hang' (lbrace </> stack (map ppr bs)) </>
    rbrace <>
    line
    where
      param (d, i) = ppr d <+> ppr i

instance Pretty Declaration where
  ppr (VarDecl v)   = ppr v
  ppr (ConstDecl c) = ppr c
  ppr (BusDecl b)   = ppr b
  ppr (FuncDecl f)  = ppr f

instance Pretty Variable where
  ppr (Variable n t v r) =
    text "var" <+>
    ppr n <> colon <+>
    ppr t <> ppr (catL (space <> equals <> space) v) <> ppr (catL space r) <>
    semi

instance Pretty Constant where
  ppr (Constant n t v) =
    text "const" <+> ppr n <> colon <+> ppr t <+> equals <+> ppr v <> semi

instance Pretty Function where
  ppr (Function n ps bs) =
    hang'
      (text "func" <+>
       ppr n <> parens (commasep $ map ppr ps) <+> lbrace </> stack (map ppr bs)) </>
    rbrace

instance Pretty Statement where
  ppr (Assign i v) = ppr i <+> text "=" <+> ppr v <> semi
  ppr (If c bs ei e) =
    hang' (text "if" <+> parens (ppr c) <+> lbrace </> stack (map ppr bs)) </>
    rbrace <>
    stack (map (ppr . elifBlock) ei) <>
    ppr (elblock <$> e)
    where
      elifBlock (ee, ss) =
        hang'
          (space <> text "elif" <+>
           parens (ppr ee) <+> lbrace </> stack (map ppr ss)) </>
        rbrace
      elblock [] = empty
      elblock ss =
        hang' (space <> text "else" <+> lbrace </> stack (map ppr ss)) </>
        rbrace
  ppr (For v f t bs) =
    hang'
      (ppr "for" <+>
       ppr v <+>
       text "=" <+>
       ppr f <+> text "to" <+> ppr t <+> lbrace </> stack (map ppr bs)) </>
    rbrace
  ppr (Switch v cs ds) =
    hang'
      (ppr "switch" <+>
       parens (ppr v) <+>
       lbrace </> (stack (map scase cs) </> ppr (dcase <$> ds))) </>
    rbrace
    where
      scase (e, ss) =
        hang' (text "case" <+> ppr e <+> lbrace </> stack (map ppr ss)) </>
        rbrace
      dcase ss =
        hang' (text "default" </> lbrace </> stack (map ppr ss)) </> rbrace
  ppr Barrier = text "barrier" <> semi
  ppr Break = text "break" <> semi
  ppr (Return v) = text "return" <+> ppr v <> semi

instance Pretty Enumeration where
  ppr (Enumeration n fs) =
    hang' (text "enum" <+> ppr n <+> lbrace </> commasep (map field fs)) </> rbrace
    where
      field :: (Ident, Maybe Expr) -> Doc
      field (i, e) = ppr i <+> ppr (catL (text "=") e)

instance Pretty Direction where
  ppr In    = text "in"
  ppr Out   = text "out"
  ppr Const = text "const"

instance Pretty Expr where
  ppr (Binary op e1 e2) = ppr e1 <+> ppr op <+> ppr e2
  ppr (Unary op e1)     = ppr op <> ppr e1
  ppr (PrimLit l)       = ppr l
  ppr (PrimName n)      = ppr n
  ppr (FunCall n ps)    = ppr n <> parens (commasep (map ppr ps))

instance Pretty Instance where
  ppr (Instance i e ps) =
    text "instance" <+>
    ppr (toInstName i) <+>
    text "of" <+> ppr e <+> parens (commasep $ map param ps) <> semi
    where
      param (Nothing, ee) = ppr ee
      param (Just n, ee)  = ppr n <> colon <+> ppr ee
      toInstName (Just a) = a
      toInstName Nothing  = "_"

instance Pretty BinOp where
  ppr PlusOp  = text "+"
  ppr MinusOp = text "-"
  ppr MulOp   = text "*"
  ppr DivOp   = text "/"
  ppr ModOp   = text "%"
  ppr EqOp    = text "=="
  ppr NeqOp   = text "!="
  ppr SllOp   = text "<<"
  ppr SrlOp   = text ">>"
  ppr LtOp    = text "<"
  ppr GtOp    = text ">"
  ppr LeqOp   = text "<="
  ppr GeqOp   = text ">="
  ppr AndOp   = text "&"
  ppr OrOp    = text "|"
  ppr XorOp   = text "^"

instance Pretty UnOp where
  ppr UnPlus  = text "+"
  ppr UnMinus = text "-"
  ppr NotOp   = text "!"

instance Pretty Name where
  ppr (Ident i)         = ppr i
  ppr (HierAccess is)   = cat $ punctuate dot (map ppr is)
  ppr (ArrayAccess n e) = ppr n <> brackets (ppr e)

instance Pretty Type where
  ppr (Signed s)   = text "i" <> ppr s
  ppr (Unsigned s) = text "u" <> ppr s
  ppr Single       = text "f32"
  ppr Double       = text "f64"
  ppr Bool         = text "bool"
  ppr (Array l t)  = brackets (ppr l) <> ppr t

instance Pretty Literal where
  ppr (LitInt i)    = integer i
  ppr (LitFloat f)  = double f
  ppr (LitString s) = dquotes $ ppr s
  ppr (LitArray es) = brackets (commasep (map ppr es))
  ppr LitTrue       = text "true"
  ppr LitFalse      = text "false"

nestL :: Int
nestL = 4

indent' :: Doc -> Doc
indent' = nest nestL

hang' :: Doc -> Doc
hang' = hang nestL

ppIf :: Bool -> Doc -> Doc
ppIf True d  = d
ppIf False _ = empty

pprr
  :: (Pretty a)
  => a -> String
pprr = pretty 80 . ppr

catL :: (Pretty a, Functor f) => Doc -> f a -> f Doc
catL d e = catL' d <$> e

catL' :: (Pretty a) => Doc -> a -> Doc
catL' d e = d <> ppr e
