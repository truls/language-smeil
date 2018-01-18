{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.SMEIL.Pretty
  ( pprr
  ) where

import           Language.SMEIL.Syntax

import           Text.PrettyPrint.Mainland
import           Text.PrettyPrint.Mainland.Class

instance Pretty (DesignFile a) where
  ppr (DesignFile du _) = stack $ map ppr du

instance Pretty (DesignUnit a) where
  ppr (DesignUnit i es _) = stack (map ppr i) </> stack (map ppr es)

instance Pretty (UnitElement a) where
  ppr (UnitProc p _) = ppr p
  ppr (UnitNet n _)  = ppr n

instance Pretty (Import a) where
  ppr (Import s _) = text "import" <+> ppr s <> semi

instance Pretty (Network a) where
  ppr (Network i ps is _) =
      text "network" <+>
       ppr i <+>
       parens (commasep $ map param ps) </>
       hang' (lbrace </> stack (map ppr is)) </>
    rbrace
    where
      param (d, e) = ppr d <+> ppr e

instance Pretty (NetworkDecl a) where
  ppr (NetInst i _)  = ppr i
  ppr (NetBus b _)   = ppr b
  ppr (NetConst c _) = ppr c

instance Pretty (Bus a) where
  ppr (Bus e n ss _) =
    ppIf e (text "exposed") <+>
    text "bus" <+>
    ppr n <+> braces (stack $ map (\s -> ppr s <> semi) ss) <> semi

instance Pretty (BusSignal a) where
  ppr (BusSignal n t v r _) =
    ppr n <> colon <+> ppr t <+> ppr (catL (text "= ") v) <+> ppr r

instance Pretty (Range a) where
  ppr (Range u l _) = text "range" <+> ppr u <+> text "to" <+> ppr l

instance Pretty (Process a) where
  ppr (Process n ps ds bs c _) =
    hang'
      (ppIf c (text "sync") <+>
       text "proc" <+>
       ppr n <+> parens (commasep (map param ps)) </> stack (map ppr ds)) </>
    hang' (lbrace </> stack (map ppr bs)) </>
    rbrace <>
    line
    where
      param (d, i) = ppr d <+> ppr i

instance Pretty (Declaration a) where
  ppr (VarDecl v)   = ppr v
  ppr (ConstDecl c) = ppr c
  ppr (BusDecl b)   = ppr b
  ppr (FuncDecl f)  = ppr f

instance Pretty (Variable a) where
  ppr (Variable n t v r _) =
    text "var" <+>
    ppr n <> colon <+>
    ppr t <> ppr (catL (space <> equals <> space) v) <> ppr (catL space r) <>
    semi

instance Pretty (Constant a) where
  ppr (Constant n t v _) =
    text "const" <+> ppr n <> colon <+> ppr t <+> equals <+> ppr v <> semi

instance Pretty (Function a) where
  ppr (Function n ps bs _) =
    hang'
      (text "func" <+>
       ppr n <> parens (commasep $ map ppr ps) <+> lbrace </> stack (map ppr bs)) </>
    rbrace

instance Pretty (Statement a) where
  ppr (Assign i v _) = ppr i <+> text "=" <+> ppr v <> semi
  ppr (If c bs ei e _) =
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
  ppr (For v f t bs _) =
    hang'
      (ppr "for" <+>
       ppr v <+>
       text "=" <+>
       ppr f <+> text "to" <+> ppr t <+> lbrace </> stack (map ppr bs)) </>
    rbrace
  ppr (Switch v cs ds _) =
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
  ppr (Barrier _) = text "barrier" <> semi
  ppr (Break _) = text "break" <> semi
  ppr (Return v _) = text "return" <+> ppr v <> semi

instance Pretty (Enumeration a) where
  ppr (Enumeration n fs _) =
    hang' (text "enum" <+> ppr n <+> lbrace </> commasep (map field fs)) </> rbrace
    where
      field :: (Ident, Maybe (Expr a)) -> Doc
      field (i, e) = ppr i <+> ppr (catL (text "=") e)

instance Pretty (Direction a) where
  ppr (In _)    = text "in"
  ppr (Out _)   = text "out"
  ppr (Const _) = text "const"

instance Pretty (Expr a) where
  ppr (Binary op e1 e2 _) = ppr e1 <+> ppr op <+> ppr e2
  ppr (Unary op e1 _)     = ppr op <> ppr e1
  ppr (PrimLit l _)       = ppr l
  ppr (PrimName n _)      = ppr n
  ppr (FunCall n ps _)    = ppr n <> parens (commasep (map ppr ps))

instance Pretty (Instance a) where
  ppr (Instance i e ps _) =
    text "instance" <+>
    ppr (toInstName i) <+>
    text "of" <+> ppr e <+> parens (commasep $ map param ps) <> semi
    where
      param (Nothing, ee) = ppr ee
      param (Just n, ee)  = ppr n <> colon <+> ppr ee
      toInstName (Just a) = a
      toInstName Nothing  = "_"

instance Pretty (BinOp a) where
  ppr (PlusOp _)  = text "+"
  ppr (MinusOp _) = text "-"
  ppr (MulOp _)   = text "*"
  ppr (DivOp _)   = text "/"
  ppr (ModOp _)   = text "%"
  ppr (EqOp _)    = text "=="
  ppr (NeqOp _)   = text "!="
  ppr (SllOp _)   = text "<<"
  ppr (SrlOp _)   = text ">>"
  ppr (LtOp _)    = text "<"
  ppr (GtOp _)    = text ">"
  ppr (LeqOp _)   = text "<="
  ppr (GeqOp _)   = text ">="
  ppr (AndOp _)   = text "&"
  ppr (OrOp _)    = text "|"
  ppr (XorOp _)   = text "^"

instance Pretty (UnOp a) where
  ppr (UnPlus _)  = text "+"
  ppr (UnMinus _) = text "-"
  ppr (NotOp _)   = text "!"

instance Pretty (Name a) where
  ppr (Ident i _)         = ppr i
  ppr (HierAccess is _)   = cat $ punctuate dot (map ppr is)
  ppr (ArrayAccess n e _) = ppr n <> brackets (ppr e)

instance Pretty (Type a) where
  ppr (Signed s _)   = text "i" <> ppr s
  ppr (Unsigned s _) = text "u" <> ppr s
  ppr (Single _)     = text "f32"
  ppr (Double _)     = text "f64"
  ppr (Bool _)       = text "bool"
  ppr (Array l t _)  = brackets (ppr l) <> ppr t

instance Pretty (Literal a) where
  ppr (LitInt i _)    = integer i
  ppr (LitFloat f _)  = double f
  ppr (LitString s _) = dquotes $ ppr s
  ppr (LitArray es _) = brackets (commasep (map ppr es))
  ppr (LitTrue _)     = text "true"
  ppr (LitFalse _)    = text "false"

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
