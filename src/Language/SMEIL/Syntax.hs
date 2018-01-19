-- | This module defines the syntax for the SME intermediate
-- representation. For details, see: TODO/langspec.pdf

{-# LANGUAGE DuplicateRecordFields #-}

module Language.SMEIL.Syntax where

data DesignFile a = DesignFile
  { units :: [DesignUnit a]
  , annot :: a
  } deriving (Eq, Show)

data DesignUnit a = DesignUnit
  { imports     :: [Import a] -- ^ Imports of the design unit
  , unitElement :: [UnitElement a] -- ^ A unit-level process or network
  , annot       :: a
  } deriving (Eq, Show)

data UnitElement a
  = UnitProc { process :: Process a }
  | UnitNet { network :: Network a }
  deriving (Eq, Show)

-- | Specifies a module to be imported in current design module
data Import a = Import
  { ident :: Name a -- ^ Name of the module to be imported
  , annot :: a
  } deriving (Eq, Show)

-- | Instantiates either a "Process" or a "Network"
data Instance a = Instance
  { instName :: Maybe Ident -- ^ The name of the instance
  , elName   :: Ident -- ^ The name of the object to initialize
  , params   :: [(Maybe Ident, Expr a)] -- ^ Optionally named parameters of the object
  , annot    :: a
  } deriving (Eq, Show)

-- | Defines a Network
data Network a = Network
  { name     :: Ident -- ^ Name of network
  , params   :: [(Direction a, Ident)]
  , netDecls :: [NetworkDecl a] -- ^ Declarations in network
  , annot    :: a
  } deriving (Eq, Show)

data NetworkDecl a
  = NetInst { inst :: Instance a -- ^ A network instance
            }
  | NetBus { bus :: Bus a -- ^ A network declaration
           }
  | NetConst { const :: Constant a -- ^ A network constant
             }
  deriving (Eq, Show)

data Bus a = Bus
  { exposed :: Bool -- ^Bus is exposed on top level
  , name    :: Ident -- ^Name of bus
  , signals :: [BusSignal a] -- ^Bus signals
  , annot   :: a
  } deriving (Eq, Show)

data BusSignal a = BusSignal
  { name  :: Ident -- ^Name of signal
  , ty    :: Type a -- ^Type of signal
  , value :: Maybe (Expr a) -- ^Initial value of signal
  , range :: Maybe (Range a) -- ^Signal range
  , annot :: a
  } deriving (Eq, Show)

data Range a = Range
  { lower :: Expr a -- ^Lower bound
  , upper :: Expr a-- ^Upper bound
  , annot :: a
  } deriving (Eq, Show)

data Process a = Process
  { name   :: Ident -- ^Name of process
  , params :: [(Direction a, Ident)] -- ^Process parameters
  , decls  :: [Declaration a] -- ^
  , body   :: [Statement a]
  , sync   :: Bool
  , annot  :: a
  } deriving (Eq, Show)

data Declaration a
  = VarDecl (Variable a)
  | ConstDecl (Constant a)
  | BusDecl (Bus a)
  | FuncDecl (Function a)
  deriving (Eq, Show)

data Variable a = Variable
  { name  :: Ident
  , ty    :: Type a
  , val   :: Maybe (Expr a)
  , range :: Maybe (Range a)
  , annot :: a
  } deriving (Eq, Show)

data Constant a = Constant
  { name  :: Ident
  , ty    :: Type a
  , val   :: Expr a
  , annot :: a
  } deriving (Eq, Show)

data Function a = Function
  { name   :: Ident
  , params :: [Ident]
  , body   :: [Statement a]
  , annot  :: a
  } deriving (Eq, Show)

data Statement a
  = Assign { dest  :: Name a
          ,  val   :: Expr a
          ,  annot :: a }
  | If { cond  :: Expr a
      ,  body  :: [Statement a]
      ,  elif  :: [(Expr a, [Statement a])]
      ,  els   :: Maybe [Statement a]
      ,  annot :: a }
  | For { var   :: Ident
       ,  from  :: Expr a
       ,  to    :: Expr a
       ,  body  :: [Statement a]
       ,  annot :: a }
  | Switch { value       :: Expr a
          ,  cases       :: [(Expr a, [Statement a])]
          ,  defaultCase :: Maybe [Statement a]
          ,  annot       :: a }
  | Barrier { annot :: a }
  | Break { annot :: a }
  | Return { retVal :: Maybe (Expr a)
          ,  annot  :: a }
  deriving (Eq, Show)

data Enumeration a = Enumeration
  { name   :: Ident
  , fields :: [(Ident, Maybe (Expr a))]
  , annot  :: a
  } deriving (Eq, Show)

data Direction a
  = In { annot :: a }
  | Out { annot :: a }
  | Const { annot :: a }
  deriving (Eq, Show)

data Expr a
  = Binary { binOp :: BinOp a
           , left  :: Expr a
           , right :: Expr a
           , annot :: a }
  | Unary { unOp  :: UnOp a
          , expr  :: Expr a
          , annot :: a }
  | PrimLit { lit   :: Literal a
            , annot :: a }
  | PrimName { name  :: Name a
             , annot :: a}
  | FunCall { name   :: Name a
            , params :: [Expr a]
            , annot  :: a }
  deriving (Eq, Show)

data BinOp a
  = PlusOp { annot :: a }
  | MinusOp { annot :: a }
  | MulOp { annot :: a }
  | DivOp { annot :: a }
  | ModOp { annot :: a }
  | EqOp { annot :: a }
  | NeqOp { annot :: a }
  | SllOp { annot :: a }
  | SrlOp { annot :: a }
  | LtOp { annot :: a }
  | GtOp { annot :: a }
  | LeqOp { annot :: a }
  | GeqOp { annot :: a }
  | AndOp { annot :: a }
  | OrOp { annot :: a }
  | XorOp { annot :: a }
  deriving (Eq, Show)

data UnOp a
  = UnPlus { annot :: a }
  | UnMinus { annot :: a }
  | NotOp { annot :: a }
  deriving (Eq, Show)

data Name a
  = Ident { ident :: Ident
         ,  annot :: a}
  | HierAccess { idents :: [Ident]
              ,  annot  :: a }
  | ArrayAccess { name  :: Name a
               ,  index :: Expr a
               ,  annot :: a }
  deriving (Eq, Show)

data Type a
  = Signed { size  :: Integer
           , annot :: a }
  | Unsigned { size  :: Integer
             , annot :: a }
  | Single { annot :: a }
  | Double { annot :: a }
  | Bool { annot :: a }
  | Array { arrLength :: Maybe (Expr a)
          , innerTy   :: Type a
          , annot     :: a }
  deriving (Eq, Show)

data Literal a
  = LitInt { intVal :: Integer
           , annot  :: a }
  | LitFloat { floatVal :: Double
             , annot    :: a }
  | LitString { stringVal :: String
              , annot     :: a }
  | LitArray { arrayVal :: [Expr a]
             , annot    :: a }
  | LitTrue { annot :: a }
  | LitFalse { annot :: a }
  deriving (Eq, Show)

type Ident = String
