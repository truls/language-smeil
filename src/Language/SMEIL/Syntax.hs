-- | This module defines the syntax for the SME intermediate
-- representation. For details, see: TODO/langspec.pdf

{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Language.SMEIL.Syntax
  ( DesignFile(..)
  , DesignUnit(..)
  , UnitElement(..)
  , Import(..)
  , Instance(..)
  , Param(..)
  , Network(..)
  , NetworkDecl(..)
  , Bus(..)
  , BusSignal(..)
  , Range(..)
  , Process(..)
  , Generate(..)
  , Declaration(..)
  , Variable(..)
  , Constant(..)
  , Function(..)
  , Statement(..)
  , Enumeration(..)
  , Direction(..)
  , Expr(..)
  , BinOp(..)
  , UnOp(..)
  , Name(..)
  , ArrayIndex(..)
  , Type(..)
  , Literal(..)
  , Ident
  ) where

import           Data.Data (Data, Typeable)

data DesignFile a = DesignFile
  { units :: [DesignUnit a]
  , annot :: a
  } deriving (Eq, Show, Data, Typeable)

data DesignUnit a = DesignUnit
  { imports     :: [Import a] -- ^ Imports of the design unit
  , unitElement :: [UnitElement a] -- ^ A unit-level process or network
  , annot       :: a
  } deriving (Eq, Show, Data, Typeable)

data UnitElement a
  = UnitProc { process :: Process a }
  | UnitNet { network :: Network a }
  deriving (Eq, Show, Data, Typeable)

-- | Specifies a module to be imported in current design module
data Import a
  = SimpleImport { modName   :: [Ident] -- ^ Name of the module to be imported
                ,  qualified :: Maybe Ident -- ^ Optional qualified name of import
                ,  annot     :: a}
  | SpecificImport { modName   :: [Ident] -- ^ Name of module to be imported
                  ,  entities  :: [Ident] -- ^ Entities from module to be imported
                  ,  qualified :: Maybe Ident -- ^ Optional qualified name of import
                  ,  annot     :: a}
  deriving (Eq, Show, Data, Typeable)

-- | Instantiates either a "Process" or a "Network"
data Instance a = Instance
  { instName  :: Maybe Ident -- ^ The name of the instance
  , instIndex :: Maybe (Expr a)
  , elName    :: Ident -- ^ The name of the object to initialize
  , params    :: [(Maybe Ident, Expr a)] -- ^ Optionally named parameters of the object
  , annot     :: a
  } deriving (Eq, Show, Data, Typeable)

-- | Describes a parameter used in the specification of "Process" or "Network"
data Param a = Param
  { count :: Maybe (Maybe (Expr a)) -- ^ Length of array of params
  , dir   :: Direction a -- ^ Parameter direction
  , name  :: Ident -- ^ Parameter name
  , annot :: a
  } deriving (Eq, Show, Data, Typeable)

-- | Defines a Network
data Network a = Network
  { name     :: Ident -- ^ Name of network
  , params   :: [Param a] -- ^ Network parameters
  , netDecls :: [NetworkDecl a] -- ^ Declarations in network
  , annot    :: a
  } deriving (Eq, Show, Data, Typeable)

data NetworkDecl a
  = NetInst { inst :: Instance a -- ^ A network instance
            }
  | NetBus { bus :: Bus a -- ^ A network declaration
           }
  | NetConst { const :: Constant a -- ^ A network constant
             }
  | NetGen { gen :: Generate a -- ^ Generator statement
           }
  deriving (Eq, Show, Data, Typeable)

data Bus a = Bus
  { exposed :: Bool -- ^Bus is exposed on top level
  , unique  :: Bool -- ^Bus is unique, i.e., not duplicated on process instantiation
  , name    :: Ident -- ^Name of bus
  , signals :: [BusSignal a] -- ^Bus signals
  , annot   :: a
  } deriving (Eq, Show, Data, Typeable)

data BusSignal a = BusSignal
  { name  :: Ident -- ^Name of signal
  , ty    :: Type a -- ^Type of signal
  , value :: Maybe (Expr a) -- ^Initial value of signal
  , range :: Maybe (Range a) -- ^Signal range
  , annot :: a
  } deriving (Eq, Show, Data, Typeable)

data Range a = Range
  { lower :: Expr a -- ^Lower bound
  , upper :: Expr a-- ^Upper bound
  , annot :: a
  } deriving (Eq, Show, Data, Typeable)

data Process a = Process
  { name   :: Ident -- ^Name of process
  , params :: [Param a] -- ^Process parameters
  , decls  :: [Declaration a] -- ^Process declarations
  , body   :: [Statement a] -- ^Process body
  , sync   :: Bool -- ^Process is synchronous
  , annot  :: a
  } deriving (Eq, Show, Data, Typeable)

-- | Generator expression for use in Processes
data Generate a = Generate
  { var     :: Ident
  , from    :: Expr a
  , to      :: Expr a
  , genBody :: [NetworkDecl a]
  , annot   :: a
  } deriving (Eq, Show, Data, Typeable)

data Declaration a
  = VarDecl (Variable a)
  | ConstDecl (Constant a)
  | BusDecl (Bus a)
  | FuncDecl (Function a)
  | EnumDecl (Enumeration a)
  deriving (Eq, Show, Data, Typeable)

data Variable a = Variable
  { name  :: Ident
  , ty    :: Type a
  , val   :: Maybe (Expr a)
  , range :: Maybe (Range a)
  , annot :: a
  } deriving (Eq, Show, Data, Typeable)

data Constant a = Constant
  { name  :: Ident
  , ty    :: Type a
  , val   :: Expr a
  , annot :: a
  } deriving (Eq, Show, Data, Typeable)

data Function a = Function
  { name   :: Ident
  , params :: [(Ident, Type a)]
  , retTy  :: Type a
  , decls  :: [Declaration a]
  , body   :: [Statement a]
  , annot  :: a
  } deriving (Eq, Show, Data, Typeable)

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
  deriving (Eq, Show, Data, Typeable)

data Enumeration a = Enumeration
  { name   :: Ident
  , fields :: [(Ident, Maybe (Expr a))]
  , annot  :: a
  } deriving (Eq, Show, Data, Typeable)

data Direction a
  = In { annot :: a }
  | Out { annot :: a }
  | Const { annot :: a }
  deriving (Eq, Show, Data, Typeable)

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
             , annot :: a }
  | FunCall { name   :: Name a
            , params :: [Expr a]
            , annot  :: a }
  deriving (Eq, Show, Data, Typeable)

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
  deriving (Eq, Show, Data, Typeable)

data UnOp a
  = UnPlus { annot :: a }
  | UnMinus { annot :: a }
  | NotOp { annot :: a }
  deriving (Eq, Show, Data, Typeable)

data Name a
  = Ident { ident :: Ident
         ,  annot :: a}
  | HierAccess { idents :: [Name a]
              ,  annot  :: a}
  | ArrayAccess { name  :: Name a
               ,  index :: ArrayIndex a
               ,  annot :: a}
  deriving (Eq, Show, Data, Typeable)

data ArrayIndex a
  = Wildcard
  | Index (Expr a)
  deriving (Eq, Show, Data, Typeable)

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
  deriving (Eq, Show, Data, Typeable)

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
  deriving (Eq, Show, Data, Typeable)

type Ident = String
