-- | This module defines the syntax for the SME intermediate
-- representation. For details, see: TODO/langspec.pdf

{-# LANGUAGE DuplicateRecordFields #-}

module Language.SMEIL.Syntax where


newtype DesignFile = DesignFile
  { units :: [DesignUnit]
  } deriving (Eq, Show)

data DesignUnit = DesignUnit
  { imports   :: [Import] -- ^ Imports of the design unit
  , networks  :: [Network] -- ^ Networks of the design unit
  , processes :: [Process] -- ^ Processes of the design unit
  } deriving (Eq, Show)

-- | Specifies a module to be imported in current design module
newtype Import = Import
  { ident :: Ident -- ^ Name of the module to be imported
  } deriving (Eq, Show)

-- | Instantiates either a "Process" or a "Network"
data Instance = Instance
  { ident  :: Ident -- ^ The name of the instance
  , elName :: Ident -- ^ The name of the object to initialize
  , params :: [(Maybe Ident, Expr)] -- ^ Optionally named parameters of the object
  } deriving (Eq, Show)

-- | Defines a Network
data Network = Network
  { name     :: Ident -- ^ Name of network
  , netDecls :: [NetworkDecl] -- ^ Declarations in network
  } deriving (Eq, Show)

data NetworkDecl
  = NetInst { inst :: Instance} -- ^ A network instance
  | NetDecl { decl :: Declaration} -- ^ A network declaration
  deriving (Eq, Show)

data Bus = Bus
  { name    :: Ident -- ^
  , signals :: [BusSignal]
  } deriving (Eq, Show)

data BusSignal = BusSignal
  { name  :: Ident
  , ty    :: Type
  , value :: Maybe Expr
  , range :: Maybe Range
  } deriving (Eq, Show)

data Range = Range
  { lower :: Expr
  , upper :: Expr
  } deriving (Eq, Show)

data Process = Process
  { name       :: Ident
  , params     :: [(Direction, Ident)]
  , decls      :: [Declaration]
  , body       :: [Statement]
  , clocked    :: Bool
  , simulation :: Bool
  } deriving (Eq, Show)

data Declaration
  = VarDecl Variable
  | ConstDecl Constant
  | BusDecl Bus
  | FuncDecl Function
  deriving (Eq, Show)

data Variable = Variable
  { name  :: Ident
  , ty    :: Type
  , val   :: Maybe Expr
  , range :: Maybe Range
  } deriving (Eq, Show)

data Constant = Constant
  { name :: Ident
  , ty   :: Type
  , val  :: Expr
  } deriving (Eq, Show)

data Function = Function
  { name   :: Ident
  , params :: [Ident]
  , body   :: [Statement]
  } deriving (Eq, Show)

data Statement
  = Assign { var :: Ident
          ,  val :: Expr}
  | If { cond :: Expr
      ,  body :: [Statement]
      ,  elif :: [(Expr, [Statement])]
      ,  els  :: Maybe [Statement]}
  | For { var  :: Ident
       ,  from :: Expr
       ,  to   :: Expr
       ,  body :: [Statement]}
  | Switch { value       :: Expr
          ,  cases       :: [(Expr, [Statement])]
          ,  defaultCase :: Maybe [Statement]}
  | Barrier
  | Break
  | Return { val :: Expr }
  deriving (Eq, Show)

data Enumeration = Enumeration
  { name   :: Ident
  , fields :: [(Ident, Maybe Expr)]
  } deriving (Eq, Show)

data Direction
  = In
  | Out
  | Const
  deriving (Eq, Show)

data Expr
  = Binary { binOp :: BinOp
           , left  :: Expr
           , right :: Expr}
  | Unary { unOp :: UnOp
          , expr :: Expr}
  | PrimLit { lit :: Literal}
  | PrimName { name :: Ident}
  deriving (Eq, Show)

data BinOp
  = PlusOp
  | MinusOp
  | MulOp
  | DivOp
  | ModOp
  | EqOp
  | NeqOp
  | SllOp
  | SrlOp
  | LtOp
  | GtOp
  | LeqOp
  | GeqOp
  | AndOp
  | OrOp
  | XorOp
  deriving (Eq, Show)

data UnOp
  = UnPlus
  | UnMinus
  | NotOp
  deriving (Eq, Show)

data Type
  = Signed { size :: Int}
  | Unsigned { size :: Int}
  | Single
  | Double
  | Bool
  | Array { arrLength :: Maybe Int
         ,  innerTy   :: Type}
  deriving (Eq, Show)

data Literal
  = LitInt Integer
  | LitString String
  | LitTrue
  | LitFalse
  deriving (Eq, Show)

type Ident = String
