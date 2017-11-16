module VM.Instruction where

data Instruction
  = ConstantValue Int
  | Operator (Operator, Int)
  | Memory Memory
  | Label Label
  | Goto Goto
  | Function (FuncName, LocalArgs)
  | Return
  | Call (FuncName, Int, Int)
  deriving (Show)

type Filename = String

type Label = String

type FuncName = String

type LocalArgs = Int

data Goto
  = IfGoto String
  | AlwaysGoto String
  deriving (Show)

data Operator
  = Add
  | Sub
  | Neg
  | Eq
  | Gt
  | Lt
  | And
  | Or
  | Not
  deriving (Show, Read)

data Memory = Location
  { command :: Command
  , segment :: Segment
  , index   :: Int
  } deriving (Show)

data Segment
  = Argument
  | Local
  | Static
  | Constant
  | This
  | That
  | Pointer
  | Temp
  deriving (Show, Read)

data Command
  = Push
  | Pop
  deriving (Show, Read)

type Error = String
