module Assembly.Instruction where

data Instruction
  = AInstruction Address
  | CInstruction Operation
  deriving (Show)

data Register
  = M
  | D
  | A
  deriving (Show, Read, Eq)

data Constant
  = Zero
  | One
  deriving (Show, Eq)

data Operator
  = Minus
  | Not
  | Plus
  | And
  | Or
  deriving (Show, Eq)

data Jump
  = JGT
  | JEQ
  | JGE
  | JLT
  | JNE
  | JLE
  | JMP
  deriving (Show, Read)

data Computation = Computation
  { first    :: Maybe Register
  , operator :: Maybe Operator
  , second   :: Either Constant Register
  } deriving (Show)

noComp :: Computation
noComp = Computation Nothing Nothing (Left Zero)

data Operation = Operation
  { dest :: [Register]
  , comp :: Computation
  , jump :: Maybe Jump
  } deriving (Show)

type Error = String

type Address = Int
