module Assembler where

data Instruction
  = A Int
  | C Operation
  deriving (Show)

data Register
  = MReg
  | DReg
  | AReg
  deriving (Show)

data Jump
  = JGT
  | JEQ
  | JGE
  | JLT
  | JNE
  | JLE
  | JMP
  deriving (Show, Read)

data Computation =
  Computation
  deriving (Show)

data Operation = Operation
  { dest :: [Register]
  , comp :: String -- Computation
  , jump :: Maybe Jump
  } deriving (Show)

assemble :: String -> Instruction
assemble = undefined

parse :: String -> Instruction
parse inst@(x:xs) =
  if x == '@'
    then A (read xs :: Int)
    else let (first, second) = break (== '=') inst
             (dst, other) =
               if second == []
                 then ([], first)
                 else (first, second)
             (cmp, jmp) = break (== ';') other
         in C $
            Operation
              (parseDest dst)
              (dropWhile (== '=') cmp)
              (parseMaybeJump (dropWhile (== ';') jmp))

parseDest :: String -> [Register]
parseDest ""  = []
parseDest dst = map parseReg dst

parseReg :: Char -> Register
parseReg 'D' = DReg
parseReg 'A' = AReg
parseReg 'M' = MReg
parseReg _   = undefined

parseMaybeJump :: String -> Maybe Jump
parseMaybeJump "" = Nothing
parseMaybeJump j  = Just (parseJump j)

parseJump :: String -> Jump
parseJump "JGT" = JGT
parseJump "JEQ" = JEQ
parseJump "JGE" = JGE
parseJump "JLT" = JLT
parseJump "JNE" = JNE
parseJump "JLE" = JLE
parseJump "JMP" = JMP
parseJump _     = undefined
