module Assembler where

import           Data.Maybe
import           Text.Read

data Instruction
  = AInstruction Int
  | CInstruction Operation
  deriving (Show)

data Register
  = M
  | D
  | A
  deriving (Show, Read)

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

type Error = String

assemble :: String -> Instruction
assemble = undefined

parse inst@(x:xs) =
  case x of
    '@' -> mkAInstruction xs
    --_   -> mkCInstruction inst

mkCInstruction inst =
  let (first, second) = break (== '=') inst
      (dst, other) =
        if second == []
          then ([], first)
          else (first, second)
      (cmp, jmp) = break (== ';') other
  in CInstruction $
     Operation
       (parseDest dst)
       (dropWhile (== '=') cmp)
       (parseJump (dropWhile (== ';') jmp))

mkAInstruction :: String -> Either Error Instruction
mkAInstruction i =
  let mi = readMaybe i :: Maybe Int
  in case mi of
       Nothing -> Left $ i ++ " could not be parsed as an address."
       Just i  -> Right (AInstruction i)

parseDest :: String -> [Register]
parseDest = catMaybes . map parseReg

parseReg :: Char -> Maybe Register
parseReg c = readMaybe [c]

parseJump :: String -> Maybe Jump
parseJump = readMaybe
