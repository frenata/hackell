module Assembler where

import           Data.Either
import           Data.Either.Extra
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

parse :: String -> Either [Error] Instruction
parse inst@(x:xs) =
  case x of
    '@' -> mkAInstruction xs
    _   -> parseC inst

parseC :: String -> Either [Error] Instruction
parseC inst =
  let (first, second) = break (== '=') inst
      (dst, other) =
        if second == []
          then ([], first)
          else (first, second)
      (cmp, jmp) = break (== ';') other
      dest = (parseDest dst)
      comp = (dropWhile (== '=') cmp)
      jump = (parseJump (dropWhile (== ';') jmp))
  in mkCInstruction dest (Right comp) jump

mkCInstruction ::
     Either [Error] [Register]
  -> Either Error String
  -> Either Error Jump
  -> Either [Error] Instruction
mkCInstruction (Left err) c j = Left $ err ++ lefts [c] ++ lefts [j]
mkCInstruction d (Left err) j = Left $ err : (concat $ lefts [d]) ++ lefts [j]
mkCInstruction d c (Left err) = Left $ err : (concat $ lefts [d]) ++ lefts [c]
mkCInstruction dest comp jump =
  Right $
  CInstruction $
  Operation (fromRight [] dest) (fromRight "" comp) (Just $ fromRight JMP jump)

mkAInstruction :: String -> Either [Error] Instruction
mkAInstruction i =
  let mi = readMaybe i :: Maybe Int
  in case mi of
       Nothing -> Left [(i ++ " could not be parsed as an address.")]
       Just i  -> Right (AInstruction i)

parseDest :: String -> Either [Error] [Register]
parseDest str =
  let (lefts, rights) = partitionEithers $ map parseReg str
  in case length lefts of
       0 -> Right rights
       _ -> Left lefts

parseReg :: Char -> Either Error Register
parseReg c =
  case readEither [c] of
    Right ok -> Right ok
    Left _   -> Left $ c : " could not be parsed as a register"

parseJump :: String -> Either Error Jump
parseJump j =
  case readEither j of
    Right ok -> Right ok
    Left _   -> Left $ j ++ " could not be parsed as a jump command"
