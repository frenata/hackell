module VMParser where

import           Data.Char
import           Data.Either
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Text.Read

data Instruction
  = ConstantValue Int
  | Operator Operator
  | Memory Memory
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

parse :: String -> Either [Error] Instruction
parse str
  | pushConstant `isPrefixOf` str =
    mkConstant $ fromJust (stripPrefix pushConstant str)
  | isOperator str = Right $ Operator $ mkOperator str
  | push `isPrefixOf` str && count ' ' str >= 2 =
    let [com, seg, index] = splitOn " " str
    in mkMemory (readCommand com) (readSegment seg) (readIndex index)
  | otherwise = Left [str ++ " was not recognized as a valid instruction."]
  where
    pushConstant = "push constant "
    push = "push"

mkConstant :: String -> Either [Error] Instruction
mkConstant str =
  case readEither str of
    Left _  -> Left $ [str ++ " could not be parsed into a constant value."]
    Right n -> Right (ConstantValue n)

mkOperator :: String -> Operator
mkOperator (x:xs) = read (toUpper x : xs)

isOperator :: String -> Bool
isOperator [] = False
isOperator (x:xs) =
  case readOp (toUpper x : xs) of
    Nothing -> False
    Just _  -> True

readOp :: String -> Maybe Operator
readOp = readMaybe

mkMemory ::
     Either Error Command
  -> Either Error Segment
  -> Either Error Int
  -> Either [Error] Instruction
mkMemory (Left err) s i = Left $ err : lefts [s] ++ lefts [i]
mkMemory c (Left err) i = Left $ err : lefts [c] ++ lefts [i]
mkMemory c s (Left err) = Left $ err : lefts [s] ++ lefts [c]
mkMemory (Right com) (Right seg) (Right index) =
  Right $ Memory $ Location com seg index

{-
mkCInstruction (Left err) c j = Left $ err ++ (concat $ lefts [c]) ++ lefts [j]
mkCInstruction d (Left err) j = Left $ err ++ (concat $ lefts [d]) ++ lefts [j]
mkCInstruction d c (Left err) =
  Left $ err : (concat $ lefts [d]) ++ (concat $ lefts [c])
-}
readCommand :: String -> Either Error Command
readCommand str =
  case readEither (capFirst str) of
    Left _  -> Left $ str ++ " could not be parsed into a push/pop command."
    Right a -> Right a

readSegment :: String -> Either Error Segment
readSegment str =
  case readEither (capFirst str) of
    Left _  -> Left $ str ++ " could not be parsed into a memory segment"
    Right a -> Right a

readIndex :: String -> Either Error Int
readIndex str =
  case readEither str of
    Left _  -> Left $ str ++ " could not be parsed into an index."
    Right a -> Right a

capFirst :: String -> String
capFirst []     = []
capFirst (x:xs) = toUpper x : xs

count :: Char -> String -> Int
count c = length . findIndices (== c)
