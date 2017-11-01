module Assembler where

import           Data.Char
import           Data.Either
import           Data.Either.Extra
import           Data.List
import           Data.Maybe
import           Numeric
import           Text.Read

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

main :: IO ()
main = do
  putStrLn "File: "
  file <- getLine
  asm <- readFile file
  let asmLines = lines asm
  --putStr $ intercalate "\n" asmLines
  let noComments = map (takeWhile (/= '/')) asmLines
  let stripSpace = map (filter (\x -> not (isSpace x))) noComments
  let stripEmpty = filter (\x -> x /= []) stripSpace
  --putStr $ intercalate "\n" stripEmpty
  let instructions = map parse stripEmpty
  let output = map assemble instructions
  putStr $ intercalate "\n" output
  return ()

assemble :: Either [Error] Instruction -> String
assemble (Left err) = concat err
assemble (Right (AInstruction address)) = leftpad 16 '0' $ toBinary address
assemble (Right (CInstruction op)) =
  prefix ++
  (formatCmp $ comp op) ++ (formatDst $ dest op) ++ (formatJmp $ jump op)
  where
    prefix = "111"

formatCmp :: Computation -> String
formatCmp cmp@(Computation first operator second) =
  pred a ++
  pred (not $ (checkMaybe [D] first) || (checkRight [D] second)) ++
  pred (negateAM cmp) ++
  pred (not $ (checkMaybe [A, M] first) || (checkRight [A, M] second)) ++
  pred (negateD cmp) ++
  pred ((checkMaybe [Minus, Plus] operator) || (isLeft second)) ++
  pred (negateOutput cmp)
  where
    a = checkMaybe [M] first || checkRight [M] second
    pred p =
      if p
        then "1"
        else "0"

negateAM :: Computation -> Bool
negateAM (Computation first operator second) =
  noFirst &&
  not
    (((checkMaybe [D] first &&
       checkMaybe [Minus] operator && checkLeft One second)) ||
     ((checkMaybe [D] first &&
       checkMaybe [Plus] operator && checkRight [A, M] second)) ||
     ((checkMaybe [D] first &&
       checkMaybe [And] operator && checkRight [A, M] second)))
  where
    noFirst =
      not (checkMaybe [] first) &&
      not (checkLeft Zero second || checkRight [D] second)

negateD :: Computation -> Bool
negateD (Computation first operator second) = notDA && (one || aPlus1 || anyD)
  where
    anyD = checkMaybe [D] first || checkRight [D] second
    one = checkMaybe [] first && checkMaybe [] operator && checkLeft One second
    aPlus1 =
      checkMaybe [A, M] first &&
      checkMaybe [Plus] operator && checkLeft One second
    notDA =
      not
        (checkMaybe [D] first &&
         checkRight [A, M] second && checkMaybe [Plus, Minus, And] operator)

negateOutput :: Computation -> Bool
negateOutput (Computation first operator second) =
  one || opADM || admPlusOne || admOpAdm
  where
    one = checkMaybe [] first && checkMaybe [] operator && checkLeft One second
    opADM =
      checkMaybe [] first &&
      checkMaybe [Minus, Not] operator && checkRight [A, D, M] second
    admPlusOne =
      checkMaybe [A, D, M] first &&
      checkMaybe [Plus] operator && checkLeft One second
    admOpAdm =
      checkMaybe [A, D, M] first &&
      checkMaybe [Minus, Or] operator && checkRight [A, D, M] second

checkMaybe as mb =
  case mb of
    Nothing -> False
    Just b  -> elem b as

checkLeft a (Left e)  = a == e
checkLeft a (Right e) = False

checkRight a (Right e) = elem e a
checkRight a (Left e)  = False

formatDst :: [Register] -> String
formatDst rs = (check A) ++ (check D) ++ (check M)
  where
    check r =
      if elem r rs
        then "1"
        else "0"

formatJmp :: Maybe Jump -> String
formatJmp Nothing = "000"
formatJmp (Just jump) =
  case jump of
    JGT -> "001"
    JEQ -> "010"
    JGE -> "011"
    JLT -> "100"
    JNE -> "101"
    JLE -> "110"
    JMP -> "111"

toBinary :: Int -> String
toBinary n = showIntAtBase 2 intToDigit n ""

leftpad :: Int -> Char -> String -> String
leftpad n c str =
  case compare n (length str) of
    GT -> (++ str) . take diff . repeat $ c
    _  -> str
  where
    diff = n - (length str)

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
      dest = parseDest dst
      comp = parseComp . dropWhile (== '=') $ cmp
      jump = parseJump . dropWhile (== ';') $ jmp
  in mkCInstruction dest comp jump

mkCInstruction ::
     Either [Error] [Register]
  -> Either [Error] Computation
  -> Either Error (Maybe Jump)
  -> Either [Error] Instruction
mkCInstruction (Left err) c j = Left $ err ++ (concat $ lefts [c]) ++ lefts [j]
mkCInstruction d (Left err) j = Left $ err ++ (concat $ lefts [d]) ++ lefts [j]
mkCInstruction d c (Left err) =
  Left $ err : (concat $ lefts [d]) ++ (concat $ lefts [c])
mkCInstruction dest comp jump =
  Right $
  CInstruction $
  Operation (fromRight [] dest) (fromRight noComp comp) (fromRight Nothing jump)

mkAInstruction :: String -> Either [Error] Instruction
mkAInstruction i =
  let mi = readMaybe i :: Maybe Int
  in case mi of
       Nothing -> Left [(i ++ " could not be parsed as an address.")]
       Just i  -> Right (AInstruction i)

parseComp :: String -> Either [Error] Computation
parseComp str =
  case length str of
    3 ->
      mkComp
        (parseFirst $ str !! 0)
        (parseBinaryOperator $ str !! 1)
        (parseSecond $ str !! 2)
    2 ->
      mkComp
        (Right Nothing)
        (parseUnaryOperator $ str !! 0)
        (parseSecond $ str !! 1)
    1 -> mkComp (Right Nothing) (Right Nothing) (parseSecond $ str !! 0)
    _ -> Left $ [str ++ " could not be parsed into a computation"]

parseFirst :: Char -> Either Error (Maybe Register)
parseFirst c =
  case parseReg c of
    Left e  -> Left e
    Right r -> Right (Just r)

parseSecond :: Char -> Either Error (Either Constant Register)
parseSecond c =
  case c of
    '0' -> Right (Left Zero)
    '1' -> Right (Left One)
    _ ->
      case parseReg c of
        Left err -> Left err
        Right r  -> Right (Right r)

parseUnaryOperator :: Char -> Either Error (Maybe Operator)
parseUnaryOperator c =
  case c of
    '!' -> Right (Just Not)
    '-' -> Right (Just Minus)
    '&' -> Left $ c : binaryErr
    '|' -> Left $ c : binaryErr
    '+' -> Left $ c : binaryErr
    _   -> Left $ "Unrecognized operator: " ++ [c]
  where
    binaryErr = " is a unary operator but used in a binary context"

parseBinaryOperator :: Char -> Either Error (Maybe Operator)
parseBinaryOperator c =
  case c of
    '!' -> Left "! is a unary operator but used in a binary context"
    '-' -> Right (Just Minus)
    '&' -> Right (Just And)
    '|' -> Right (Just Or)
    '+' -> Right (Just Plus)
    _   -> Left $ "Unrecognized operator: " ++ [c]

mkComp ::
     Either Error (Maybe Register)
  -> Either Error (Maybe Operator)
  -> Either Error (Either Constant Register)
  -> Either [Error] Computation
mkComp (Left e) op s = Left $ e : lefts [op] ++ lefts [s]
mkComp f (Left e) s = Left $ e : lefts [f] ++ lefts [s]
mkComp f op (Left e) = Left $ e : lefts [f] ++ lefts [op]
mkComp f op s =
  Right
    (Computation
       (fromRight Nothing f)
       (fromRight Nothing op)
       (fromRight (Left Zero) s))

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

parseJump :: String -> Either Error (Maybe Jump)
parseJump [] = Right Nothing
parseJump j =
  case readEither j of
    Right ok -> Right (Just ok)
    Left _   -> Left $ j ++ " could not be parsed as a jump command"
