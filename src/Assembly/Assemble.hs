module Assembly.Assemble where

import           Assembly.Instruction
import           Data.Char
import           Data.Either
import           Data.Either.Extra
import           Data.Maybe
import           Numeric

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
    noFirst || adPlus1 || aMinus1 || dMinusOrA
  where
    adPlus1 =
      checkMaybe [A,M,D] first && checkMaybe [Plus] operator && checkLeft One second
    aMinus1 = checkMaybe [A,M]first && checkMaybe [Minus] operator && checkLeft One second
    dMinusOrA = checkMaybe [D]first && checkMaybe [Minus,Or] operator && checkRight [A,M] second
    noFirst =
      isNothing first && not (checkLeft Zero second || checkRight [D] second)

negateD :: Computation -> Bool
negateD (Computation first operator second) = notDA && (one || aPlus1 || anyD)
  where
    anyD = checkMaybe [D] first || checkRight [D] second
    one = isNothing first && isNothing operator && checkLeft One second
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
    one = isNothing first && isNothing operator && checkLeft One second
    opADM =
      isNothing first &&
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
