module VMTranslate where

import           VMParser

translate :: Either [Error] Instruction -> [String]
translate (Left errs) = errs
translate (Right instruction) =
  case instruction of
    ConstantValue n -> printValue n
    Operator op     -> printOperator op
    Memory loc      -> printMemory loc

printValue :: Int -> [String]
printValue n =
  [ "// push constant " ++ index
  , '@' : index
  , "D=A"
  , "@SP"
  , "A=M"
  , "M=D"
  , "@SP"
  , "M=M+1"
  ]
  where
    index = show n

printOperator :: Operator -> [String]
printOperator op =
  case op of
    Add -> add
    _   -> []

printMemory :: Memory -> [String]
printMemory loc = undefined

add :: [String]
add = ["// add", "@SP", "AM=M-1", "D=M", "A=A-1", "M=M+D"]
