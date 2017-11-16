module VM.Translate where

import           Data.Char
import           Prelude        hiding (and, not, or)
import           VM.Goto
import           VM.Instruction
import           VM.Memory

translate :: Filename -> Either [Error] Instruction -> [String]
translate _ (Left errs) = errs
translate filename (Right instruction) =
  case instruction of
    ConstantValue n               -> printValue n
    Operator (op, num)            -> printOperator op num
    Memory loc                    -> printMemory filename loc
    Label label                   -> printLabel label
    Goto goto                     -> printGoto goto
    Function (func_name, var_num) -> printFunc func_name var_num
    Return                        -> printReturn
    Call (name, args, linenum)    -> printCall name args linenum

printCall :: String -> Int -> Int -> [String]
printCall name args linenum =
  [comment, "@" ++ returnAddr, "D=A"] ++
  pushFromD ++
  push "LCL" ++
  push "ARG" ++
  push "THIS" ++
  push "THAT" ++
  [ "@" ++ show (args + 5)
  , "D=A"
  , "@SP"
  , "D=M-D"
  , "@ARG"
  , "M=D"
  , "@SP"
  , "D=M"
  , "@LCL"
  , "M=D"
  , "@" ++ name
  , "0;JMP"
  , "(" ++ returnAddr ++ ")"
  ]
  where
    comment = "// call " ++ name ++ " with " ++ show args ++ " args"
    pushFromD = ["@SP", "A=M", "M=D", "@SP", "M=M+1"]
    push var = ["@" ++ var, "D=M"] ++ pushFromD
    returnAddr = "$RIP$" ++ show linenum

printReturn :: [String]
printReturn =
  ["@LCL", "D=M", "@FRAME", "M=D"] ++
  printDeref "RET" 5 ++
  ["@SP", "A=M-1", "D=M", "@ARG", "A=M", "M=D", "D=A+1", "@SP", "M=D"] ++
  printDeref "THAT" 1 ++
  printDeref "THIS" 2 ++
  printDeref "ARG" 3 ++ printDeref "LCL" 4 ++ ["@RET", "0;JMP"]

printDeref :: String -> Int -> [String]
printDeref name num =
  ["@" ++ show num, "D=A", "@FRAME", "A=M", "A=A-D", "D=M", "@" ++ name, "M=D"]

printFunc :: String -> Int -> [String]
printFunc func_name var_num =
  ["(" ++ func_name ++ ")"] ++ (concat $ take var_num $ repeat $ printValue 0)

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

printOperator :: Operator -> Int -> [String]
printOperator op n =
  case op of
    Add -> add
    Sub -> sub
    Neg -> neg
    And -> and
    Or  -> or
    Not -> not
    Eq  -> eq n
    Lt  -> lt n
    Gt  -> gt n

getStackToD :: [String]
getStackToD = ["@SP", "AM=M-1", "D=M", "A=A-1"]

add :: [String]
add = ["// add"] ++ getStackToD ++ ["M=M+D"]

sub :: [String]
sub = ["// sub"] ++ getStackToD ++ ["M=M-D"]

and :: [String]
and = ["// and"] ++ getStackToD ++ ["M=M&D"]

or :: [String]
or = ["// or"] ++ getStackToD ++ ["M=M|D"]

neg :: [String]
neg = ["// neg", "@SP", "A=M-1", "M=-M"]

not :: [String]
not = ["// not", "@SP", "A=M-1", "M=!M"]

lt :: Int -> [String]
lt = asmFunc "lt"

gt :: Int -> [String]
gt = asmFunc "gt"

eq :: Int -> [String]
eq = asmFunc "eq"

asmFunc :: String -> Int -> [String]
asmFunc name return =
  [ "// " ++ name
  , "@" ++ returnAddress
  , "D=A"
  , "@$" ++ (map toUpper name)
  , "0;JMP"
  , "(" ++ returnAddress ++ ")"
  ]
  where
    returnAddress = "$RIP$" ++ show return
