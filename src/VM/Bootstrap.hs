module VM.Bootstrap where

bootstrap :: [String]
bootstrap =
  ["@START", "0;JMP"] ++ eqFunction ++ ltFunction ++ gtFunction ++ ["(START)"]

eqFunction :: [String]
eqFunction =
  "($EQ)" :
  saveReturnAddress ++
  arg2LessArg1 ++
  ["@$EQ_END", "D;JEQ", "D=-1", "($EQ_END)", "D=!D"] ++ pushAndReturn

ltFunction :: [String]
ltFunction =
  "($LT)" :
  saveReturnAddress ++
  arg2LessArg1 ++
  [ "@$LT_TRUE"
  , "D;JLT"
  , "D=0"
  , "@$LT_END"
  , "0;JMP"
  , "($LT_TRUE)"
  , "D=-1"
  , "($LT_END)"
  ] ++
  pushAndReturn

gtFunction :: [String]
gtFunction =
  "($GT)" :
  saveReturnAddress ++
  arg2LessArg1 ++
  [ "@$GT_TRUE"
  , "D;JGT"
  , "D=0"
  , "@$GT_END"
  , "0;JMP"
  , "($GT_TRUE)"
  , "D=-1"
  , "($GT_END)"
  ] ++
  pushAndReturn

saveReturnAddress = ["@R15", "M=D"]

arg2LessArg1 = ["@SP", "AM=M-1", "D=M", "A=A-1", "D=M-D"]

pushAndReturn = ["@SP", "A=M-1", "M=D", "@R15", "A=M", "0;JMP"]
