module VM.Memory where

import           VM.Instruction

printMemory :: Filename -> Memory -> [String]
printMemory filename (Location command segment index) =
  case segment of
    Static  -> printStatic filename command segment index
    Pointer -> printPointer command segment index
    _       -> printHeap command segment index

printStatic :: Filename -> Command -> Segment -> Int -> [String]
printStatic filename command segment index =
  case command of
    Pop  -> comment : popToD name
    Push -> comment : [name, "D=M"] ++ pushAndIncSP
  where
    comment = mkComment command segment index
    name = "@" ++ filename ++ "." ++ show index

printPointer :: Command -> Segment -> Int -> [String]
printPointer command segment index =
  case command of
    Pop  -> comment : popToD name
    Push -> comment : [name, "D=M"] ++ pushAndIncSP
  where
    comment = mkComment command segment index
    name =
      if index == 1
        then "@THAT"
        else "@THIS"

printHeap :: Command -> Segment -> Int -> [String]
printHeap command segment index =
  case command of
    Pop ->
      comment : (saveIndex index) ++ (saveMemR13 $ asmSeg segment) ++ popR13
    Push ->
      comment :
      (saveIndex index) ++ (getMemToD $ asmSeg segment) ++ pushAndIncSP
  where
    comment = mkComment command segment index

saveIndex :: Int -> [String]
saveIndex index = ["@" ++ show index, "D=A"]

saveMemR13 :: String -> [String]
saveMemR13 segment
  | segment == "5" = ["@" ++ segment, "D=A+D", "@R13", "M=D"]
  | otherwise = ["@" ++ segment, "D=M+D", "@R13", "M=D"]

getMemToD :: String -> [String]
getMemToD segment
  | segment == "5" = ["@" ++ segment, "A=A+D", "D=M"]
  | otherwise = ["@" ++ segment, "A=M+D", "D=M"]

popR13 :: [String]
popR13 = ["@SP", "AM=M-1", "D=M", "@R13", "A=M", "M=D"]

pushAndIncSP :: [String]
pushAndIncSP = ["@SP", "A=M", "M=D", "@SP", "M=M+1"]

popToD :: String -> [String]
popToD name = ["@SP", "AM=M-1", "D=M", name, "M=D"]

mkComment :: Command -> Segment -> Int -> String
mkComment command segment index =
  "// " ++ (show command) ++ (show segment) ++ (show index)

asmSeg :: Segment -> String
asmSeg segment =
  case segment of
    Local    -> "LCL"
    Argument -> "ARG"
    Static   -> "static"
    Constant -> "constant"
    This     -> "THIS"
    That     -> "THAT"
    Pointer  -> "pointer"
    Temp     -> "5"
