module VM.Goto where

import           VM.Instruction

printLabel :: String -> [String]
printLabel label = ["(" ++ label ++ ")"]

printGoto :: Goto -> [String]
printGoto (IfGoto label)     = printIfGoto label
printGoto (AlwaysGoto label) = printAlwaysGoto label

printIfGoto :: String -> [String]
printIfGoto label =
  ["// if-goto " ++ label, "@SP", "AM=M-1", "D=M", "@" ++ label, "D;JNE"]

printAlwaysGoto :: String -> [String]
printAlwaysGoto label = ["// goto " ++ label, "@" ++ label, "0;JMP"]
