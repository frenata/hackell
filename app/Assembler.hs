module Main where

import           Assemble
import           Data.Char
import           Data.List
import           Symbols
import           System.Environment
import           System.Exit

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "Please specify a file"
      exitFailure
    (x:xs) -> assembleFile x

assembleFile :: String -> IO ()
assembleFile file = do
  asm <- readFile file
  let asmLines1 = lines asm
  let asmLines2 = map (takeWhile (/= '/')) asmLines1
  let asmLines3 = map (filter (\x -> not (isSpace x))) asmLines2
  let asmLines4 = filter (\x -> x /= []) asmLines3
  let noSymbols =
        addLabels (baseSymbolTable, asmLines4) |> addVars |> replaceSymbols
  let instructions = map parse noSymbols
  let output = map assemble instructions
  putStr $ intercalate "\n" output
  return ()

(|>) = flip ($)
