module Main where

import           Assemble
import           Data.Char
import           Data.List
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
