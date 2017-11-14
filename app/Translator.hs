module Main where

import           Data.List
import           Data.Text          (pack, strip, unpack)
import           System.Environment
import           System.Exit
import           VM.Bootstrap
import           VM.Parse
import           VM.Translate

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "Please specify a file"
      exitFailure
    (x:xs) -> translateFile x

translateFile :: String -> IO ()
translateFile file = do
  asm <- readFile file
  let asmLines1 = lines asm
  let asmLines2 = map (takeWhile (/= '/')) asmLines1
  let asmLines3 = map (unpack . strip . pack) asmLines2
  let asmLines4 = filter (\x -> x /= []) asmLines3
  let asmLines5 = [(asmLines4 !! n, n) | n <- [0 .. ((length asmLines4) - 1)]]
  let instructions = map parse asmLines5
  let output = map (translate file) instructions
  putStr . intercalate "\n" . (bootstrap ++) . concat $ output
  return ()

(|>) = flip ($)
