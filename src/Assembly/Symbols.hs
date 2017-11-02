module Assembly.Symbols where

import qualified Data.Map  as M
import           Text.Read

type SymbolTable = M.Map String Int

replaceSymbols :: (SymbolTable, [String]) -> [String]
replaceSymbols (table, lines) = map (replaceSymbol table) lines

replaceSymbol :: SymbolTable -> String -> String
replaceSymbol table str =
  case M.lookup str table of
    Nothing -> str
    Just a  -> "@" ++ (show a)

baseSymbolTable :: (SymbolTable)
baseSymbolTable =
  M.fromList $
  [ ("@SP", 0)
  , ("@LCL", 1)
  , ("@ARG", 2)
  , ("@THIS", 3)
  , ("@THAT", 4)
  , ("@SCREEN", 16384)
  , ("@KBD", 24576)
  ] ++
  registers

registers :: [(String, Int)]
registers = map (\x -> ("@R" ++ (show x), x)) [0 .. 15]

addSymbol :: String -> Int -> SymbolTable -> SymbolTable
addSymbol str n table = M.insert str n table

addLabels :: (SymbolTable, [String]) -> (SymbolTable, [String])
addLabels (table, lines) = go table lines 0
  where
    go table [] n = (table, [])
    go table (l:lines) n =
      if isLabel l
        then let newTable = addSymbol (getLabel l) n table
             in go newTable lines n
        else let (newTable, xs) = go table lines (n + 1)
             in (newTable, l : xs)

isLabel :: String -> Bool
isLabel [] = False
isLabel xs = (head xs) == '(' && (last xs) == ')'

getLabel :: String -> String
getLabel [] = []
getLabel xs = ('@' :) . drop 1 . takeWhile (/= ')') $ xs

addVars :: (SymbolTable, [String]) -> (SymbolTable, [String])
addVars (table, lines) = go table lines 16
  where
    go table [] n = (table, [])
    go table (l:lines) n =
      if isSymbol l table
        then let newTable = addSymbol l n table
                 (rTable, xs) = go newTable lines (n + 1)
             in (rTable, l : xs)
        else let (rTable, xs) = go table lines n
             in (rTable, l : xs)

isSymbol :: String -> SymbolTable -> Bool
isSymbol [] _ = False
isSymbol line@(x:xs) table =
  M.lookup line table == Nothing &&
  x == '@' &&
  case readMaybe xs :: Maybe Int of
    Nothing -> True
    Just a  -> False
