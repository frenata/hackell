module Main where

import           Assemble
import qualified Data.Map   as M
import           Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "Computations" $ do
      it "verify all computations against table" $ do
        M.foldrWithKey (\k v acc -> acc && checkComp k (asm k)) True compTable
      it "verify all destinations against table" $ do
        M.foldrWithKey (\k v acc -> acc && checkDest k (asm k)) True destTable
      it "verify all jumps against table" $ do
        M.foldrWithKey (\k v acc -> acc && checkJump k (asm k)) True jumpTable
  where
    asm = assemble . parse

compTable :: (M.Map String String)
compTable =
  M.fromList
    [ ("0", wrap "0101010")
    , ("1", wrap "0111111")
    , ("-1", wrap "0111010")
    , ("D", wrap "0001100")
    , ("A", wrap "0110000")
    , ("M", wrap "1110000")
    , ("!D", wrap "0001101")
    , ("!A", wrap "0110001")
    , ("!M", wrap "1110001")
    , ("-D", wrap "0001111")
    , ("-A", wrap "0110011")
    , ("-M", wrap "1110011")
    , ("D+1", wrap "0011111")
    , ("A+1", wrap "0110111")
    , ("M+1", wrap "1110111")
    , ("D-1", wrap "0001110")
    , ("A-1", wrap "0110010")
    , ("M-1", wrap "1110010")
    , ("D+A", wrap "0000010")
    , ("D+M", wrap "1000010")
    , ("D-A", wrap "0010011")
    , ("D-M", wrap "1010011")
    , ("A-D", wrap "0000111")
    , ("M-D", wrap "1000111")
    , ("D&A", wrap "0000000")
    , ("D&M", wrap "1000000")
    , ("D|A", wrap "0010101")
    , ("D|M", wrap "1010101")
    ]
  where
    wrap cmp = prefix ++ cmp ++ postfix
    prefix = "111"
    postfix = "000000"

checkComp :: String -> String -> Bool
checkComp cmp result =
  case fmap (== result) $ M.lookup cmp compTable of
    Nothing -> False
    Just b  -> b

checkMap = M.mapWithKey (\k v -> (asm k, checkComp k (asm k))) compTable
  where
    asm = assemble . parse

badEntries = filter (\x -> snd (snd x) == False) (M.toList checkMap)


destTable :: (M.Map String String)
destTable =
  M.fromList
    [ ("M=1", wrap "001")
    , ("D=1", wrap "010")
    , ("MD=1", wrap "011")
    , ("A=1", wrap "100")
    , ("AM=1", wrap "101")
    , ("AD=1", wrap "110")
    , ("AMD=1", wrap "111")]
    where wrap dst = prefix ++ dst ++ postfix
          prefix = "1110111111"
          postfix = "000"

checkDest :: String -> String -> Bool
checkDest dst result =
  case fmap (== result) $ M.lookup dst destTable of
    Nothing -> False
    Just b  -> b

jumpTable :: (M.Map String String)
jumpTable =
  M.fromList
    [ ("1;JGT", wrap "001")
    , ("1;JEQ", wrap "010")
    , ("1;JGE", wrap "011")
    , ("1;JLT", wrap "100")
    , ("1;JNE", wrap "101")
    , ("1;JLE", wrap "110")
    , ("1;JMP", wrap "111")]
    where wrap jmp = prefix ++ jmp
          prefix = "1110111111000"

checkJump :: String -> String -> Bool
checkJump jmp result =
  case fmap (== result) $ M.lookup jmp jumpTable of
    Nothing -> False
    Just b  -> b

