module Lib
    ( someFunc
    ) where

import Data.Text ( pack, splitOn, unpack, map )
import Text.Regex.PCRE
    ( AllTextSubmatches(getAllTextSubmatches), (=~) )

replacePlusSign = Data.Text.map (\c -> if c == '+' then ' ' else c)

parseContent :: [Char] -> [(Int, String, Int)]
parseContent content = do
    let lines = [unpack (replacePlusSign line) | line <- splitOn (pack "\n") (pack content)]
    let lineRegex = "^(nop|acc|jmp) ([\\s+-]\\d+)$"
    let instructionLines = [getAllTextSubmatches (line =~ lineRegex) :: [String] | line <- lines]
    [(index, instructionLine !! 1, read (instructionLine !! 2) :: Int) | (index, instructionLine) <- zip [0..] instructionLines]

executeInst :: [(Int, String, Int)] -> Int -> [Int] -> Int -> (Int, String, Int) -> Int
executeInst _ acc hist instrPtr _
    | instrPtr `elem` hist = acc
executeInst list acc hist instPtr (i, "nop", _) = executeInst list acc (hist ++ [i]) (instPtr + 1) (list !! (instPtr  + 1))
executeInst list acc hist instPtr (i, "acc", accVal) = executeInst list (acc + accVal) (hist ++ [i]) (instPtr + 1) (list !! (instPtr + 1))
executeInst list acc hist instPtr (i, "jmp", jmpVal) = executeInst list acc (hist ++ [i]) (instPtr + jmpVal) (list !! (instPtr + jmpVal))

someFunc :: IO ()
someFunc = do
    content <- readFile "8.input"
    let codeInstructions = parseContent content
    let infiniteLoopAcc = executeInst codeInstructions 0 [] 0 (head codeInstructions)
    print infiniteLoopAcc
