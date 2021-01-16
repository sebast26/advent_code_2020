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

executeInst :: [(Int, String, Int)] -> Int -> [Int] -> Int -> (Int, String, Int) -> (Int, Bool)
executeInst _ acc hist instrPtr _
    | instrPtr `elem` hist = (acc, False)
executeInst _ acc _ _ (_, "end", _) = (acc, True)
executeInst list acc hist instPtr (i, "nop", _) = executeInst list acc (hist ++ [i]) (instPtr + 1) (list !! (instPtr  + 1))
executeInst list acc hist instPtr (i, "acc", accVal) = executeInst list (acc + accVal) (hist ++ [i]) (instPtr + 1) (list !! (instPtr + 1))
executeInst list acc hist instPtr (i, "jmp", jmpVal) = executeInst list acc (hist ++ [i]) (instPtr + jmpVal) (list !! (instPtr + jmpVal))

replaceNth :: Int -> a -> [a] -> [a]
replaceNth n newVal list = do
    let (x,_:y) = splitAt n list
    x ++ [newVal] ++ y

findNextIndex :: [(Int, String, Int)] -> Int -> String -> Int
findNextIndex instructions index fnName = do
    let (_, instName, _) = instructions !! (index + 1)
    if instName == fnName then index + 1 else findNextIndex instructions (index + 1) fnName

findReplacement :: [(Int, String, Int)] -> (Int, Int, Bool) -> (Int, Bool)
findReplacement _ (_, acc, True) = (acc, True)
findReplacement instructions (index, _, _) = do
    let newIndex = findNextIndex instructions index "jmp"
    let modifiedInstructions = replaceNth newIndex (newIndex, "nop", -4) instructions
    let (executionAcc, isTerminated) = executeInst modifiedInstructions 0 [] 0 (head instructions)
    findReplacement instructions (newIndex, executionAcc, isTerminated)

someFunc :: IO ()
someFunc = do
    content <- readFile "8.input"
    let codeInstructions = parseContent content
    let (infiniteLoopAcc, _) = executeInst codeInstructions 0 [] 0 (head codeInstructions)
    print infiniteLoopAcc
    let stopInstruction = (0, "end", 0)
    let (instAcc, terminated) = findReplacement (codeInstructions ++ [stopInstruction]) (0, 0, False)
    print instAcc
