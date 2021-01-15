module Lib
    ( someFunc
    ) where

import Data.Text ( pack, splitOn, unpack, map )
import Text.Regex.PCRE
    ( AllTextSubmatches(getAllTextSubmatches), (=~) )

replacePlusSign = Data.Text.map (\c -> if c == '+' then ' ' else c)

parseContent :: (Num a, Enum a) => [Char] -> [(a, String, Integer)]
parseContent content = do
    let lines = [unpack (replacePlusSign line) | line <- splitOn (pack "\n") (pack content)]
    let lineRegex = "^(nop|acc|jmp) ([\\s+-]\\d+)$"
    let instructionLines = [getAllTextSubmatches (line =~ lineRegex) :: [String] | line <- lines]
    [(index, instructionLine !! 1, read (instructionLine !! 2) :: Integer) | (index, instructionLine) <- zip [0..] instructionLines]

someFunc :: IO ()
someFunc = do
    content <- readFile "8.sample"
    let codeInstructions = parseContent content
    print codeInstructions
