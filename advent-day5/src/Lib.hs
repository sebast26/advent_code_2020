{-# LANGUAGE QuasiQuotes                      #-}
module Lib
    ( someFunc
    ) where

import Text.RE.TDFA.String ( matched, re, (?=~) )
import Data.List.Split ( splitOn )
import Data.Char (digitToInt)
import Data.List (foldl', sort)

parseIntoList :: String -> [String]
parseIntoList = splitOn "\n"

replaceChar :: Char -> Char
replaceChar 'F' = '0'
replaceChar 'B' = '1'
replaceChar 'L' = '0'
replaceChar 'R' = '1'

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

someFunc :: IO ()
someFunc = do
    content <- readFile "5.input"
    let input = parseIntoList content
    let binaryInput = [[replaceChar char| char <- line] | line <- input]
    let rowsAndColumns = [ (toDec (take 7 line), toDec (drop 7 line)) | line <- binaryInput]
    let seats = [fst seat * 8 + snd seat | seat <- rowsAndColumns]
    let test = matched $ "2016-01-09 2015-12-5 2015-10-05" ?=~ [re|[0-9]{4}-[0-9]{2}-[0-9]{2}|]
    -- putStrLn (show (length binaryInput))
    -- putStrLn (head binaryInput)
    -- putStrLn (show (head rowsAndColumns))
    -- putStrLn (show (head seats))
    putStrLn ("4a=" ++ show (maximum seats))
    let sortedBinaryInput = sort binaryInput
    let seatsDec = [toDec seat | seat <- sortedBinaryInput]
    let seatPairs = zip seatsDec (tail seatsDec)
    let result = [pair | pair <- seatPairs, snd pair - fst pair /= 1]
    putStrLn ("4b=" ++ show (head result))


