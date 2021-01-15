module Lib
    ( someFunc, intersectAll
    ) where

import Data.Text ( splitOn, pack, unpack )
import Data.Set ( fromList, toList, delete, size, intersection )

intersectAll :: Ord a => [[a]] -> [a]
intersectAll [] = []
intersectAll (x:[]) = x
intersectAll (x:y) = do
    let intersect = toList (intersection (fromList x) (fromList (head y)))
    intersectAll (intersect : tail y)

someFunc :: IO ()
someFunc = do
    content <- readFile "6.input"
    let groups = splitOn (pack "\n\n") (pack content)
    let sets = [delete '\n' (fromList (unpack group)) | group <- groups]
    let result4a = sum [ size set | set <- sets ]
    putStrLn (show result4a)
    let allAnswers = [splitOn (pack "\n") group | group <- groups ]
    let allAnswersString = [[ unpack personAnswer | personAnswer <- groupAnswers] | groupAnswers <- allAnswers ]
    let everyoneYes = [intersectAll answers | answers <- allAnswersString]
    let result4b = sum [ length item | item <- everyoneYes]
    putStrLn (show result4b)
