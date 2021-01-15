module Lib
    ( someFunc
    ) where

import Text.Regex.PCRE
    ( AllTextSubmatches(getAllTextSubmatches), (=~) )
import Text.Regex.PCRE.String ()
import Data.Text ( pack, unpack, splitOn )
import Data.Graph ( Graph, Vertex, graphFromEdges, vertices, path )

parseContent :: [Char] -> [[String]]
parseContent content = do
    let lines = [unpack line | line <- splitOn (pack "\n") (pack content)]
    let lineRegex = "^(\\w+ \\w+) bags contain (?:(\\d) (\\w+ \\w+))?(?: bag[s]?, (\\d) (\\w+ \\w+))?(?: bag[s]?, (\\d) (\\w+ \\w+))?(?: bag[s]?, (\\d) (\\w+ \\w+))?(?: bag[s]?, (\\d) (\\w+ \\w+))?"
    [getAllTextSubmatches (line =~ lineRegex) :: [String] | line <- lines]

getNodeData :: [[Char]] -> Int -> ([[Char]], [[Char]]) -> ([[Char]], [[Char]])
getNodeData bagData index nodeData
    | index == 0 = error "Cannot call getNodeData with 0 index"
    | bagData !! index == "" = nodeData
    | index == 1 = getNodeData bagData (index + 1) (newFstValue, snd nodeData)
    | odd index = getNodeData bagData (index + 1) (fst nodeData, newSndValue)
    | even index = getNodeData bagData (index + 1) (newFstValue, snd nodeData)
    where 
        currentBagValue = bagData !! index
        newFstValue = fst nodeData ++ [currentBagValue]
        newSndValue = snd nodeData ++ [currentBagValue]

makeGraphEdge :: ([a], [a]) -> ([a], a, [a])
makeGraphEdge nodeData = (fst nodeData, head (fst nodeData), snd nodeData)

countVerticesOnPathTo :: Num t => Data.Graph.Graph -> Data.Graph.Vertex -> Int -> t -> t
countVerticesOnPathTo graph dst actualVertex count
    | length (vertices graph) - 1 == actualVertex = count
    | actualVertex == dst = countVerticesOnPathTo graph dst (actualVertex + 1) count
    | path graph actualVertex dst = countVerticesOnPathTo graph dst (actualVertex + 1) (count + 1)
    | otherwise = countVerticesOnPathTo graph dst (actualVertex + 1) count

findNodeByVertexName :: ([Char] -> Maybe Vertex) -> (Vertex -> ([[Char]], [Char], [[Char]])) -> [Char] -> ([[Char]], [Char], [[Char]])
findNodeByVertexName vertexFromKey nodeFromVertex vertexName = 
    nodeFromVertex vertex
    where
        (Just vertex) = vertexFromKey vertexName

countAllBagsFromNode :: ([Char] -> Maybe Vertex) -> (Vertex -> ([[Char]], [Char], [[Char]])) -> ([String], b, [[Char]]) -> Int
countAllBagsFromNode _ _ (_, _, []) = 0
countAllBagsFromNode vertexFromKey nodeFromVertex node = do
    let (nodeSpec, _, adjectantKeys) = node
    let innerBagsNum = [read s::Int | s <- tail nodeSpec]
    let innerBags = zip innerBagsNum adjectantKeys
    let innerBagCount = sum [count * countAllBags vertexFromKey nodeFromVertex bagName | (count, bagName) <- innerBags]
    let directBagCount = sum innerBagsNum
    directBagCount + innerBagCount

countAllBags :: ([Char] -> Maybe Vertex) -> (Vertex -> ([[Char]], [Char], [[Char]])) -> [Char] -> Int
countAllBags vertexFromKey nodeFromVertex vertexName = do
    let node = findNodeByVertexName vertexFromKey nodeFromVertex vertexName
    countAllBagsFromNode vertexFromKey nodeFromVertex node

someFunc :: IO ()
someFunc = do
    content <- readFile "7.input"
    let bagsData = parseContent content
    let graphEdges = [makeGraphEdge (getNodeData bagData 1 ([], [])) | bagData <- bagsData]
    let (graph, nodeFromVertex, vertexFromKey) = graphFromEdges graphEdges
    let (Just shinyGoldVertex) = vertexFromKey "shiny gold"
    print (countVerticesOnPathTo graph shinyGoldVertex 0 0)
    let allBagsCount = countAllBags vertexFromKey nodeFromVertex "shiny gold"
    print allBagsCount
