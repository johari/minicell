module ExampleGraphs where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree (Gr)


genLNodes1 :: [LNode String]
genLNodes1 = [(0, "v_0")]

genLEdges1 :: [LEdge Int]
genLEdges1 = []

mygraph1 :: Gr String Int
mygraph1 = mkGraph genLNodes1 genLEdges1

genLNodes2 :: [LNode String]
genLNodes2 = zip [1..2] ["A","B"]

genLEdges2 :: [LEdge Int]
genLEdges2 = [(1,2,10),
             (2,1,20)]

mygraph2 :: Gr String Int
mygraph2 = mkGraph genLNodes2 genLEdges2

genLNodes3 :: [LNode String]
genLNodes3 = zip [1..3] ["A","B","C"]

genLEdges3 :: [LEdge Int]
genLEdges3 = [(1,2,15),
             (2,1,5), (2, 3, 5),
             (3, 2, 10)
             ]

mygraph3 :: Gr String Int
mygraph3 = mkGraph genLNodes3 genLEdges3
