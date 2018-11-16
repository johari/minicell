module Spreadsheet.Example exposing (..)

import Spreadsheet.Types exposing (..)
import Spreadsheet.Wrangling.Types exposing (WranglingMode(..))
import Spreadsheet.Wrangling.AdjacencyMatrix as AM
import Spreadsheet.Wrangling.ListOfVertexTuples as LOVT
import Spreadsheet.Wrangling.Ranking exposing (..)
import Examples.TopoSort exposing (dressUp)
import Dict
import Graph exposing (nodes, edges)
import Set

el : List Cell

el = [ formulaCell (0, 0) "exampleGraph" [ESLit "self"]
     , intCell (0, 1) 42
     , formulaCell (0, 2) "+" [(ECellRef (0, 1)), (ECellRef (0, 0))]
     , formulaCell (1, 0) "secondsSinceEpoch" []
     , formulaCell (2, 0) "+" [(ECellRef (1, 0)), (ECellRef (0, 0))]
     , graphCell (3, 3) dressUp
     , graphCell (4, 3) dressUp
     , stringCell (5, 5) "Hello"
     , stringCell (6, 5) "world!"
     , stringCell (6, 6) "Hello"
     , stringCell (7, 6) "@mysql://nima.wiki/phd/writing_cardbased"
     ]

theCities = 
     [ stringCell (0, 1) "A", stringCell (1, 0) "A"
     , stringCell (0, 2) "B", stringCell (2, 0) "B"
     , stringCell (0, 3) "C", stringCell (3, 0) "C"
     , stringCell (0, 4) "D", stringCell (4, 0) "D" 
     , stringCell (0, 5) "E", stringCell (5, 0) "E" 
     , stringCell (0, 6) "F", stringCell (6, 0) "F" 
     
     , intCell (1, 2) 1
     , intCell (1, 5) 7
     , intCell (1, 6) 6
     , intCell (2, 3) 2
     , intCell (2, 4) 9
     , intCell (2, 5) 8
     , intCell (3, 4) 3
     , intCell (4, 5) 4
     , intCell (5, 6) 5
     ]

twitterExample = 
     [ stringCell (0, 0) "id1", stringCell (0, 1) "tweet1"
     , stringCell (1, 0) "id2", stringCell (1, 1) "tweet2", stringCell (1, 2) "id1"
     ]

exampleSpreadsheet =
    { emptySpreadsheet | database = theCities }

wranglers database = [
     (\_ -> (AM.canonicalMatrixWrangler database))
     ]

exampleSpreadsheetWithGraph =
     let
          database = exampleSpreadsheet.database
          situation = { database = database, demos = emptyDemonstration }
          bestWrangler = pickBestWithRespectTo situation (wranglers database)
          formula = (bestWrangler database |> ESuperFancyGraph)
     in
          { exampleSpreadsheet |
               database = exampleSpreadsheet.database ++ [{ emptyCell | value = formula, addr = (10, 0)}]
          }

score x = 1 -- FIXME

exampleSpreadsheetAdjacencyListWithGraph = 
     let extractedGraphCell =
          (LOVT.databaseToCandidateGraphs twitterExample [] []) 
          |> List.map (\x -> x) -- (\x -> (score x) |> List.sort
          |> List.head |> Maybe.withDefault (Graph.fromNodesAndEdges [] [])
          |> ESuperFancyGraph
     in
          { emptySpreadsheet | database = twitterExample ++ [ { emptyCell | value = extractedGraphCell, addr = (10, 0)}]}

