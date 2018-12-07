module Spreadsheet.Wrangling.ListOfVertexTuples where

database = [(0,0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2)]

demoVertices = [(0, 0), (0, 2)]

expectedVertices = [(0, 0), (0, 2), (1, 0), (1, 2)]

-- edgeWrangler vertices = 

-- poolOfWranglers = []

main = do
  putStrLn "> [vertex] Loading up the example spreadsheet"
  print database

  putStrLn "> [vertex] Generating the pool of wranglers"
  putStrLn "> [vertex] Loading examples from the user"
  putStrLn "> [vertex] Picking the best vertex wranglers"
  
  putStrLn "> [edge] Loading wrangled vertices"
  putStrLn "> [edge] Generating the pool of wranglers"
  putStrLn "> [edge] Loading examples from the user"
  putStrLn "> [edge] Pick the best wrangler with respect to users demonstration"
