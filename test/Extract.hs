import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "G.E. (Graph Extract)" $ do
    describe "Incidence matrix" $ do
      -- ╔═══╤═══════╤═══════╤══════╤═══════╗
      -- ║   │ A     │ B     │ C    │ D     ║
      -- ╠═══╪═══════╪═══════╪══════╪═══════╣
      -- ║ 1 │       │ alpha │ beta │ gamma ║
      -- ╟───┼───────┼───────┼──────┼───────╢
      -- ║ 2 │ alpha │ 0     │ 15   │       ║
      -- ╟───┼───────┼───────┼──────┼───────╢
      -- ║ 3 │ beta  │ 5     │ 0    │ 5     ║
      -- ╟───┼───────┼───────┼──────┼───────╢
      -- ║ 4 │ gamma │       │ 10   │ 0     ║
      -- ╚═══╧═══════╧═══════╧══════╧═══════╝
      -- T2
      it "extracts a graph from the T2" $ do
        pending

    describe "Adjacency list" $ do
      -- ╔═══╤══════╤══════════════════════╗
      -- ║   │ A    │ B                    ║
      -- ╠═══╪══════╪══════════════════════╣
      -- ║ 1 │ url  │ tags                 ║
      -- ╟───┼──────┼──────────────────────╢
      -- ║ 2 │ url1 │ (tag1) (tag2) (tag3) ║
      -- ╟───┼──────┼──────────────────────╢
      -- ║ 3 │ url2 │ (tag2) (tag3)        ║
      -- ╟───┼──────┼──────────────────────╢
      -- ║ 4 │ url3 │ (tag4)               ║
      -- ╚═══╧══════╧══════════════════════╝
      -- T1

      it "can generalize limited demonstrations (on the sheet) to full graphs" $ do
        -- Given table T and two or three highlights in rows 1 and 2
        --   generalize a graph with vertices as {url1, url2, url3} U {tag1, tag2, tag3}
        --   and edges {(url1, tag1), (url1, tag2), (url1, tag3), ..., ..., (url3, tag4)}
    
        -- let demonstrations = [ switchColor "yellow"
        --                      , entireCell $ "A" 2
        --                      , switchColor "magenta"
        --                      , withinCell $ "B" 1 $ "tag1"
        --                      , withinCell $ "B" 1 $ "tag2"
        --                      , withinCell $ "B" 1 $ "tag3"
        --                      ]
        
        -- 
        -- 
        --   MAGIC HAPPENS [HERE]
        -- 
        -- 


        -- generalize demonstrations t1

        -- yellow@{url1, url2, url3} V
        -- magenta@{tag1, tag2, tag3} V
        -- assert V = (yellow) U (magenta)

        -- ASSERT
        -- vertices of ROW 2 are {url1} and {tag1, tag2, tag3}
        --                 3 are {url2} and {tag2, tag3}
        --                 4 are {url3} and {tag4}

        -- ASSERT
        -- between every Y[ellow] node in row (R), there is an edge going to all M[agenta] nodes in the same row (R)

        pending