{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.Graph.Inductive.Query.SP

import ExampleGraphs

import Database.MySQL.Simple
import System.Environment

import Data.List

writeCardToFile fileName contents = do
  writeFile (mconcat ["/tmp/phd/", fileName, ".tex"]) contents

main :: IO ()
main = do
  pass <- getEnv "MINICELL_PASS"
  conn <- connect defaultConnectInfo { connectHost = "127.0.0.1", connectUser = "root", connectPassword = pass, connectPort = 3307, connectDatabase = "phd" }
  -- (el :: [(Maybe Int, Maybe String)]) <- query_ conn "select (card_id, card_markdown) from writing_card_based where manuscript = 2 ORDER BY CHAR_LENGTH(card_markdown) DESC"
  (el :: [(Maybe Int, Maybe String)]) <- query conn "select card_id, card_markdown from writing_card_based where card_manuscript_id = 2 ORDER BY CHAR_LENGTH(card_markdown)" ()
  
  sequence_ $ (\(Just a, Just b) -> writeCardToFile (show a) b) <$> el
  -- main_hspec

main_hspec = hspec $ do
  describe "MiniCell" $ do

    describe "GraphFill" $ do
      it "can find the hosrtest path of the trivial graph"

        pending

      it "can find the hosrtest path of K_2"

        pending

      it "can fill the blank cells in the T1 as following (T2)" $ do
        -- ╔═════╤═════╤═════╤═════╗
        -- ║     │ v_0 │ v_1 │ v_2 ║
        -- ╠═════╪═════╪═════╪═════╣
        -- ║ v_0 │ 0   │ 15  │ x   ║
        -- ╟─────┼─────┼─────┼─────╢
        -- ║ v_1 │ 5   │ 0   │ 5   ║
        -- ╟─────┼─────┼─────┼─────╢
        -- ║ v_2 │ y   │ 10  │ 0   ║
        -- ╚═════╧═════╧═════╧═════╝
        -- T1


        -- ╒═════╤═════╤═════╕
        -- │     │ v_0 │ v_2 │
        -- ╞═════╪═════╪═════╡
        -- │ v_0 │     │ 20  │
        -- ├─────┼─────┼─────┤
        -- │ v_2 │ 15  │     │
        -- ╘═════╧═════╧═════╛
        -- T2

        let Just y = spLength 3 1 mygraph3
        let Just x  = spLength 1 3 mygraph3
        x `shouldBe` 20
        y `shouldBe` 15

      it "recomputes the value of the formula, if the weights on the graph change"

        -- construct a data type that represent the original spreadsheet (S1)

        -- execute commands that would consturcts (S2), which agrees with S1 everywhere except a few edges

        -- probe the shortest path between A and C in the new graph (S2) and check if has changed

        pending

      describe "max-flow" $ do
        return ()

      describe "neighbors" $ do
        it "given a graph (G) and a vertex (V), it can construct a new graph where the vertices are the neighbors of (V)" $ do

          pending

      describe "VISUAL RECONCILATION" $ do
        it "can highlight the shortest path on the diagram of the graph" $ do
          pending


        it "highlights the corresponding row and column when the mouse is hovered over the vertex in the graphical representation" $ do
          pending


  -- describe "Prelude.head" $ do
  --   it "returns the first element of a list" $ do
  --     head [23 ..] `shouldBe` (23 :: Int)

  --   it "returns the first element of an *arbitrary* list" $
  --     property $ \x xs -> head (x:xs) == (x :: Int)

  --   it "throws an exception if used with an empty list" $ do
  --     evaluate (head []) `shouldThrow` anyException

  -- describe "read" $ do
  --   it "is inverse to show" $ property $
  --     \x -> (read . show) x == (x :: Int)