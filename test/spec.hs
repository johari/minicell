{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Text.ParserCombinators.Parsec

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)


import Data.Graph.Inductive.Query.SP
import Data.Graph.Inductive.Basic (grev)
import Data.Graph.Inductive.Dot

import ExampleGraphs

import Database.MySQL.Simple
import System.Environment

import Data.List

import Spreadsheet.Evaluator.Parser
import Spreadsheet.Types

main = hspec $ do
  describe "Minicell" $ do

    describe "Formula" $ do
      describe "Parser" $ do
        it "can parse integer literals" $ do
          (parse cellContent "" "42") `shouldBe` (Right $ EILit 42)
          (parse cellContent "" "-5") `shouldBe` (Right $ EILit (-5))
        
        it "can parse string literals" $ do
          (parse cellContent "" "Hello World!") `shouldBe` (Right $ ESLit "Hello World!")

        it "can parse reference to cells" $ do
          (parse cellContent "" "=B2") `shouldBe` (Right $ ECellRef (1, 1))
          (parse cellContent "" "=A3") `shouldBe` (Right $ ECellRef (2, 0))

        it "can parse functions that as an argument take a reference to a cell" $ do
          (parse cellContent "" "=OP(A1, 42)") `shouldBe` (Right $ EApp "OP" [ECellRef (0, 0), EILit 42])

      describe "Evaluator" $ do
        it "can resolve references" $ do
          let fourtyTwoDatabase = [ (emptyCell { value = EILit 42, addr = (0, 0) }) 
                                  , (emptyCell { value = EILit 43, addr = (1, 0) })
                                  ]
              fourtyTwoSpreadsheet = emptySpreadsheet { database = fourtyTwoDatabase }

main_hspec = hspec $ do
  describe "MiniCell" $ do

    describe "GraphFill" $ do
      it "can find the hosrtest path of the trivial graph"

        pending

      it "can find the hosrtest path of K_2" $ do

        pendingWith "need graph support in cells first"

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