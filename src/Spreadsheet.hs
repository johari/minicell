module Spreadsheet where

import Spreadsheet.Types

import Data.List

modifyModelWithNewCellValue :: CellAddress -> EExpr -> Spreadsheet -> Spreadsheet
modifyModelWithNewCellValue targetAddr newValue model = 
    case find p (database model) of
        Just cell -> model { database = map (\x -> if p x then f x else x) (database model) }
        Nothing -> model { database = (database model) ++ [ emptyCell { value = newValue, addr = targetAddr }] }
    where
        p x = addr x == targetAddr
        f x = x { value = newValue }