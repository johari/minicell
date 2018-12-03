module Minicell.FUSE.RouteParser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Char (string)
import Data.Char (digitToInt)

import Minicell.Types

excelStyleAddr :: GenParser Char st Addr
excelStyleAddr =
  do
    column <- letter
    row <- many1 digit
    return $ Addr ((read row) - 1) ((digitToInt column) - 10)

addrToExcelStyle addr = 
    let columnString = [(['A'..] !! (column addr))] in
    mconcat [columnString, show (row addr)]

addrToPath addr =
    let columnString = [(['A'..] !! (column addr))] in
    mconcat [columnString, show (row addr), ".json" ]

pathToAddr :: GenParser Char st Addr
pathToAddr =
    do
        string "/"
        addr <- excelStyleAddr
        string ".json"
        return $ addr