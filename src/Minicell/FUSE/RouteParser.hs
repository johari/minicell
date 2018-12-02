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

cellAddressPath :: GenParser Char st Addr
cellAddressPath =
    do
        string "/"
        addr <- excelStyleAddr
        string ".cell"
        return $ addr