module Piet.DSL.FAM.Parser where


import Piet.DSL.FAM.Parser.Common

import Control.Monad -- Tracking source position
import Text.Parsec -- Tracking source position
import Text.Parsec.Prim -- Tracking source position
import Text.Parsec.String -- Tracking source position


import Piet.DSL.FAM.Parser.Inline
import Piet.DSL.FAM.Parser.Singleton

import Piet.DSL.FAM.Syntax

import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.String

import Debug.Trace
import Data.Functor.Identity

import Data.Monoid
import Data.List
import Data.List.Split (splitOn)

import Control.Monad

document :: Parser (Document SourcePos)
document = do
    pos <- sourcePos
    paragraphs <- many1 paragraph
    eof
    return $ Document pos paragraphs

paragraph :: Parser (Paragraph SourcePos)
paragraph = do
    pos <- sourcePos
    s <- singletonParagraph <|> (do { r <- inlineParagraph; newline; return r})

    -- previous new lines are captured by singletonParagraph (through EmptyLine singleton)
    return $ s