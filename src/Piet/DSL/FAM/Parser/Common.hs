module Piet.DSL.FAM.Parser.Common where

import Control.Monad
import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.String


-- This little helper function helps us augment the AST with the
-- precise location in the original markdown.

sourcePos :: Monad m => ParsecT s u m SourcePos
sourcePos = statePos `liftM` getParserState
