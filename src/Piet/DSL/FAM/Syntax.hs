{-# Language DeriveDataTypeable #-}
module Piet.DSL.FAM.Syntax where

import Data.Data
import Text.Parsec

-- This type holds meta information about the document
-- We don't need it much
data Meta = MetaData { documentTitle :: String }

-- A document is a list of paragraphs
data Document a = Document a [Paragraph a] deriving (Show, Data)

-- A paragraph is a list of Inline items ending with an EMPTY LINE (a "\n" that is preceeded by a "\n")
data Paragraph a = ParagraphInline a [Inline a] | ParagraphSingleton a (Singleton a) deriving (Show, Data)

-- A box item is the Box itself along with an accompanying "paragraph"
data BoxItem a = BoxItem a Int Box [Inline a] deriving (Show, Data)
-- The type Box specifies what can come inbetween the [ ]s
data Box = Checked | Unchecked | Pending | Faulty | Arb String deriving (Show, Eq, Data)

data Singleton a = Heading a Int String
                 | BoxList a [BoxItem a]
                 -- A box list is conveniently a list of boxes
                 | HRule a String
                 | EmptyLine a
                 deriving (Show, Data)

data Inline a = Unformatted a String
              | HyperLink a String String
              | Command a String [String]
              | MathFormula a String
              | Typewriter a String
              | Hashtag a String
              | CellJumpLink a String
              | Image a String -- where String holds the "src"
              | WikiLink a String
              deriving (Show, Data)


data FamDoc = FamDoc Meta (Document SourcePos)
