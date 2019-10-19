{-# Language OverloadedStrings #-}

module Piet.DSL.FAM.Markup.Pure where

import Piet.DSL.FAM.Parser.Inline
import Piet.DSL.FAM.Parser.Singleton

import Piet.DSL.FAM.Syntax

import Text.Blaze
import Text.Blaze.Html
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Text.Parsec (SourcePos)
import Text.Parsec.Pos (sourceColumn, sourceLine, sourceName)

import Data.String
import Data.Monoid

import Data.Char        (toLower)

import Data.List (intersperse)

slugChars :: [Char]
slugChars = ['a'..'z'] ++ ['0'..'9'] ++ ['-']

slugify :: String -> String
slugify = (mkSlug True)
  where
    mkSlug _ [] = []
    mkSlug replace (x:xs)
      | toLower x `elem` slugChars = toLower x:mkSlug True xs
      | otherwise                  = (if replace then ['-'] else []) ++ mkSlug False xs

instance ToMarkup Box where
  toMarkup Checked = fromString "x"
  toMarkup Unchecked = fromString " "
  toMarkup Pending = fromString "."
  toMarkup Faulty = fromString "!"
  toMarkup (Arb s) = H.code $ fromString s

instance ToMarkup (SourcePos) where
  --toMarkup s = H.a ! A.href (fromString href) $ fromString (show s)
  toMarkup s = H.a ! A.href (fromString href) $ "[edit]"
    where
          href = mconcat [ "subl://open?url=file://", sourceName s
                         , "&line=", show $ sourceLine s
                         , "&column=", show $ sourceColumn s
                         ]

instance (Show a, ToMarkup a) => ToMarkup (Inline a) where
  toMarkup (MathFormula a m) = H.code $ fromString m
  toMarkup (HyperLink a title url) = H.a ! A.href (fromString url) $ fromString title
  toMarkup (WikiLink a pageName) = H.a ! A.href (fromString $ "https://nima.wiki/" <> pageName) ! A.class_ "wiki-link" $ fromString pageName
  toMarkup (Hashtag a str) = H.code $ H.a $ fromString str
  toMarkup (CellJumpLink a str) = H.code $ H.a ! A.href (fromString $ "#" <> str)$ fromString str

  toMarkup (Command a "kbd" [keyCombination]) = H.kbd $ fromString keyCombination

  toMarkup (Command a "skip" _) = fromString ""

  toMarkup (Command a "raw" [content]) = preEscapedToHtml $ content

  toMarkup (Command a "cursor" _) = H.span ! A.class_ "cursor" $ "|"

  toMarkup (Command a cmd args) = do
    (H.table ! A.class_ (fromString $ "table-command xx-" <> cmd) $ (mconcat [
        H.tr $ H.td $ toMarkup a -- position information
      , H.tr $ H.td $ fromString cmd -- command (like question, time, daysAgo etc.)
      , H.tr $ H.td $ H.pre $ fromString (mconcat args) -- arguments, typically the last argument is the body of the backtick
      ]))

  --toMarkup (Command a cmd args) = fromString $  mconcat ["<?", cmd, ">", mconcat args , "</", cmd, ">"]


  toMarkup (Unformatted _ s) = fromString s

  toMarkup (Typewriter a str) = H.code ! A.class_ "tt"  $ fromString str


instance (Show a, ToMarkup a) => ToMarkup (BoxItem a) where
  toMarkup (BoxItem a _ x listofInlines) =
    case x of
      Faulty -> (H.div ! A.class_ "faulty" $ theBox <> (pp))
      Checked -> (H.div ! A.class_ "checked" $ theBox <> (H.del $ pp))
      Pending -> (H.div ! A.class_ "pending" $ theBox <> pp)
      _       -> (H.div $ theBox <> pp)
    where pp = toHtml $ mconcat $ toMarkup <$> listofInlines
          theBox = H.code $ (fromString "[") <> (toMarkup x) <> (fromString "] ")


instance (Show a, ToMarkup a) => ToMarkup (Singleton a) where
  toMarkup (Heading a level p) = do
    -- let content = mconcat (toMarkup <$> p) in
    ([H.h1, H.h2, H.h3] !! level) ! A.id (fromString $ slugify p) $ fromString p
              --mconcat ["<box=", show x, ">", toMarkupP p , "</box>"]

  toMarkup (HRule a _) = H.hr

-- fake
instance (Show a, ToMarkup a) => ToMarkup (Document a) where
  toMarkup (Document _ l) =  mconcat $ toMarkup <$> l

instance (Show a, ToMarkup a) => ToMarkup (Paragraph a) where
  toMarkup (ParagraphInline _ l) = mconcat (intersperse H.br $ H.span <$> toMarkup <$> l)
  toMarkup (ParagraphSingleton _ l) = toMarkup l

--instance (Show a, ToMarkup a) => ToMarkup [Paragraph a] where
--  --toMarkup l = mconcat $ intersperse (fromString " ") $ mconcat $ [H.span <$> toMarkup <$> x | x <- l]
--  toMarkup l = mconcat $ intersperse (fromString " ") $ toMarkUpmconcat $ [H.span <$> toMarkup <$> x | x <- l]