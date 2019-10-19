module Piet.DSL.FAM.Parser.Inline where

import Piet.DSL.FAM.Parser.Common
import Piet.DSL.FAM.Syntax

import Text.Parsec
import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.String

import Data.Monoid
import Data.List.Split (splitOn)

import Debug.Trace

-- println2 = traceShowM

inlines = [ command
          , tripleBacktick
          , mathFormula
          , typewriter
          , typewriter'

          , wikiLink
          , hyperLink
          , hyperLinkRaw

          , hashtag
          , cellJumpLink
          , unformatted ]

inlineParagraph :: Parser (Paragraph SourcePos)
inlineParagraph = do
    -- choice over the rest
    pos <- sourcePos
    -- println2 $ show pos
    s <- sepEndBy1 (choice inlines) (many $ char ' ')
    (lookAhead newline)
    return $ ParagraphInline pos s


command :: Parser (Inline SourcePos)
command = do
    pos <- sourcePos
    str <- between (string "{{") (string "}}") (manyTill anyChar (lookAhead $ string "}}"))
    let cmd:args = splitOn "|" str
    return $ Command pos cmd args

tripleBacktick :: Parser (Inline SourcePos)
tripleBacktick = do
    (try $ string "```")
    pos <- sourcePos
    str <- manyTill anyChar newline
    argn <- manyTill anyChar (try $ string "```")

    let cmd:args = splitOn "|" str
    return $ Command pos cmd (args ++ [argn])

mathFormula :: Parser (Inline SourcePos)
mathFormula = do
    pos <- sourcePos
    formula <- between (string "$") (string "$") (manyTill anyChar (lookAhead $ string "$"))
    return $ MathFormula pos formula

typewriter :: Parser (Inline SourcePos)
typewriter = do
    pos <- sourcePos
    str <- between (string "`") (string "`") (manyTill anyChar (lookAhead $ string "`"))
    return $ Typewriter pos str

typewriter' :: Parser (Inline SourcePos)
typewriter' = do
    pos <- sourcePos
    string ">"
    many (char ' ')
    str <- manyTill anyChar newline
    return $ Typewriter pos str

hyperLink :: Parser (Inline SourcePos)
hyperLink = do
    pos <- sourcePos
    url <- between (string "[") (string "]") (manyTill anyChar (lookAhead $ string "]"))
    case splitOn " " url of
      [untitledUrl] -> return $ HyperLink pos url url
      (titledUrl:_) -> return $ HyperLink pos (drop (length titledUrl + 1) url) titledUrl

wikiLink :: Parser (Inline SourcePos)
wikiLink = do
    pos <- sourcePos
    str <- try $ between (string "[[") (string "]]") (manyTill anyChar (lookAhead $ string "]]"))
    return $ WikiLink pos str

hyperLinkRaw :: Parser (Inline SourcePos)
hyperLinkRaw = do
  pos <- sourcePos
  schema <- try $ do
    string "https://"
  restOfUrl <- manyTill anyChar (lookAhead $ oneOf " \n)")
  let url = schema <> restOfUrl

  return $ HyperLink pos url url

hashtag :: Parser (Inline SourcePos)
hashtag = do
    try $ string "#"
    pos <- sourcePos
    ht <- many1 (alphaNum <|> oneOf "-.")

    return $ Hashtag pos ht

cellJumpLink :: Parser (Inline SourcePos)
cellJumpLink = do
    try $ string "="
    pos <- sourcePos
    ht <- many1 (alphaNum <|> oneOf "-.")

    return $ CellJumpLink pos ht

-- seeNext :: String -> Int -> ParsecT String u Identity ()
-- seeNext from n = do
--   println (">>> " <> from)
--   s <- getParserState
--   let out = take n (stateInput s)
--   println out

unformatted :: Parser (Inline SourcePos)
unformatted = do
    pos <- sourcePos
    --try $ char '#'
    sss <- option "" (many1 $ char ' ')
    s <- (many1 $ alphaNum <|> (oneOf ".(),?!+-'\":_/;"))
    ss <- option "" (many1 $ char ' ')
    return $ Unformatted pos (sss <> s <> ss)
    -- return $ Unformatted pos s
