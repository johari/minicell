module Piet.DSL.FAM.Parser.Singleton where

import Piet.DSL.FAM.Parser.Common
import Piet.DSL.FAM.Parser.Inline

import Piet.DSL.FAM.Syntax

import Text.Parsec
import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.String

import Control.Monad

import Debug.Trace

println = traceShowM

singletonParagraph :: Parser (Paragraph SourcePos)
singletonParagraph = do
    pos <- sourcePos
    s <- choice [boxList, hrule, heading, emptyLine]

    println $ "Parsed a singleton!\n" ++ (show s)

    return $ ParagraphSingleton pos s

emptyLine :: Parser (Singleton SourcePos)
emptyLine = do
    pos <- sourcePos
    when (sourceColumn pos /= 1) parserZero -- ensure we are at the begining of the line :)
    try $ do
        newline
        skipMany newline
    return $ EmptyLine pos

-- 2. heading
heading :: Parser (Singleton SourcePos)
heading = do
    pos <- sourcePos
    -- when (sourceColumn pos /= 1) parserZero -- here too :)
    howmany <- many1 (char '#')
    pos <- sourcePos
    optional $ char ' '
    -- ParagraphInline _ p <- inlineParagraph
    p <- manyTill anyChar newline
    newline
    return $ Heading pos ((length howmany) - 1) p

hrule :: Parser (Singleton SourcePos)
hrule = do
    pos <- sourcePos
    s <- try $ do
        string "#"
        many (char ' ')
        manyTill (oneOf "=-") newline

    newline
    return $ HRule pos s


boxList :: Parser (Singleton SourcePos)
boxList = do
    pos <- sourcePos
    boxItems <- (sepEndBy1 boxItem newline)

    return (BoxList pos boxItems)

boxItem :: Parser (BoxItem SourcePos)
boxItem = do
    pos <- sourcePos
    when (sourceColumn pos /= 1) parserZero -- ensure we are at the begining of the line :)
    (indentationLevel, content) <- try $ do
                -- we are doing this for nested lists. a cheap hack.
                indentationMarker <- many (char ' ')
                -- instead of skipping, we can count the number of spaces for level of indentation

                char '['
                s <- oneOf " .-+xX*/0ط!"
                char ']'
                return (length indentationMarker, s)

    many (char ' ')
    ParagraphInline inlinePos inlineContent <- inlineParagraph

    let ret = case content of
                'ط' -> Checked
                'x' -> Checked
                ' ' -> Unchecked
                '.' -> Pending
                '!' -> Faulty
                (_) -> Arb [content]

    return $ BoxItem pos indentationLevel ret inlineContent