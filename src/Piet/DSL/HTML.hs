
{-# LANGUAGE Arrows #-}

module Piet.DSL.HTML where


-----------

import System.Log.Logger (Priority (DEBUG), debugM, infoM, setLevel,
                          updateGlobalLogger, warningM, noticeM,
                          rootLoggerName,
                          setHandlers
                          )
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter

-----------

import Data.List (nub,sort,isPrefixOf,transpose,groupBy)
import qualified Data.Map

import Text.XML.HXT.Core
import Text.XML.HXT.Arrow.WriteDocument
import qualified Text.XML.HXT.DOM.XmlNode as XN

-- Read this for basic usage of HXT
-- http://adit.io/posts/2012-04-14-working_with_HTML_in_haskell.html#arrow-interlude-%231:-hxt-arrows

import Spreadsheet.Types

-- Parsec stuff
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char

rootTemplateOf html = do
  let doc = readString [withParseHTML yes, withWarnings no] html

  -- shouldFetchSource2 <- runX ( doc >>> writeDocumentToString [])
  -- putStrLn $ head shouldFetchSource2

  tplSourcePathL <- runX ( doc //> isPietExtend >>> hasAttr "src" >>> getAttrValue "src" )

  case tplSourcePathL of
    (x:_) -> return $ Just x
    _     -> return $ Nothing


retrievePietBlocks doc = do
  blocks <- runX $ doc //> hasName "piet-block" >>> (
    proc arr -> do
      idOfBlock       <- hasAttr "id" >>> getAttrValue "id" -< arr
      contentOfBlock  <- writeDocumentToString [] -< arr
      returnA -< (idOfBlock, contentOfBlock)
    )
  return blocks

eval' :: (Spreadsheet -> Formula -> IO Formula) -> Spreadsheet -> Formula -> IO Formula
eval' eval model expr = case normalizeOp expr of
  EApp "TPL" [ elPath ] -> do

    ESLit myPath <- eval model elPath
    html <- readFile myPath
    let doc = readString [withParseHTML yes, withWarnings no] html
    maybeRootTemplate <- rootTemplateOf html
    case maybeRootTemplate  of
      Just root -> do
        -- Retrieve the source document
        let basePath = "/tmp/glitch/poppet/views/" -- FIXME

        -- Find all block definitions in the derived document
        pietBlocks <- retrievePietBlocks doc
        debugM "wiki.sheets.html" (mconcat ["Current template (", myPath, ") has the following blocks:"])
        sequence ((debugM "wiki.sheets.html") <$> show <$> pietBlocks)

        debugM "wiki.sheets.html" (mconcat ["The source (", root, ") before substitution looks like this:"])
        ESLit sourceDoc <- eval' eval model (EApp "TPL" [ ESLit $ basePath <> root ])
        let sourceDoc' = readString [withParseHTML yes, withWarnings no] sourceDoc

        debugM "wiki.sheets.html" (mconcat ["The source (", root, ") after substitution looks like this:"])

        let myMapping = Data.Map.fromList [] :: Data.Map.Map String XmlTree
        let processedDoc = sourceDoc' >>> substPietBlocks myMapping >>> writeDocumentToString []
        res <- runX processedDoc
        debugM "wiki.sheets.html" (head res)

        -- [ ] Handle block substitutions
        return $ ESLit (mconcat ["FIXME: root template is ", root])

      Nothing -> do
        -- [ ] Handle includes
        -- [ ] Handle for loops

        let doc2 = doc >>> addRefIcon >>> writeDocumentToString []
        res <- runX doc2

        debugM "wiki.sheets.html" (mconcat ["Template ", myPath, " has no roots!"])
        debugM "wiki.sheets.html" (mconcat res)

        return $ ESLit (mconcat res)

  {-
  EApp "BOLD" [ elE ] -> do
      el <- eval model elE
      case el of
        EString elList -> return $ EString "<b></b>"
        _ -> return $ ENotImplemented
  -}

  _ -> return ENotImplemented


-- substPietBlocks :: ArrowXml a => Data.Map.Map String XmlTree -> a XmlTree XmlTree
substPietBlocks dict = processTopDown (replaceAccordingToDict dict `when` isPietBlockWithId)


-- replaceAccordingToDict _ = getAttrValue "id" >>> mkText -- This works
replaceAccordingToDict _ = myImgElem "foo"


myReplacement2 elemId = mkelem "img" [ sattr "src" (mconcat ["/icons/", elemId, ".png" ]) ] []


myImgElem elemId = do
  mkelem "img" [ sattr "src" (mconcat ["/icons/", elemId, ".png" ]) ] []

-- myReplacement' = proc arr -> do
--   elemId <- getAttrValue "id" -< arr
--   returnA -< mkelem "img" [ sattr "src" (mconcat ["/icons/", elemId, "ref.png" ]) ] []

isPietExtend = isElem >>> hasName "piet-extend"
isPietBlockWithId = isElem >>> hasName "piet-block" >>> hasAttr "id"




{-
mkAbsHRefs  :: ArrowXml a => String -> a XmlTree XmlTree
mkAbsHRefs base = processTopDown editHRef

editHRef = processAttrl ( changeAttrValue (absHRef base) `when` hasName "href" ) `when` ( isElem >>> hasName "a" )

absHRef :: String -> String -> String   -- (5)
absHRef base url = fromMaybe url . expandURIString url $ base
-}


-- addRefIcon  :: ArrowXml a => a XmlTree XmlTree
addRefIcon = processTopDown (addImg `when` isExternalRef)

isExternalRef = isElem >>> hasName "a" >>> hasAttr "href" >>> getAttrValue "href" >>> isA isExtRef

isExtRef = isPrefixOf "http:"           -- or something more precise

addImg = replaceChildren ( getChildren <+> imgElement )

imgElement = mkelem "img" [ sattr "src" "/icons/ref.png"
                          , sattr "alt" "external ref"
                          ] []






{-

data JDoc pos  = JExtends pos String [ IBlock ]
               | JBlock   pos String String
               | JRaw     pos String
               | JInclude pos String
               | JVar     pos String
               | JEExpr   pos String

command :: Parser (JDoc SourcePos)
command = do
    pos <- sourcePos
    str <- between (string "{{") (string "}}") (manyTill anyChar (lookAhead $ string "}}"))
    let cmd:args = splitOn "|" str
    return $ JVar pos cmd args

unformatted :: Parser (JDoc SourcePos)
unformatted = do
    pos <- sourcePos
    --try $ char '#'
    sss <- option "" (many1 $ char ' ')
    s <- (many1 $ alphaNum <|> (oneOf ".(),?!+-'\":_/;"))
    ss <- option "" (many1 $ char ' ')
    return $ JRaw pos (sss <> s <> ss)
    -- return $ Unformatted pos s

inlines = [ command
          , unformatted ]

jdoc :: Parser (Paragraph SourcePos)
jdoc = do
    -- choice over the rest
    pos <- sourcePos
    -- println2 $ show pos
    s <- sepEndBy1 (choice inlines) (many $ char ' ')
    (lookAhead newline)
    return $ ParagraphInline pos s

-}







