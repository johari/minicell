{-# LANGUAGE OverloadedStrings #-}

module Spreadsheet.Evaluator.Parser where

-- Bytestring stuff

import qualified Data.ByteString.Lazy as LB

-- MD5 stuff

import Data.Digest.Pure.MD5

-- Posix stuff

import System.IO.Temp
import System.Directory
import System.Process

-- Haskell stuff

import Data.List
import Data.Char (ord, toLower, toUpper)
import Data.Maybe 
import qualified Data.Map

import Control.Monad

-- Minicell stuff
import Spreadsheet.Types 
import Spreadsheet.Examples.Graphs

-- Graph stuff

import Data.Graph.Inductive.NodeMap


import Data.Graph.Inductive.Example (vor)

import Data.Graph.Inductive.Basic


import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.GraphViz.Attributes.Complete (Attributes)

-- Graph Algorithms

import Data.Graph.Inductive.Query.BFS
import Data.Graph.Inductive.Query.MaxFlow
import Data.Graph.Inductive.Query.SP

-- GraphViz stuff

import Data.GraphViz (dotToGraph)
import Data.GraphViz.Types (parseDotGraph, mapDotGraph, graphNodes, graphEdges)
-- import Data.GraphViz.Types.Graph
import Data.GraphViz.Types.Generalised

-- Parsec stuff
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char


-- Stache stuff

import Data.String
import Data.Aeson
import Text.Mustache hiding (Node)
import Data.Aeson
import qualified Data.Text.Lazy as L
import qualified Data.Text as T

-- MySQL stuff

import Database.MySQL.Simple

cellContent :: Parser EExpr
cellContent = do
  rest <- getInput 
  case rest of
    "" -> return $ EEmpty
    _ -> do
        s <- choice $ [ formulaWithEqSign, numberLiteral, stringLiteral ]
        return s

numberLiteral :: Parser EExpr
numberLiteral = do
  num <- many1 digit
  return $ EILit (read num :: Int)

formulaWithEqSign = do
  char '='
  formula

formula = do
  s <- choice [ formulaCellRef
              , formulaWithOperands, numberLiteral, stringLiteral ]
  return s

cometKeyToAddr cometKey =
  case parse excelStyleAddr "" cometKey of
    Right addr -> addr
    Left err  -> (-1, -1)

excelStyleAddr :: Parser CellAddress
excelStyleAddr =
  do
    column <- letter
    row <- many1 digit
    return $ (((read row) - 1), ((ord $ toLower $ column) - (ord 'a'))) -- This is ultra buggy (works only for A-F)

-- =A1 or =A1:G7
formulaCellRef = do 
  -- column <- try $ many1 letter
  -- row <- try $ many1 digit
  parsedAddress1 <- try $ excelStyleAddr
  rangeRefPortion <- optionMaybe $ do
    try $ string ":"
    parsedAddress2 <- try $ excelStyleAddr
    return $ parsedAddress2

  case rangeRefPortion of
    Just parsedAddress2 -> return $ ECellRange parsedAddress1 parsedAddress2
    Nothing -> return $ ECellRef parsedAddress1

-- =X(A1:G7)
formulaWithOperands = do
  operation <- try $ many1 letter
  char '('
  args <- sepBy formula (char ',' *> spaces)
  char ')'
  return $ EApp operation args

stringLiteral = do
  s <- many1 anyToken
  return $ ESLit s
-- stringToEExpr buffer =
--   if buffer |> String.startsWith "@" then 
--     EApp "exampleGraph" [(buffer |> String.dropLeft 1 |> ESLit)] |> eval emptySpreadsheet
--   else if buffer |> String.startsWith "=A" then
--     ECellRef (withDefault 0 (buffer |> String.dropLeft 2 |> String.toInt)-1, 0)
--   else if buffer |> String.startsWith "=B" then
--     ECellRef (withDefault 0 (buffer |> String.dropLeft 2 |> String.toInt)-1, 1)
--   else if buffer == "=1+1" then
--     EILit 2
--   else if all isDigit buffer  then
--     EILit (String.toInt buffer |> withDefault 0)
--   --else if buffer == "=(+1)" then
--   --  ELambda "(+1)"
--   else
--     ESLit (Debug.toString (buffer |> run sexp))


-- Shortcuts to load dummy values (for demo purposes)
-- 
-- @cities
-- @1
-- @dressUp
-- @2

-- Literals
-- 
-- Foo
-- 1
-- 
-- 
-- =C2
-- =1
-- =SUM(A1:A10)
-- =GUNIOUN(G1:G2)
-- =GUNIOUN(G1, G2)
-- =C3+C1
-- =YOUTUBE("5ScquRuRIyE", 850) (same as =https://www.youtube.com/watch?v=5ScquRuRIyE&t=850s)
-- =YOUTUBE("5ScquRuRIyE", 850, 60)
-- 
-- Gunrock
-- =GR_LOAD()
-- =GR_UNION()
-- 
-- 
-- Perhaps later on we can provide fast vector processing as well
-- 
-- Or a DSL for combining sound and media
-- or gifs
-- 
-- It'd be nice if we could connect to external sources and read documents from them too, say
-- git, dat and mysql.
-- 
-- Sometimes I dream about fetching RSS and JSON as well.
-- 

normalizeOp expr =
  case expr of
    EApp op args -> EApp (map toUpper op) args
    _ -> expr

eval :: Spreadsheet -> Formula -> IO Formula
eval model expr = case normalizeOp expr of
  ECellRef lookupAddr -> do
    case find (\x -> addr x == lookupAddr) (database model) of
      Nothing ->
        return $ EError $ "#REF " ++ (addrToExcelStyle lookupAddr)
      Just cell -> eval model (value cell)
  
  
  EApp op [ g, s, t ] | elem op ["MF", "SP"] -> do
    -- TODO: Return EError when pattern matching fails

    eval model s >>= print
    eval model t >>= print

    (ESLit s') <- eval model s

    (ESLit t') <- eval model t
    myError <- eval model g
    print myError
    let (EGraphFGL g') = myError

    let nn1 = gsel (\(_, _, label, _) -> label == s') g'
    let nn2 = gsel (\(_, _, label, _) -> label == t') g'

    print nn1
    print nn2
    let ((_, n1, _, _):_) = nn1
    let ((_, n2, _, _):_) = nn2

    case (map toLower op) of
        "mf" -> return (EILit $ maxFlow g' n1 n2)
        "sp" -> do
          print (n1, n2)
          return (EILit $ fromMaybe 0 $ spLength n1 n2  g')
        _ -> return (EError $ "error evaluating " ++ op)

  EApp "X" [ECellRange (rhoL, kappaL) (rhoR, kappaR)] -> do
    let headerRow = [ (rhoL, kappa) | kappa <- [ (kappaL + 1) .. kappaR] ]
        headerColumn = [ (rho, kappaL) | rho <- [ (rhoL+1) .. rhoR ] ]
        matrix = [ (rho, kappa) | rho <- [rhoL+1 .. rhoR], kappa <- [kappaL+1 .. kappaR] ]

    verticesWithAddr <- sequence [ do val <- eval model (ECellRef addr); return (addr, val) | addr <- (headerRow ++ headerColumn) ]
    edgesWithAddr <- sequence [ do val <- eval model (ECellRef addr); return (addr, val) | addr <- matrix ]
    
    let newVertices = (catMaybes $ (maybeVertex <$> verticesWithAddr))
    let newEdges = catMaybes $
          [
            do
              ((rho, kappa), i) <- maybeEdge s
              v1 <- lookup (rho, kappaL) newVertices
              v2 <- lookup (rhoL, kappa) newVertices
              return $ (v1, v2, i)
          | s <- edgesWithAddr
          ]

    let (newNodes, nm) = mkNodes new (snd <$> newVertices)
    return $ EGraphFGL $ mkGraph newNodes (fromMaybe [] $ mkEdges nm newEdges)

    where
      maybeVertex (addr, (ESLit s)) = Just (addr, s)
      maybeVertex _ = Nothing
      maybeEdge (addr, (EILit i)) = Just (addr, i)
      maybeEdge _ = Nothing



  EApp "GREV" [g] -> do
    (EGraphFGL g') <- eval model g
    return $ EGraphFGL $ grev g'

  EApp "CTX" [g, node] -> do
    (EGraphFGL g') <- eval model g
    
    (ESLit source) <- eval model node
    -- TODO: lookup node numer

    let nn1 = gsel (\(_, _, label, _) -> label == source) g'
    -- print g'
    -- print nn1
    let ((_, n1, _, _):_) = nn1

    -- let (newNodes, _) = mkNodes new [fromMaybe "" (lab g' n2) | n2 <- (suc g' n1) <> (pre g' n1)]
    -- return $ EGraphFGL $ mkGraph newNodes []

    -- return $ EGraphFGL $ efilter (\(x1, x2, _) -> n1 == x1 || n1 == x2) g'

    return $ EGraphFGL $ subgraph ([n1] <> neighbors g' n1) g'

  

  EApp "MYSQL" args -> do
    print args
    conn <- connect (defaultConnectInfo { connectHost = "127.0.0.1"
                                        , connectPort = 3307
                                        , connectPassword = "SECRET"
                                        , connectUser = "root"
                                        , connectDatabase = "wiki-thesis"
                                        })
    
    
    -- from: the current page title that corresponds to `pl_from`
    -- to: pl_title
    -- value on edge: none at this point.
    -- xs <- query_ conn "SELECT cast(page_title as CHAR), cast(pl_title as CHAR) FROM `thesis_pagelinks` INNER JOIN thesis_page ON thesis_page.page_id = pl_from WHERE 1"

    xs <- query_ conn "SELECT cast(page_title as CHAR), cast(pl_title as CHAR) FROM `nima_pagelinks` INNER JOIN nima_page ON nima_page.page_id = pl_from WHERE 1"

    newEdges <- forM xs $ \(linkFrom, linkTo) ->
      return $ ((linkFrom, linkTo, 0) :: (String, String, Int))

    let (newNodes, nm) = mkNodes new (concat $ (\(x, y, _) -> [x, y]) <$> newEdges)
    return $ EGraphFGL $ mkGraph newNodes (fromMaybe [] $ mkEdges nm newEdges)

  EApp "MUSTACHE" args -> do
    let mustacheText = "Hello {{A1}} <a href=\".{{A2}}\">{{A2}}</a>"
    let compiledTemplate = compileMustacheText "foo" mustacheText
    
    a1InHtml <- eval model (ECellRef (0, 0)) >>= eexprToHtml
    a2InHtml <- eval model (ECellRef (1, 0)) >>= eexprToHtml

    case compiledTemplate of
      Left _ -> return $ EError "Mustache compile failed"
      Right template ->
        return $ ESLit (L.unpack $ renderMustache template $ object [ "A1" .= (T.pack $ a1InHtml)
                                                                    , "A2" .= (T.pack $ a2InHtml)
                                                                    ])

  EApp "DOT" [expr] -> do
    ESLit dot <- eval model expr
    let dotGraph = parseDotGraph $ fromString dot :: DotGraph String
    let verticesFromDot = nodeID <$> graphNodes dotGraph
    let edgesFromDot = (\x -> (fromNode x, toNode x)) <$> graphEdges dotGraph

    let g = (mkGraph vertices edges)
              where
                (vertices, nm) = mkNodes new verticesFromDot
                edges = fromMaybe [] $ mkEdges nm [ (v1, v2, 1) | (v1, v2) <- edgesFromDot ]

    -- let okayGraph = mapDotGraph (const 0) dotGraph :: DotGraph Node
    -- print  $ (dotToGraph (okayGraph)  :: Gr Data.GraphViz.Attributes.Complete.Attributes Data.GraphViz.Attributes.Complete.Attributes)
    return $ EGraphFGL g

  EApp "LOAD" [expr] -> do
    loadName <- eval model expr
    case loadName of
      ESLit "cities" -> return (EGraphFGL vor)
      ESLit "hello" -> return (EGraphFGL helloGraph)
      ESLit "ouroboros" -> return (EGraphFGL $ emap (const 0) $ nmap addrToExcelStyle $ dependencyGraph $ database model)
      _ -> return (EError $ (mconcat ["graph `", show loadName, "` not found :("] :: String))

  EApp "AUDIO" [ expr ] -> do
    ESLit src <- eval model expr

    return $ EAudio src

  EApp "AOLAY" [ expr1, expr2 ] -> do
    EAudio src1 <- eval model expr1
    EAudio src2 <- eval model expr2
    audioPath <- do
                    h <- getHomeDirectory
                    return (h ++ "/Dropbox/minicell-uploads/audio/")

    -- TODO:
    -- To avoid unnecessary recomputation
    -- Add a cache subsystem
    -- Then calculate md5 checksum of src1 and src2
    -- Then associate ("OLAY", checksum1, checksum2)
    -- to checksum of the output file

    targetPath <- withSystemTempDirectory "minicell-audio" $ \tmp -> do      
                    readProcess "ffmpeg" [ "-i", mconcat [ audioPath, src1 ]
                                         , "-i", mconcat [ audioPath, src2 ]
                                         , "-filter_complex", "amerge"
                                         , "-ac", "2"
                                         , "-c:a", "libmp3lame"
                                         , "-q:a", "4"
                                         , "output.mp3"
                                         ] ""
                    fileContent <- LB.readFile "output.mp3"
                    let md5Digest = md5 fileContent
                    let targetPath = mconcat [audioPath, show md5Digest, ".mp3"]
                    copyFile "output.mp3" targetPath
                    return targetPath

    return (EAudio $ targetPath)

  EApp "ACONCAT" [ expr1, expr2 ] -> do
    EAudio src1 <- eval model expr1
    EAudio src2 <- eval model expr2
    audioPath <- do
                    h <- getHomeDirectory
                    return (h ++ "/Dropbox/minicell-uploads/audio/")

    let fullSrc1 = mconcat [ audioPath, src1 ]
        fullSrc2 = mconcat [ audioPath, src2 ]

    fileContent1 <- LB.readFile fullSrc1
    fileContent2 <- LB.readFile fullSrc2
    let cacheKey = mconcat ["ACONCAT", show $ md5 fileContent1, show $ md5 fileContent2 ]

    let targetPath = mconcat [ audioPath, cacheKey, ".mp3" ]

    exists <- doesFileExist targetPath

    if exists then return () else
      withSystemTempDirectory "minicell-audio" $ \tmp -> do      
        print tmp
        readProcess "sox" [ fullSrc1
                          , fullSrc2
                          , "output.mp3"
                          ] ""

        copyFile "output.mp3" targetPath
        return ()

    return (EAudio $ targetPath)

  EApp op args -> do
    return $ ESLit $ (show op) ++ " " ++ show args ++ " is not implemented"

  _ -> return expr
