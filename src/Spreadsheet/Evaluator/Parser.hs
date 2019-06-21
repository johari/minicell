{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE TupleSections #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


{-# LANGUAGE ScopedTypeVariables #-}


module Spreadsheet.Evaluator.Parser where

-- QR

import Codec.QRCode
import Codec.QRCode.JuicyPixels
import qualified System.IO.Streams as IOS
import System.IO

-- Logger

import System.Log.Logger

-- Interop

-- import Minicell.Interop.GitHub
import Minicell.Interop.PDF as PDF
import Minicell.Interop.YouTube as YT

-- Time stuff

import Data.Time.Clock.POSIX

-- Diagrams stuff

import Diagrams.Prelude hiding (value, (.=), connect)
-- import Diagrams.Backend.Rasterific.Text
-- import Diagrams.Backend.Rasterific
import Diagrams.Backend.SVG
import Graphics.Svg.Core

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
        s <- choice $ [ bangBang, formulaWithEqSign, numberLiteral, stringLiteral ]
        return s

bangBang :: Parser EExpr
bangBang = do
  string "!!"
  return (EApp "PDF" [ESLit "http://bangbangcon.com/west/images/logo.png", EILit 0])

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
  
  
  EApp op [ g, s, t ] | op `elem` ["MF", "SP"] -> do
    -- TODO: Return EError when pattern matching fails

    infoM "wiki.sheets.eval.eapp" "applying MF or SP"

    s1 <- eval model s
    infoM "wiki.sheets.eval.eapp" (show s1)
    s2 <- eval model t
    infoM "wiki.sheets.eval.eapp" (show s2)

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
      maybeVertex (addr, (EILit i)) = Just (addr, show i)
      maybeVertex _ = Nothing
      maybeEdge (addr, (EILit i)) = Just (addr, i)
      maybeEdge _ = Nothing



  EApp "GUNION" [ g1e, g2e ] -> do
    EGraphFGL g1 <- eval model g1e
    EGraphFGL g2 <- eval model g2e
    print $ labNodes g1
    print $ labNodes g2
    -- return $ EGraphFGL $ (labEdges g2) `insEdges` insNodes (labNodes g2) g1
    return $ EGraphFGL (union g1 g2)
    where
      union g1 g2 = mkGraph newNodes (fromMaybe [] $ mkEdges nm newEdges)
        where
          ln       = nub $ snd <$> ((labNodes g1) ++ (labNodes g2))
          (newNodes, nm) = mkNodes new (ln)
          newEdges = (betterListOfEdges g1) ++ (betterListOfEdges g2)

          betterListOfEdges g = catMaybes $
            [ do
                lab1 <- nodeLab g u
                lab2 <- nodeLab g v
                return $ (lab1, lab2, edgeLab)
              | (u, v, edgeLab) <- labEdges g 
            ]
            where
              revLabNodes g = Data.Map.fromList (labNodes g)
              nodeLab g nodeId = Data.Map.lookup nodeId (revLabNodes g)

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

  EApp "SHAPE" [ s ] -> do
    ESLit shape <- eval model s

    case shape of
      "circle" -> return $ EDiag (XDiagram $ circle 1)
      "square" -> return $ EDiag (XDiagram $ square 1)
      _ -> do
        return $ EDiag (XDiagram $ triangle 1)

  EApp "SHIFTX" [ d, x ] -> do
    EDiag (XDiagram diag) <- eval model d 
    EILit x <- eval model x
    
    return $ EDiag (XDiagram $ (diag # translate (r2 (fromIntegral x, 0)) # showOrigin))

  EApp "HCONCAT" [ d1, d2 ] -> do
    EDiag (XDiagram diag1) <- eval model d1
    EDiag (XDiagram diag2) <- eval model d2
    
    return $ EDiag $ XDiagram (diag1 ||| diag2)

  EApp "VCONCAT" [ d1, d2 ] -> do
    EDiag (XDiagram diag1) <- eval model d1
    EDiag (XDiagram diag2) <- eval model d2
    
    return $ EDiag $ XDiagram (diag1 === diag2)

  EApp "TURN" [ d, n1, n2 ] -> do
    EDiag (XDiagram diag1) <- eval model d
    EILit nn1 <- eval model n1
    EILit nn2 <- eval model n2
    
    return $ EDiag $ XDiagram (diag1 # rotateBy ((fromIntegral nn1)/ fromIntegral nn2))

  -- Adding colorful shapes to spreadsheets with =PAINT, =SHAPE and =HCONCAT
  EApp "PAINT" [ d, c ] -> do
    EDiag (XDiagram diag) <- eval model d 
    ESLit colorString <- eval model c

    let myColor = case colorString of
                      "green" -> green
                      "cyan" -> cyan
                      "yellow" -> yellow
                      "pink" -> pink
                      _ -> red

    return $ EDiag $ XDiagram (fc myColor $ diag)

  EApp "MYSQL" [ _databaseE, _queryE ] -> do
    ESLit databaseE <- eval model _databaseE
    ESLit queryE    <- eval model _queryE

    conn <- connect (defaultConnectInfo { connectHost = "127.0.0.1"
                                        , connectPort = 3306
                                        , connectPassword = "pietpietpiet"
                                        , connectUser = "root"
                                        , connectDatabase = databaseE
                                        })


    -- from: the current page title that corresponds to `pl_from`
    -- to: pl_title
    -- value on edge: none at this point.
    -- xs <- query_ conn "SELECT cast(page_title as CHAR), cast(pl_title as CHAR) FROM `thesis_pagelinks` INNER JOIN thesis_page ON thesis_page.page_id = pl_from WHERE 1"

    xs <- query_ conn (fromString queryE)

    close conn

    {-
    newEdges <- forM xs $ \(linkFrom, linkTo) ->
      return $ ((linkFrom, linkTo, 0) :: (String, String, Int))

    let (newNodes, nm) = mkNodes new (concat $ (\(x, y, _) -> [x, y]) <$> newEdges)
    return $ EGraphFGL $ mkGraph newNodes (fromMaybe [] $ mkEdges nm newEdges)
    -}

    let cleanUpMysql x = case x of
                          (Only s) -> s
                          _ -> ""

    return $ EList $ (ESLit <$> cleanUpMysql <$> (xs))

  EApp "QR" [ _payloadE ] -> do
    payloadE <- eval model _payloadE
    case payloadE of
      ESLit payloadS -> do
        let Just mat = Codec.QRCode.encode (defaultQRCodeOptions M) Iso8859_1 payloadS
        let imageURI = toPngDataUrlS 30 30 mat

        return $ EImage imageURI

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

  EApp "GH" [ repoE ] -> do
    ESLit repo <- eval model repoE
    g <- octoGraph repo
    return (EGraphFGL $ g)

  EApp "YT" _ -> do
    return $ EVideo "https://www.sample-videos.com/video123/mp4/720/big_buck_bunny_720p_1mb.mp4"

  EApp "PDF" [ urlE, pageE ] -> do
    ESLit url <- eval model urlE
    EILit page <- eval model pageE
    imagePath <- PDF.magicPdf url page
    -- Retrieve the PDF from url
    -- Convert PDF to a collection of PNGs
    -- Pick the appropriate PNG
    -- Save it in Minicell's cache
    -- Return the address so that cells can render it
    return (EImage $ imagePath)

  EApp "CROP" [ imgE, wE, hE, x0E, y0E ] -> do
    EImage sourceImagePath <- eval model imgE

    EILit x0 <- eval model x0E
    EILit y0 <- eval model y0E

    EILit w <- eval model wE
    EILit h <- eval model hE

    imagePath <- PDF.crop sourceImagePath (w, h, x0, y0)
    return (EImage $ imagePath)

  EApp "DEP" _ -> do
    putStrLn $ show model
    return $ dep

  EApp "DIR" _ -> do
    -- path <- eval model pathE
    -- print path
    listOfFiles <- getDirectoryContents "/minibox/"
    print listOfFiles
    return $ EList (EImage <$> listOfFiles)

  EApp "LEN" [ elE ] -> do
      el <- eval model elE
      case el of
        EList elList -> return $ EILit (length $ elList)
        _ -> return $ EError "Given argument is not a list."

  EApp "CAR" [ elE ] -> do
      el <- eval model elE
      case el of
        EList [] -> return $ EError "The list is empty"
        EList (x:xs) -> return $ x
        _ -> return $ EError "Given argument is not a list."

  EApp "LAST" [ elE ] -> do
      el <- eval model elE
      case el of
        EList [] -> return $ EError "The list is empty"
        EList el -> return $ last el
        _ -> return $ EError "Given argument is not a list."

  EApp "LOAD" [expr] -> do
    loadName <- eval model expr
    case loadName of
      ESLit "cities" -> return (EGraphFGL vor)
      ESLit "hello" -> return (EGraphFGL helloGraph)
      ESLit "ouroboros" -> return $ dep
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

  EApp "THESIS" [arg1, arg2, arg3] -> do
    aarg1 <- eval model arg1 
    aarg2 <- eval model arg2
    aarg3 <- eval model arg3

    let out = 
          case aarg1 of
            ESLit "poly" -> 
              case aarg2 of 
                ESLit "abstract" ->
                  case aarg3 of
                    EILit 1 -> "First sentence of abstract" 
                    _ -> (show aarg3) <> " not found in abstract"
                _ -> (show aarg2) <> " not found in poly"
            _ -> (show aarg1) <> "is not in thesis"
      in return $ ESLit out

  EApp "MAKE" args -> do
    case args of
      [ECellRange (rhoL, kappaL) (rhoR, kappaR)] -> do
        let matrix = [ (rho, kappa) | rho <- [rhoL .. rhoR], kappa <- [kappaL .. kappaR] ]
        vals <- sequence [ do val <- eval model (ECellRef addr); return (addr, val) | addr <- matrix ]

        return $ ESLit $ intercalate "\n" (curl <$> (filter (\(_,v) -> (eexprToASCII v) /= "") vals))

      _ -> return $ EError "Invalid arguments to MAKE"

    where
      curl (addr, val) = mconcat ["curl -X post  http://localhost:3000/minicell/"
                                 , (addrToExcelStyle addr)
                                 , "/write.json -d 'formula="
                                 , eexprToASCII val
                                 , "'" ]
      eexprToASCII val =
        case val of
          ESLit s -> s
          EILit i -> show i
          EError _ -> ""
          _ -> ""

  EApp "LATEX" ddd@[ECellRange (rhoL, kappaL) (rhoR, kappaR)] -> do
    let matrix = [ (rho, kappa) | rho <- [rhoL .. rhoR], kappa <- [kappaL .. kappaR] ]

    vals <- sequence [ do val <- eval model (ECellRef addr); return (addr, val) | addr <- matrix ]
    
    let ccc = "|" <> (intercalate "|" $ map (const "c") [kappaL .. (kappaR + 1)]) <> "|"

    let hBegin = "\\begin{tabular}{ " <> ccc <> " } \\hline % \\hline"
    let hC = (" & " <> (intercalate " & " (addrKappaToExcelStyle <$> [kappaL .. kappaR])))
    -- "cell1 & cell2 & cell3 \\\\"
    -- "cell4 & cell5 & cell6 \\\\"
    -- "cell7 & cell8 & cell9 \\\\"
    let hEnd = "\\end{tabular}"


    -- return $ ESLit $ intercalate "\n" ([h0, h1, hC] ++ [(rows vals)] ++ [h2, h3])
    return $ ESLit $ intercalate " \\\\ \\hline \n" [hBegin, hC, (rows vals), hEnd]

    where
      rows vals = intercalate "\\\\\n" $ map (printRow vals) [rhoL .. rhoR]
      printRow vals rho = intercalate " & " $ [show (rho+1)] <> (eexprToLaTeX <$> ((flip lookup) vals) <$> ((rho,) <$> [kappaL .. kappaR]))
      eexprToLaTeX val =
        case val of
          Just v ->
            case v of
              ESLit s -> s
              EILit i -> show i
              EError _ -> ""
              EGraphFGL _ -> "$\\mathbf{G}$"
              _ -> show v
          _ -> ""

  EApp "UNIXEPOCH" _ -> do
    t <- getPOSIXTime
    return $ EILit (round t)

  EApp "MOD" [ a1, a2 ] -> do
    EILit arg1 <- eval model a1
    EILit arg2 <- eval model a2

    return $ EILit (arg1 `mod` arg2)

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

  where
    dep = (EGraphFGL $ emap (const 0) $ nmap addrToExcelStyle $ dependencyGraph $ database model)
