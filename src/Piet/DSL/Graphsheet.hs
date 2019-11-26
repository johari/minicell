module Piet.DSL.Graphsheet where

import Spreadsheet.Types
import Spreadsheet.Examples.Graphs

import qualified Data.Map


import System.Log.Logger

-- HTTP stuff
import Network.HTTP

--

import Data.String
import Data.Char (toLower)
import Data.List (nub)
import Data.Maybe (catMaybes, fromMaybe)


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


eval' :: (Spreadsheet -> Formula -> IO Formula) -> Spreadsheet -> Formula -> IO Formula
eval' eval model expr = case normalizeOp expr of
  EApp "LOAD" [expr] -> do
    loadName <- eval model expr
    case loadName of
      ESLit "cities" -> return (EGraphFGL vor)
      ESLit "hello" -> return (EGraphFGL helloGraph)
      ESLit "ouroboros" -> return $ dep
      _ -> return (EError $ (mconcat ["graph `", show loadName, "` not found :("] :: String))


  EApp "DEP" _ -> do
    putStrLn $ show model
    return $ dep


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

  EApp op [ queryGraph_, baseGraph_ ] | op `elem` ["MATCH", "IMATCH"] -> do
    EGraphFGL queryGraph <- eval model queryGraph_
    EGraphFGL baseGraph  <- eval model baseGraph_

    infoM "wiki.sheets.eval.sm" (show $ labNodes queryGraph)

    let payload = [ show $ labNodes queryGraph
                  , show $ labEdges queryGraph
                  , show $ labNodes baseGraph
                  , show $ labEdges baseGraph
                  ]

    let endpoint = case op of
                    "MATCH"  -> "http://127.0.0.1:9999/sm-di"
                    "IMATCH" -> "http://127.0.0.1:9999/sm"

    b <- simpleHTTP (postRequestWithBody endpoint "application/json" (show payload)) >>= getResponseBody
    let subgraphNodes = (read b) :: [[Int]]

    case length subgraphNodes of
      0 -> return (EError "No subgraph found")
      -- n -> return $ EILit n
      _ -> return $ EList $ nub $ ((\x -> (EGraphFGL $ subgraph (x) baseGraph)) <$> subgraphNodes)

  EApp op [ g, s, t ] | op `elem` ["MF", "SP"] -> do
    -- TODO: Return EError when pattern matching fails

    -- infoM "wiki.sheets.eval.eapp" "applying MF or SP"

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

  {-
  EApp "CAL" _ -> do
    Right (s, _) <- parseICalendarFile def "/Users/nima/johari/minicell/examples/trip.ics"
    let events = vcEvents $ s !! 0
    return $ EList (ESLit <$> L.unpack <$> (Data.Map.elems $ (maybe "No summary" summaryValue) <$> veSummary <$> events))

  -}

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

  _ -> return $ ENotImplemented

  where
    dep = (EGraphFGL $ emap (const 0) $ nmap addrToExcelStyle $ dependencyGraph $ database model)