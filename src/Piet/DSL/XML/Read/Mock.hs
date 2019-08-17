{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE PartialTypeSignatures              #-}

import System.Random (randomRIO)

import Diagrams.Prelude hiding (deep)
import Diagrams.Backend.SVG.CmdLine

import Text.XML.HXT.Core

data Annotation = Annotation { body :: String, x, y, w, h :: Int }
  deriving (Show, Eq)

noPhoneIO = do
  res <- loadImageEmb "flickr.png"
  return $ case res of
     Left err    -> mempty
     Right phone -> image phone # sized (dims2D 1.5 1.5)

getBody = deep (isElem >>> hasName "note") >>>
  proc ann -> do
  	body <- getText <<< getChildren -< ann
  	x <- getAttrValue "x" -< ann
  	y <- getAttrValue "y" -< ann
  	w <- getAttrValue "w" -< ann
  	h <- getAttrValue "h" -< ann
  	let myColor = [yellow, red, blue, green, purple, cyan, orange] !! ((read x :: Int) `mod` 5)
  	-- returnA -< Annotation { body = body, x = read x :: Int, y = read y :: Int, w = read w :: Int, h = read h :: Int }
  	returnA -< example myColor (read x :: Double) (read y :: Double) (read w :: Double) (read h :: Double)

main2 = do
  -- x <-  runX (readDocument [ withValidate no] "flickr.xml"
  --               >>> deep (isElem >>> hasName "note"))
  x <-  runX (readDocument [ withValidate no] "flickr.xml" >>> getBody )
  y <- noPhoneIO
  return $ mconcat ([y] ++ x)

main = do
  s <- main2
  mainWith s

example :: _ -> Double -> Double -> Double -> Double -> Diagram B
example myColor x y w h = rect w h # fcA (myColor `withOpacity` 0.8) # translateX x # translateY (-y)
-- example = circle 0.2 # lc purple # fc yellow
