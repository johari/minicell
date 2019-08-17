module Piet.DSL.Graphics.Shapes where

import Spreadsheet.Types

-- Diagrams stuff

import Diagrams.Prelude hiding (value, (.=), connect)
-- import Diagrams.Backend.Rasterific.Text
-- import Diagrams.Backend.Rasterific
import Diagrams.Backend.SVG
import Graphics.Svg.Core


eval' :: (Spreadsheet -> Formula -> IO Formula) -> Spreadsheet -> Formula -> IO Formula
eval' eval model expr = case normalizeOp expr of
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

  _ -> return $ ENotImplemented