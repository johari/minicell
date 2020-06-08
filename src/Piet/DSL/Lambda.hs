module Piet.DSL.Lambda where

import Spreadsheet.Types

eval' :: (Spreadsheet -> Formula -> IO Formula) -> Spreadsheet -> Formula -> IO Formula
eval' eval model expr = case normalizeOp expr of
  EApp "FUN" args -> do
      print args
      return $ ESLit "λ"

  _ -> return ENotImplemented