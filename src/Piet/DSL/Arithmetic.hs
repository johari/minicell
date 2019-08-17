module Piet.DSL.Arithmetic where

import Spreadsheet.Types

eval' :: (Spreadsheet -> Formula -> IO Formula) -> Spreadsheet -> Formula -> IO Formula
eval' eval model expr = case normalizeOp expr of
  EApp "MOD" [ a1, a2 ] -> do
    EILit arg1 <- eval model a1
    EILit arg2 <- eval model a2

    return $ EILit (arg1 `mod` arg2)

  _ -> return $ ENotImplemented