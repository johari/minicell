module Piet.DSL.Poppet where

import Spreadsheet.Types

eval' :: (Spreadsheet -> Formula -> IO Formula) -> Spreadsheet -> Formula -> IO Formula
eval' eval model expr = case normalizeOp expr of
  _ -> return $ ENotImplemented