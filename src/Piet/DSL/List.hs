module Piet.DSL.List where

import Spreadsheet.Types

eval' :: (Spreadsheet -> Formula -> IO Formula) -> Spreadsheet -> Formula -> IO Formula
eval' eval model expr = case normalizeOp expr of
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

  _ -> return ENotImplemented