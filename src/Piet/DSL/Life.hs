module Piet.DSL.Life where

import Spreadsheet.Types

eval' :: (Spreadsheet -> Formula -> IO Formula) -> Spreadsheet -> Formula -> IO Formula
eval' eval model expr = case normalizeOp expr of

  EApp "LIFE" _ -> do
    let a = nextGen (fromList [(0,0), (0,1), (1,0)])
    let b = nextGen a
    return $ ESpill $ EList (EILit <$> toList el)

  _ -> return $ ENotImplemented

  where
    neighbours p = Set.fromList [(fst p + dx, snd p + dy) | dx <- [-1..1], dy <- [-1..1], not (dx == 0 && dy == 0)]

    lives g p = ((member p g) && count `elem` [2, 3]) || (not (member p g) && count == 3)
        where count = Set.size $ Set.filter (flip member g) (neighbours p)

    nextGen g = Set.filter (lives g) (union g $ unions [neighbours p | p <- toList g])
