module Piet.DSL.Debug where

import Spreadsheet.Types

eval' :: (Spreadsheet -> Formula -> IO Formula) -> Spreadsheet -> Formula -> IO Formula
eval' eval model expr = case normalizeOp expr of
    -- EApp "WHATIS" e -> do
    --     -- res <- eval' eval model e
    --     return $ ESLit (show res)

    _ -> return ENotImplemented


-- main :: IO ()
-- main = Lua.run prog
--   where
--     prog :: Lua ()
--     prog = do
--       Lua.openlibs  -- load Lua libraries so we can use 'print'
--       Lua.callFunc "print" "Hello, World!"