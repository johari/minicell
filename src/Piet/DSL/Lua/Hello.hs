module Piet.DSL.Lua.Hello where

import Spreadsheet.Types
import qualified Foreign.Lua as Lua

eval' :: (Spreadsheet -> Formula -> IO Formula) -> Spreadsheet -> Formula -> IO Formula
eval' eval model expr = case normalizeOp expr of
    EApp "LUA" [ elFunc, elArg1 ] -> do
        func <- eval model elFunc
        case func of
            ESLit func -> do
                arg1 <- eval model elArg1
                case arg1 of
                    ESLit arg1 -> do
                        -- s <- Lua.run (prog func arg1)
                        getLuaVersion
                        return $ ESLit $ "See terminal"
                    _ -> return $ EError "I don't know how to handle non-string LUA arguments"

                where
                    -- prog :: Lua ()
                    prog func arg1 = do
                        Lua.openlibs  -- load Lua libraries so we can use 'print'
                        Lua.callFunc func arg1
                        -- Lua.callFunc "print" "Hello, World!"

            _ -> do
                print "what the func?"
                print func
                return $ EError "Given argument is not a list."


    _ -> return ENotImplemented

getLuaVersion = do
    Lua.run $ do
        Lua.openlibs
        Lua.getglobal "print"
        Lua.pushstring "Hello from"
        Lua.getglobal "_VERSION"
        Lua.call 2 0

-- main :: IO ()
-- main = Lua.run prog
--   where
--     prog :: Lua ()
--     prog = do
--       Lua.openlibs  -- load Lua libraries so we can use 'print'
--       Lua.callFunc "print" "Hello, World!"