module Piet.DSL.FAM where

-- I won't call it FAM.
-- I'd just mention this is Piet's equivalent of Markdown.

import Piet.DSL.FAM.Markup.IO
import Piet.DSL.FAM.Parser as FAM

import Text.Blaze.Html.Renderer.String
import Text.Blaze.Renderer.Utf8 (renderMarkup)

import Data.ByteString.Lazy.Char8 as Char8

import Text.Parsec
import Text.Blaze

import Spreadsheet.Types

eval' :: (Spreadsheet -> Formula -> IO Formula) -> Spreadsheet -> Formula -> IO Formula
eval' eval model expr = case normalizeOp expr of
  EApp "MD" [ elE ] -> do
      el <- eval model elE
      case el of
        ESLit markdownStr -> do
          s <- case parse FAM.document "A1" (markdownStr) of
                  (Right ast) -> do
                    html <- toMarkup' ast
                    return $ EHTML $ (Char8.unpack $ renderMarkup html)
                  (Left x) -> return $ EError (show x)
          return s
        _ -> return $ EError $ mconcat ["I can't interpret the argument (", show el, ") as a string"]

  _ -> return $ ENotImplemented

