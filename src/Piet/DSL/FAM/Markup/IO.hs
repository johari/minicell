{-# Language OverloadedStrings #-}

module Piet.DSL.FAM.Markup.IO where

import Piet.DSL.FAM.Markup.Pure
import Piet.DSL.FAM.Syntax

import Piet.DSL.FAM.Parser (paragraph)
import Text.Parsec (parse)

import Control.Monad

import Data.String
import Data.Monoid ((<>))
import Data.List (intersperse)
import Data.List.Split (splitOn)

import Text.Blaze
import Text.Blaze.Html
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import System.Posix.Time

class (ToMarkup a) => ToMarkup' a where
  toMarkup' :: a -> IO Markup
  -- toMarkup' x = return $ toMarkup x

instance (Show a, ToMarkup a) => ToMarkup' (Document a) where
  -- toMarkup' (Document _ l) = mconcat `liftM` (sequence $ (intersperse (return H.br) $ (toMarkup' <$> l)))
  toMarkup' (Document _ l) = mconcat `liftM` (sequence $ (toMarkup' <$> l))
--instance (Show a, ToMarkup a) => ToMarkup' [Paragraph a] where
--  --toMarkup' l = mconcat $ intersperse (fromString " ") $ mconcat $ [H.span <$> toMarkup <$> x | x <- l]
--  toMarkup' l = mconcat `liftM` (toMarkup' <$> l)

instance (Show a, ToMarkup a) => ToMarkup' (Paragraph a) where
  -- toMarkup' (ParagraphInline _ l) = mconcat `liftM` (sequence $ (intersperse (return H.br) $ (toMarkup' <$> l)))
  toMarkup' (ParagraphInline _ l) = mconcat `liftM` (sequence $ (toMarkup' <$> l))
  toMarkup' (ParagraphSingleton _ singletonItem) = toMarkup' singletonItem

-- instance (Show a, ToMarkup a) => ToMarkup' ([Inline a]) where
--   toMarkup' l = mconcat `liftM` (sequence $ (toMarkup' <$> l))

instance (Show a, ToMarkup a) => ToMarkup' (Singleton a) where
  toMarkup' (HRule a _) = pure H.hr

  toMarkup' (Heading a level l) = do
    -- lMarkup <- mconcat $ toMarkup' <$> l
    -- return $ ([H.h1, H.h2, H.h3] !! level) $ lMarkup
    return $ (([H.h1, H.h2, H.h3] !! level) ! A.id (fromString $ slugify l) $ mconcat [fromString l, preEscapedToMarkup ("&nbsp;"::String), linkToEditor]) where
      linkToEditor = (toMarkup a)

  toMarkup' (BoxList _ boxItems) = do
    markupedItems <- sequence (toMarkup' <$> boxItems)
    return $ mconcat (H.span <$> markupedItems)

  toMarkup' (EmptyLine _) = do
    return $ H.p ""


instance (Show a, ToMarkup a) => ToMarkup' (BoxItem a) where
  toMarkup' (BoxItem _ indentationLevel boxValue p) = do
    pp <- mconcat $ toMarkup' <$> p
    case boxValue of
      Faulty   -> return $ (H.div ! A.class_ (classForItem "faulty" indentationLevel) $ theBoxPre <> (H.span $ pp))
      Checked   -> return $ (H.div ! A.class_ (classForItem "checked" indentationLevel) $ theBoxPre <> (H.span $ (H.del $ pp)))
      Pending   -> return $ (H.div ! A.class_ (classForItem "pending" indentationLevel) $ theBoxPre <> (H.span $ pp))
      Unchecked -> return $ (H.div ! A.class_ (classForItem "unchecked" indentationLevel) $ theBoxPre <> (H.span $ pp))
      _         -> return $ (H.div $ theBoxPre <> (H.span $ pp))
    where theBoxPre = (H.code ! A.class_ "box-item-pad" $
                        (fromString $ replicate indentationLevel '.'))
                  <>  (H.code ! A.class_ "box" $
                        (fromString "[") <> (toMarkup boxValue) <> (fromString "] "))
          classForItem boxValueString indentationLevel =
            "box-item " <> (boxValueString) <> (if indentationLevel == 0 then " top-level" else "")



instance (Show a, ToMarkup a) => ToMarkup' (Inline a) where
  -- toMarkup' (Command a "table" [columnSeperator, rowSeperator, contet]) = do
  --   row <- splitOn (if rowSeperator == "\\n" then "\n" else rowSeperator) content
  --   column <- splitOn columnSeperator row
  --   liftIO $ do
  --     case fakeParagraphParse column of
  --       Right ast -> toMarkup' ast
  --       _ -> "parse error in table :("
  --   liftIO $ return
  --   where fakeParagraphParse str = parse paragraph "" (str <> "\n")

  -- toMarkup' (WikiLink a pageName) = do
  --   get "https://nima.wiki/api.php?action=query&titles=" <> page&format=xml"
  --   -- hasAttr missing
  --   --
  --   get "https://nima.wiki/api.php?action=query&titles=Nima|Foo&format=json"

  toMarkup' (Command a "now" ["epoch"]) = do
    t <- epochTime
    return $ (H.code $ fromString $ show t)

  toMarkup' c@(Command _ _ _) = do
    return $ toMarkup c

  toMarkup' x = return (toMarkup x)