{-# Language OverloadedStrings #-}
{-# Language GADTs, FlexibleContexts #-}

module Reactive where

import Text.Parsec

import Data.Monoid
import Data.String
import Data.Text as T
import Data.List

import Reflex.Dom
import Reflex.PostBuild.Base

import Control.Monad.IO.Class (liftIO)
import Control.Monad


main :: IO ()
main = mainWidget $ do
  let myFamText = "[ ] todo\n[.]doing\n[x]done\n"
  let myFamText2 = "todo\n"
  t1 <- textArea def
  t2 <- textArea def
  el "hr" $ text ""
  let evText = tagPromptlyDyn (value t1) (_textArea_input t1)

  -- t2 <- textArea $ def & setValue .~ (fmap (\x -> T.pack $ show $ parse document "" (T.unpack x)) evText) 
  --void $ widgetHold (famWidget "Hi! \n") (fmap (\x -> famWidget $ T.unpack x) evText)
 
  --void $ widgetHold (text "Hello World") (fmap (\x -> text x) evText)
  void $ widgetHold (textInput def) (fmap (\x -> textInput (def & textInputConfig_initialValue .~ (T.reverse x))) evText)
  -- s <- fmap (\x -> famWidget (T.unpack x)) evText
  -- dyn s
  -- let reflex = display =<< Reflex.Dom.count =<< button "ClickMe"
  -- reflex
