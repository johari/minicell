{-# Language DeriveDataTypeable #-}
module Piet.DSL.FAM.Utils where

import Piet.DSL.FAM.Syntax

-- create a summary of a fam file
-- we can have a
--  [ ] tiny summary (# of todo, # of doing, # of done --> [ ] 42 [.] 31 [ ] 5)
--  [ ] short summary
--  [ ] mid size summary
-- aggregate the number of todos