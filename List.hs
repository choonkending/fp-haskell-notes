{-# LANGUAGE NoImplicitPrelude #-}
-- Do not load Prelude when compiling a Haskell module

module List where
--  A module in Haskell:
--  1. Controls namespaces
--  2. Creates abstract data types

import Prelude(Show)

data List a =
  Nil
  | a :- List a
  deriving (Show)

infixr 5 :-

