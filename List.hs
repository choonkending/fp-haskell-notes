{-# LANGUAGE NoImplicitPrelude #-}
-- Do not load Prelude when compiling a Haskell module

module List where
--  A module in Haskell:
--  1. Controls namespaces
--  2. Creates abstract data types

import Prelude(Show, Integer, (+), seq)

data List a =
  Nil
  | a :- List a
  deriving (Show)

infixr 5 :-

-- values provided to test our functions

infinity ::
  List Integer
infinity =
  let inf x = x :- inf (x+1)
  in inf 0

-- Common functions used over a List

-- if the list is empty, the result is the initial value z; else
-- apply f to the first element and the result of folding the rest
foldRight :: (a -> b -> b) -> b -> List a -> b
foldRight _ b Nil       = b
foldRight f b (h :- t)  = f h (foldRight f b t)
--
-- list1 = 1 :- 2 :- 3 :- Nil
--   foldRight (+) 0 list1
-- = foldRight (+) 0 (1 :- 2 :- 3 :- Nil)
-- = (+) 1 (foldRight (+) 0 (2 :- 3:- Nil))
-- = (+) 1 ((+) 2 (foldRight (+) 0 (3 :- Nil)))
-- = (+) 1 ((+) 2 ((+) 3 foldRight((+) 0 Nil))))
-- = (+) 1 ((+) 2 ((+) 3 (+) 0)))
-- = (+) 1 ((+) 2 (+) 3)
-- = 6


-- if the list is empty, the result is the initial value z; else
-- we recursive immediately, making the new initial value the result
-- of combining the old initial value with the first element
foldLeft :: (b -> a -> b) -> b -> List a -> b
foldLeft _ b Nil        = b
foldLeft f b (h :- t)   = foldLeft f (f b h) t

foldLeft' :: (b -> a -> b) -> b -> List a -> b
foldLeft' _ b Nil       = b
foldLeft' f b (h :- t)  = let b' = f b h in seq b' foldLeft' f b' t

