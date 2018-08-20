module Main where

import qualified Reactive

data Tree a = Leaf | Node (Tree a) a (Tree a)
    deriving Show

-- instance Arbitrary a => Arbitrary (Tree a) where
--    arbitrary = genericArbitrary' Z

-- Equivalent to
-- > arbitrary =
-- >   sized $ \n ->
-- >     if n == 0 then
-- >       return Leaf
-- >     else
-- >       oneof
-- >         [ return Leaf
-- >         , Node <$> arbitrary <*> arbitrary <*> arbitrary
-- >         ]


main = do
  Reactive.main
