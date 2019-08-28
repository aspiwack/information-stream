module Lib where

-- There is a more interesting definition of finite types which is to require
-- that (a->) is traversable. While it's more interesting, it doesn't seem
-- particularly helpful here, so I'm keeping to the simple definition.
class Ord a => Finite a where
  elements :: [a]
