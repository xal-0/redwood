module Utils where

-- | Safe version of !!. Perhaps an indication we shouldn't be using
-- lists.
(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
(x : _) !!? 0 = Just x
(_ : xs) !!? n = xs !!? (n - 1)

-- | Replace an element in a list, safely.
replaceIdx :: [a] -> Int -> a -> Maybe [a]
replaceIdx [] _ _ = Nothing
replaceIdx (_ : xs) 0 y = Just (y : xs)
replaceIdx (x : xs) n y = do
  xs' <- replaceIdx xs (n - 1) y
  pure (x : xs')
