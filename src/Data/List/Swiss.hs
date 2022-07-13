module Data.List.Swiss(
    module Data.List,
    replace,
    ) where
    
import Partial    
import Data.List


-- | Replace a subsequence everywhere it occurs. The first argument must
--   not be the empty list.
--
-- > replace "el" "_" "Hello Kelly" == "H_lo K_ly"
-- > replace "ll" "r" "Hello"       == "Hero"
replace :: (Partial, Eq a) => [a] -> [a] -> [a] -> [a]
replace [] _ _ = error "Swiss.replace, first argument cannot be empty"
replace old new [] = []
replace old new xs
  | Just xs <- stripPrefix old xs = to ++ replace old new xs
replace old new (x:xs) = x : replace old new xs
