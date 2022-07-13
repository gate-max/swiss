module Data.List.Swiss(
    module Data.List,
    (!?),
    notNull,
    replace, replaceBy,
    ) where
    

import            Data.List
import            Data.Maybe(listToMaybe)

infixl 9 !?
  
-- | zero-based index, compare (!!) in Prelude
-- > [1..10] !? 1   --> Just 2 
-- > [1..10] !? 15  --> Nothing
-- > ["apple", "boy", "cat", "dog"] !? 0  --> Just "apple"
-- slow if using genericDrop to accept Integral types
(!?) :: [a] -> Int -> Maybe a
(!?) xs n
    | n < 0 = Nothing
    | otherwise = listToMaybe $ drop n xs    


notNull :: [a] -> Bool
notNull = not . null


-- | Replace a subsequence everywhere it occurs. The first argument must
--   not be the empty list.
--
-- > replace "el" "_" "Hello Kelly" == "H_lo K_ly"
-- > replace "ll" "r" "Hello"       == "Hero"
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = error "Swiss.replace, first argument cannot be empty"
replace old new [] = []
replace old new xs
    | Just xs <- stripPrefix old xs = new ++ replace old new xs
replace old new (x:xs) = x : replace old new xs


-- | > replaceBy (=='b') 'l' "book" --> "look"
--   > replaceBy (<0) 0 [-4,-5,7] --> [0,0,7]
replaceBy :: (a -> Bool) -> a -> [a] -> [a]
replaceBy f new xs = map (\x -> if f x then new else x) xs
