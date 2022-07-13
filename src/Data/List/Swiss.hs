module Data.List.Swiss(
    module Data.List,
    (!?),
    chunksOf,
    notNull,
    replace, replaceBy, replaceOnce,
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


chunksOf :: Int -> [a] -> [[a]]
chunksOf i xs 
    | null xs || i <= 0 = []
    | otherwise = [take i xs] ++ chunksOf i (drop i xs)


notNull :: [a] -> Bool
notNull = not . null


-- | Replace a subsequence everywhere it occurs. Return the original list if the first argument is empty. 
--
-- > replace "ll" "r" "Hello Kelly" == "Hero Kery"
-- > replace "l" "" "Hello"       == "Heo"
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ xs = xs
replace _ _ [] = []
replace old new xs
    | Just ys <- stripPrefix old xs = new ++ replace old new ys
replace old new (x:xs) = x : replace old new xs



-- | Replace a subsequence one time only. Return the original list if the first argument is empty. 
--
-- > replaceOnce "ll" "r" "Hello Kelly" == "Hero Kelly"
-- > replaceOnce "l" "" "Hello"       == "Helo"
replaceOnce :: Eq a => [a] -> [a] -> [a] -> [a]
replaceOnce [] _ xs = xs
replaceOnce _ _ [] = []
replaceOnce old new xs
    | Just ys <- stripPrefix old xs = new ++ ys
replaceOnce old new (x:xs) = x : replaceOnce old new xs




-- | > replaceBy (=='b') 'l' "book" --> "look"
--   > replaceBy (<0) 0 [-4,-5,7] --> [0,0,7]
replaceBy :: (a -> Bool) -> a -> [a] -> [a]
replaceBy f new xs = map (\x -> if f x then new else x) xs
