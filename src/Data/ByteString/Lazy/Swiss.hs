{-# LANGUAGE OverloadedStrings #-}

module Data.ByteString.Lazy.Swiss (
    (\\),
    delete,    
    replace, replaceOnce,
    ) where
    


import qualified Data.ByteString.Lazy.Internal as L (ByteString(Empty))
import qualified Data.ByteString.Lazy.Char8 as L (append, break, drop, foldl, null, splitAt, stripPrefix) 


infix 5 \\


-- | this is equivalant to (\\) in Data.List
-- > "hello" \\ "heo" --> "ll"  (assuming OverloadedStrings)
(\\) :: L.ByteString -> L.ByteString -> L.ByteString
(\\) = L.foldl (flip delete)


-- | delete first occurance of a char in a lazy bytestring, assume OverloadedStrings 
-- > delete 'c' "abcdec" --> "abdec"
-- > delete 'f' "abcdec" --> "abcdec"
-- > delete 'c' "" --> ""
delete :: Char -> L.ByteString -> L.ByteString
delete _ L.Empty = L.Empty
delete c bs = one `L.append` (L.drop 1 two)
    where  (one,two) = L.break (==c) bs


-- | Replace a subsequence everywhere it occurs. Return the original list if the first argument is empty. 
--
-- > replace "ll" "r" "Hello Kelly" == "Hero Kery"
-- > replace "l" "" "Hello"       == "Heo"
replace :: L.ByteString -> L.ByteString ->  L.ByteString -> L.ByteString
replace old new bs
    | L.null bs || L.null old = bs
    | Just bs2 <- L.stripPrefix old bs = new `L.append` replace old new bs2
    | (bs3, bs4) <- L.splitAt 1 bs = bs3 `L.append` replace old new bs4
    


-- | Replace a subsequence one time only. Return the original list if the first argument is empty. 
--
-- > replaceOnce "ll" "r" "Hello Kelly" == "Hero Kelly"
-- > replaceOnce "l" "" "Hello"       == "Helo"
replaceOnce :: L.ByteString -> L.ByteString ->  L.ByteString -> L.ByteString
replaceOnce old new bs
    | L.null bs || L.null old = bs
    | Just bs2 <- L.stripPrefix old bs = new `L.append` bs2
    | (bs3, bs4) <- L.splitAt 1 bs = bs3 `L.append` replaceOnce old new bs4





