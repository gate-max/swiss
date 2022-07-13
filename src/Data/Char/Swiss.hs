
module Data.Char.Swiss 
    (module Data.Char,
    notAlpha, notAlphaNum,
    notControl,
    notLower,
    notUpper,


    )where

import Data.Char (isControl, isSpace, isLower, isUpper, isAlpha, isAlphaNum, isPrint
       , isDigit, isOctDigit, isHexDigit, isLetter, isMark, isNumber, isPunctuation
       , isSymbol, isSeparator, isAscii, isLatin1, isAsciiUpper, isAsciiLower)



notControl :: Char -> Bool
notControl = not . isControl


notSpace :: Char -> Bool
notSpace = not . isSpace


notLower :: Char -> Bool
notLower = not . isLower


notUpper :: Char -> Bool
notUpper = not . isUpper


isNotAlpha :: Char -> Bool
isNotAlpha = not . isAlpha


isNotAlphaNum :: Char -> Bool
isNotAlphaNum = not . isAlphaNum


isNotPrint :: Char -> Bool
isNotPrint = not . isPrint


isNotDigit :: Char -> Bool
isNotDigit = not . isDigit


isNotOctDigit :: Char -> Bool
isNotOctDigit = not . isOctDigit


isNotHexDigit :: Char -> Bool
isNotHexDigit = not . isHexDigit


isNotLetter :: Char -> Bool
isNotLetter = not . isLetter


isNotMark :: Char -> Bool
isNotMark = not . isNotMark


isNotNumber :: Char -> Bool
isNotNumber = not . isNotNumber


isNotPunctuation :: Char -> Bool
isNotPunctuation = not . isPunctuation


isNotSymbol :: Char -> Bool
isNotSymbol = not . isSymbol


isNotSeparator :: Char -> Bool
isNotSeparator = not . isSeparator


isNotAscii :: Char -> Bool
isNotAscii = not . isAscii


isNotLatin1 :: Char -> Bool
isNotLatin1 = not . isLatin1


isNotAsciiUpper :: Char -> Bool
isNotAsciiUpper = not . isAsciiUpper


isNotAsciiLower :: Char -> Bool
isNotAsciiLower = not . isAsciiLower
