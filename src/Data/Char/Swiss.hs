
module Data.Char.Swiss 
    (isEnglish,
    notAlpha, notAlphaNum, notAscii, notAsciiLower, notAsciiUpper, 
    notControl,
    notDigit,
    notEnglish,
    notHexDigit,
    notLatin1, notLetter, notLower,
    notMark,
    notNumber,
    notOctDigit,
    notPrint, notPunctuation,
    notSeparator, notSpace, notSymbol,
    notUpper,
    )where

import Data.Char (isControl, isSpace, isLower, isUpper, isAlpha, isAlphaNum, isPrint
    , isDigit, isOctDigit, isHexDigit, isLetter, isMark, isNumber, isPunctuation
    , isSymbol, isSeparator, isAscii, isLatin1, isAsciiUpper, isAsciiLower)

-- | 52 uppercase and lowercase English letters
isEnglish :: Char -> Bool
isEnglish c = isAsciiLower c || isAsciiUpper c

notAlpha :: Char -> Bool
notAlpha = not . isAlpha


notAlphaNum :: Char -> Bool
notAlphaNum = not . isAlphaNum



notAscii :: Char -> Bool
notAscii = not . isAscii


notAsciiLower :: Char -> Bool
notAsciiLower = not . isAsciiLower


notAsciiUpper :: Char -> Bool
notAsciiUpper = not . isAsciiUpper



notControl :: Char -> Bool
notControl = not . isControl

notDigit :: Char -> Bool
notDigit = not . isDigit


notEnglish :: Char -> Bool
notEnglish = not . isEnglish


notHexDigit :: Char -> Bool
notHexDigit = not . isHexDigit


notLatin1 :: Char -> Bool
notLatin1 = not . isLatin1


notLetter :: Char -> Bool
notLetter = not . isLetter



notLower :: Char -> Bool
notLower = not . isLower


notMark :: Char -> Bool
notMark = not . isMark


notNumber :: Char -> Bool
notNumber = not . isNumber


notOctDigit :: Char -> Bool
notOctDigit = not . isOctDigit



notPrint :: Char -> Bool
notPrint = not . isPrint

notPunctuation :: Char -> Bool
notPunctuation = not . isPunctuation



notSeparator :: Char -> Bool
notSeparator = not . isSeparator

notSpace :: Char -> Bool
notSpace = not . isSpace


notSymbol :: Char -> Bool
notSymbol = not . isSymbol



notUpper :: Char -> Bool
notUpper = not . isUpper



