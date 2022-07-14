module SwissRobot.Writer (
      appendField, appendListToTuple, appendQueryParams, appendShowTuple, appendToRow, appendTupleToList, 
      incrementWord,
      repeatWord,
      

      ) where

import           Data.List.Swiss (chunksOf, replaceOnce)

import           Data.List (intercalate, replicate, transpose)




-- | > incrementWord "(Show x)" 6 -> "(Show x1, Show x2, Show x3, Show x4, Show x5, Show x6)"
--   > incrementWord " x " 4 -> " x1, x2, x3, x4 "  --see spaces around x
--   > incrementWord "[fx]" 3 -> "[fx1, fx2, fx3] "
incrementWord :: String -> Int -> String
incrementWord str n = open_mark ++ (intercalate ", " $ zipWith (++) word_list num_list) ++ close_mark
                      where word_list = replicate n . reverse . drop 1 . reverse . drop 1 $ str
                            num_list = map show [1..n]
                            open_mark = take 1 str
                            close_mark = take 1 . reverse $ str


-- | > repeatWord "(Nothing)" 5       -> "(Nothing, Nothing, Nothing, Nothing, Nothing)"
--   > repeatWord "[1]" 4             -> "[1, 1, 1, 1]"
--   > repeatWord " Maybe Int " 3     -> " Maybe Int, Maybe Int, Maybe Int "  --see spaces around the word
repeatWord :: String -> Int -> String
repeatWord str n = open_mark ++ (intercalate ", " word_list) ++ close_mark
                   where word_list = replicate n . reverse . drop 1 . reverse . drop 1 $ str
                         open_mark = take 1 str
                         close_mark = take 1 . reverse $ str
                            


----------------------------------------------------------------------------------------------

-- | compare Prelude Library Show instance
composeShowTuple :: Int -> String
composeShowTuple i = concat [ "\n-- | Show Tuple", i' 
                            , "\ninstance ", incrementWord "(Show x)" i
                            , "\n        => Show ", incrementWord "(x)" i, " where"
                            , "\n  showsPrec _ ", incrementWord "(x)" i, " s"
                            , "\n        = show_tuple ", incrementWord "[shows x]" i, " s"
                            , "\n\n" ]
                            where i' = show i

-- |  official pkg only have 1 to 15, max size is 64-tuple (GHC 9.2)
appendShowTuple :: FilePath -> IO ()
appendShowTuple path = mapM_ (\i-> appendFile path (composeShowTuple i)) [16..64]


-- | for postgresql-simple ToField instance declaration, official pkg only have 1 to 10
composeToRow :: Int -> String
composeToRow i = concat [ "\n-- | ToRow", i'
                        , "\ninstance ", incrementWord "(ToField x)" i
                        , "\n        => ToRow ", incrementWord "(x)" i, " where"
                        , "\n  toRow ", incrementWord "(x)" i
                        , "\n        = ", incrementWord "[toField x]" i
                        , "\n\n" ]
                        where i' = show i

-- | for postgresql-simple ToField instance declaration, official pkg only have 1 to 10
appendToRow :: FilePath -> IO ()
appendToRow path = mapM_ (\i -> appendFile path (composeToRow i)) [11..64]


-- | for mysql-simple QueryParams instance declaration, official pkg only have 2 to 24
composeQueryParams :: Int -> String
composeQueryParams i = concat [ "\n-- | QueryParams", i'
                        , "\ninstance ", incrementWord "(Param x)" i
                        , "\n        => QueryParams ", incrementWord "(x)" i, " where"
                        , "\n  renderParams ", incrementWord "(x)" i
                        , "\n        = ", incrementWord "[render x]" i
                        , "\n\n" ]
                        where i' = show i

-- | for mysql-simple QueryParams instance declaration, official pkg only have 2 to 24
appendQueryParams :: FilePath -> IO ()
appendQueryParams path = mapM_ (\i -> appendFile path (composeQueryParams i)) [25..64]






composeListToTuple :: Int -> String
composeListToTuple i = concat [ "\n", "listToTuple", i', " :: [a] -> ", repeatWord "(Maybe a)" i
                              , "\n", "listToTuple", i', " ", incrementWord "[x]" i, " ="
                              , "\n", "    ", incrementWord "(Just x)" i
                              , "\n", "listToTuple", i', " _ = ", repeatWord "(Nothing)" i
                              , "\n\n" ]
                              where i' = show i

appendListToTuple :: FilePath -> IO ()
appendListToTuple path = mapM_ (\i -> appendFile path (composeListToTuple i)) [2..64]  -- 2 to 64 


composeTupleToList :: Int -> String
composeTupleToList i = concat [ "\n", "tupleToList", i', " :: ", incrementWord "(Show x)" i
                              , "\n                 => ", incrementWord "(x)" i, " -> [Maybe String]"
                              , "\n", "tupleToList", i', " ", incrementWord "(x)" i, " ="
                              , "\n", "    ", incrementWord "[readS x]" i
                              , "\n\n" ]
                              where i' = show i


appendTupleToList :: FilePath -> IO ()
appendTupleToList path = mapM_ (\i -> appendFile path (composeTupleToList i)) [2..64]  -- 2 to 64 


-- | for postgresql-simple ToField instance declaration
composeField :: Int -> Int -> String
composeField fieldnum i = concat
                      [ "\n-- | Field", fieldstr, "_", i'
                      , "\ninstance Field", fieldstr, " ", incrementWord "(x)" i, " "
                      , replacex $ incrementWord "(x)" i, " ", xn, " ", xn', " where"
                      , "\n    _", fieldstr, " k ~", incrementWord "(x)" i, " ="
                      , "\n        ", "k ", xn, " <&> \\", xn', " -> "
                      , replacex $ incrementWord "(x)" i
                      , "\n\n" ]
                      where i' = show i
                            fieldstr = show fieldnum
                            xn = "x" ++ fieldstr
                            xn'= xn ++ "\'"
                            replacex s = replaceOnce xn xn' s

-- | for postgresql-simple ToField instance declaration, official postgresql-simple package has 1 to 9
appendField :: FilePath -> Int -> IO ()
appendField path n = mapM_ (\i -> appendFile path (composeField n i)) [10..64]






    