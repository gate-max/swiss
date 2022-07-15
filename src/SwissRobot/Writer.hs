module SwissRobot.Writer (
      appendField, appendListToTuple, appendQueryParams, appendShowTuple, appendToRow, appendTupleToList, 
      composeField, composeListToTuple, composeQueryParams, composeShowTuple, composeToRow, composeTupleToList,
      incrementEnclosed,
      replicateEnclosed,
      

      ) where

import           Data.List.Swiss (chunksOf, replaceOnce)

import           Data.List (intercalate, replicate, transpose)




-- | > incrementEnclosed 6 "(Show x)"     -> "(Show x1, Show x2, Show x3, Show x4, Show x5, Show x6)"
--   > incrementEnclosed 4 " x "          -> " x1, x2, x3, x4 "  --see spaces around x
--   > incrementEnclosed 3 "[fx]"         -> "[fx1, fx2, fx3] "
incrementEnclosed :: Int -> String -> String
incrementEnclosed i str = open_mark ++ (intercalate ", " $ zipWith (++) word_list num_list) ++ close_mark
    where word_list, num_list :: [String]
          word_list = replicate i . reverse . drop 1 . reverse . drop 1 $ str
          num_list = map show [1..i]
          open_mark, close_mark :: String
          open_mark = take 1 str
          close_mark = take 1 . reverse $ str


-- | > replicateEnclosed 5 "(Nothing)"       -> "(Nothing, Nothing, Nothing, Nothing, Nothing)"
--   > replicateEnclosed 4 "[1]"             -> "[1, 1, 1, 1]"
--   > replicateEnclosed 3 " Maybe Int "     -> " Maybe Int, Maybe Int, Maybe Int "  --see spaces around the word
replicateEnclosed :: Int -> String -> String
replicateEnclosed i str = open_mark ++ (intercalate ", " word_list) ++ close_mark
    where word_list :: [String]
          word_list = replicate i . reverse . drop 1 . reverse . drop 1 $ str
          open_mark, close_mark :: String
          open_mark = take 1 str
          close_mark = take 1 . reverse $ str
      


----------------------------------------------------------------------------------------------

-- | compare Prelude Library Show instance
-- > ghci> map composeShowTuple [2..5]
composeShowTuple :: Int -> String
composeShowTuple i = concat [ "\n-- | Show ", j, "-Tuple" 
                            , "\ninstance ", incrementEnclosed i "(Show x)"
                            , "\n        => Show ", incrementEnclosed i "(x)", " where"
                            , "\n  showsPrec _ ", incrementEnclosed i "(x)", " s"
                            , "\n        = show_tuple ", incrementEnclosed i "[shows x]", " s"
                            , "\n\n" ]
                            where j = show i

-- |  official pkg only have 1 to 15, max size is 64-tuple (GHC 9.2)
appendShowTuple :: FilePath -> IO ()
appendShowTuple path = mapM_ (\i-> appendFile path (composeShowTuple i)) [16..64]


-- | for postgresql-simple ToField instance declaration, official pkg only have 1 to 10
composeToRow :: Int -> String
composeToRow i = concat [ "\n-- | ToRow", j
                        , "\ninstance ", incrementEnclosed i "(ToField x)"
                        , "\n        => ToRow ", incrementEnclosed i "(x)", " where"
                        , "\n  toRow ", incrementEnclosed i "(x)"
                        , "\n        = ", incrementEnclosed i "[toField x]"
                        , "\n\n" ]
                        where j = show i

-- | for postgresql-simple ToField instance declaration, official pkg only have 1 to 10
appendToRow :: FilePath -> IO ()
appendToRow path = mapM_ (\i -> appendFile path (composeToRow i)) [11..64]


-- | for mysql-simple QueryParams instance declaration, official pkg only have 2 to 24
composeQueryParams :: Int -> String
composeQueryParams i = concat [ "\n-- | QueryParams", j
                        , "\ninstance ", incrementEnclosed i "(Param x)"
                        , "\n        => QueryParams ", incrementEnclosed i "(x)", " where"
                        , "\n  renderParams ", incrementEnclosed i "(x)"
                        , "\n        = ", incrementEnclosed i "[render x]"
                        , "\n\n" ]
                        where j = show i

-- | for mysql-simple QueryParams instance declaration, official pkg only have 2 to 24
appendQueryParams :: FilePath -> IO ()
appendQueryParams path = mapM_ (\i -> appendFile path (composeQueryParams i)) [25..64]






composeListToTuple :: Int -> String
composeListToTuple i = concat [ "\n", "listToTuple", j, " :: [a] -> ", replicateEnclosed i "(Maybe a)"
                              , "\n", "listToTuple", j, " ", incrementEnclosed i "[x]", " ="
                              , "\n", "    ", incrementEnclosed i "(Just x)"
                              , "\n", "listToTuple", j, " _ = ", replicateEnclosed i "(Nothing)"
                              , "\n\n" ]
                              where j = show i

appendListToTuple :: FilePath -> IO ()
appendListToTuple path = mapM_ (\i -> appendFile path (composeListToTuple i)) [2..64]  -- 2 to 64 


composeTupleToList :: Int -> String
composeTupleToList i = concat [ "\n", "tupleToList", j, " :: ", incrementEnclosed i "(Show x)"
                              , "\n                 => ", incrementEnclosed i "(x)", " -> [Maybe String]"
                              , "\n", "tupleToList", j, " ", incrementEnclosed i "(x)", " ="
                              , "\n", "    ", incrementEnclosed i "[readS x]"
                              , "\n\n" ]
                              where j = show i


appendTupleToList :: FilePath -> IO ()
appendTupleToList path = mapM_ (\i -> appendFile path (composeTupleToList i)) [2..64]  -- 2 to 64 


-- | for postgresql-simple ToField instance declaration
composeField :: Int -> Int -> String
composeField fieldnum i = concat
                      [ "\n-- | Field", fieldstr, "_", j
                      , "\ninstance Field", fieldstr, " ", incrementEnclosed i "(x)", " "
                      , replacex $ incrementEnclosed i "(x)", " ", xn, " ", xn', " where"
                      , "\n    _", fieldstr, " k ~", incrementEnclosed i "(x)", " ="
                      , "\n        ", "k ", xn, " <&> \\", xn', " -> "
                      , replacex $ incrementEnclosed i "(x)"
                      , "\n\n" ]
                      where j = show i
                            fieldstr = show fieldnum
                            xn = "x" ++ fieldstr
                            xn'= xn ++ "\'"
                            replacex s = replaceOnce xn xn' s

-- | for postgresql-simple ToField instance declaration, official postgresql-simple package has 1 to 9
appendField :: FilePath -> Int -> IO ()
appendField path n = mapM_ (\i -> appendFile path (composeField n i)) [10..64]






    