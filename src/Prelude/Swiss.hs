
module Prelude.Swiss (
    (&^&), (|^|),
    ) where


import           Control.Monad (liftM2)


infixr 7 &^& , |^|


-- | > filter (even &^& (>5)) [1..10]            --> [6,8,10] 
--   > filter (even &^& (>5) &^& (<9)) [1..10]  --> [6,8] 
(&^&) :: Monad m => m Bool -> m Bool -> m Bool
(&^&) = liftM2 (&&) 

-- | > filter (even |^| (>5)) [1..10]             --> [2,4,6,7,8,9,10] 
--   > filter (even |^| (>5) |^| (==1)) [1..10]  --> [1,2,4,6,7,8,9,10] 
(|^|) :: Monad m => m Bool -> m Bool -> m Bool
(|^|) = liftM2 (||) 

