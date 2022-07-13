module Data.Tuple.Swiss(
    module Data.Tuple,
    module GHC.Show.Swiss,
    fmapPair,
    fmapTuple2,
    fmapTuple3,
    fmapTuple4,
    fmapTuple5,
    fmapTuple6,
    fmapTuple7,
    fmapTuple8,
    fmapTuple9,
    fmapTuple10,
    fmapTuple11,
    fmapTuple12,
    mapPair,
    mapTuple2,
    mapTuple3,
    mapTuple4,
    mapTuple5,
    mapTuple6,
    mapTuple7,
    mapTuple8,
    mapTuple9,
    mapTuple10,
    mapTuple11,
    mapTuple12,    
    ) where
    


import           Control.DeepSeq (NFData)
import           Control.Parallel.Strategies (rdeepseq, runEval) --parallel

import           Data.Tuple
import           GHC.Show.Swiss



fmapPair :: (NFData n, NFData o) => (a -> n) -> (b -> o) -> (a,b) -> (n,o)
fmapPair f g (x,y) = runEval $ do
                    x' <- rdeepseq (f x)
                    y' <- rdeepseq (g y)
                    return (x',y')

fmapTuple2 :: (a -> n) -> (b -> o) -> (a,b) -> (n,o)
fmapTuple2 f1 f2 (a,b) = (f1 a, f2 b)

fmapTuple3 :: (a -> n) -> (b -> o) -> (c -> p) -> (a,b,c) -> (n,o,p)
fmapTuple3 f1 f2 f3 (a,b,c) = (f1 a, f2 b, f3 c)

fmapTuple4 :: (a -> n) -> (b -> o) -> (c -> p) -> (d -> q) -> (a,b,c,d) -> (n,o,p,q)
fmapTuple4 f1 f2 f3 f4 (a,b,c,d) = (f1 a, f2 b, f3 c, f4 d)

fmapTuple5 :: (a -> n) -> (b -> o) -> (c -> p) -> (d -> q) -> (e -> r) -> (a,b,c,d,e) -> (n,o,p,q,r)
fmapTuple5 f1 f2 f3 f4 f5 (a,b,c,d,e) = (f1 a, f2 b, f3 c, f4 d, f5 e)

fmapTuple6 :: (a -> n) -> (b -> o) -> (c -> p) -> (d -> q) -> (e -> r) -> (f -> s) -> (a,b,c,d,e,f) -> (n,o,p,q,r,s)
fmapTuple6 f1 f2 f3 f4 f5 f6 (a,b,c,d,e,f) = (f1 a, f2 b, f3 c, f4 d, f5 e, f6 f)

fmapTuple7 :: (a -> n) -> (b -> o) -> (c -> p) -> (d -> q) -> (e -> r) -> (f -> s) -> (g -> t) -> (a,b,c,d,e,f,g) -> (n,o,p,q,r,s,t)
fmapTuple7 f1 f2 f3 f4 f5 f6 f7 (a,b,c,d,e,f,g) = (f1 a, f2 b, f3 c, f4 d, f5 e, f6 f, f7 g)

fmapTuple8 :: (a -> n) -> (b -> o) -> (c -> p) -> (d -> q) -> (e -> r) -> (f -> s) -> (g -> t) -> (h -> u) -> (a,b,c,d,e,f,g,h) -> (n,o,p,q,r,s,t,u)
fmapTuple8 f1 f2 f3 f4 f5 f6 f7 f8 (a,b,c,d,e,f,g,h) = (f1 a, f2 b, f3 c, f4 d, f5 e, f6 f, f7 g, f8 h)

fmapTuple9 :: (a -> n) -> (b -> o) -> (c -> p) -> (d -> q) -> (e -> r) -> (f -> s) -> (g -> t) -> (h -> u) -> (i -> v) -> (a,b,c,d,e,f,g,h,i) -> (n,o,p,q,r,s,t,u,v)
fmapTuple9 f1 f2 f3 f4 f5 f6 f7 f8 f9 (a,b,c,d,e,f,g,h,i) = (f1 a, f2 b, f3 c, f4 d, f5 e, f6 f, f7 g, f8 h, f9 i)

fmapTuple10 :: (a -> n) -> (b -> o) -> (c -> p) -> (d -> q) -> (e -> r) -> (f -> s) -> (g -> t) -> (h -> u) -> (i -> v) -> (j -> w) -> (a,b,c,d,e,f,g,h,i,j) -> (n,o,p,q,r,s,t,u,v,w)
fmapTuple10 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 (a,b,c,d,e,f,g,h,i,j) = (f1 a, f2 b, f3 c, f4 d, f5 e, f6 f, f7 g, f8 h, f9 i, f10 j)

fmapTuple11 :: (a -> n) -> (b -> o) -> (c -> p) -> (d -> q) -> (e -> r) -> (f -> s) -> (g -> t) -> (h -> u) -> (i -> v) -> (j -> w) -> (k -> x) -> (a,b,c,d,e,f,g,h,i,j,k) -> (n,o,p,q,r,s,t,u,v,w,x)
fmapTuple11 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 (a,b,c,d,e,f,g,h,i,j,k) = (f1 a, f2 b, f3 c, f4 d, f5 e, f6 f, f7 g, f8 h, f9 i, f10 j, f11 k)

fmapTuple12 :: (a -> n) -> (b -> o) -> (c -> p) -> (d -> q) -> (e -> r) -> (f -> s) -> (g -> t) -> (h -> u) -> (i -> v) -> (j -> w) -> (k -> x) -> (l -> y) -> (a,b,c,d,e,f,g,h,i,j,k,l) -> (n,o,p,q,r,s,t,u,v,w,x,y)
fmapTuple12 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 (a,b,c,d,e,f,g,h,i,j,k,l) = (f1 a, f2 b, f3 c, f4 d, f5 e, f6 f, f7 g, f8 h, f9 i, f10 j, f11 k, f12 l)



------------------------------------------------------------------------
mapPair :: NFData b => (a -> b) -> (a,a) -> (b,b)
mapPair f (x,y) = runEval $ do
                    x' <- rdeepseq (f x)
                    y' <- rdeepseq (f y)
                    return (x',y')


mapTuple2 :: (a -> b) -> (a,a) -> (b,b)
mapTuple2 f (z,y) = (f z, f y)

mapTuple3 :: (a -> b) -> (a,a,a) -> (b,b,b)
mapTuple3 f (z,y,x) = (f z, f y, f x)

mapTuple4 :: (a -> b) -> (a,a,a,a) -> (b,b,b,b)
mapTuple4 f (z,y,x,w) = (f z, f y, f x, f w)

mapTuple5 :: (a -> b) -> (a,a,a,a,a) -> (b,b,b,b,b)
mapTuple5 f (z,y,x,w,v) = (f z, f y, f x, f w, f v)

mapTuple6 :: (a -> b) -> (a,a,a,a,a,a) -> (b,b,b,b,b,b)
mapTuple6 f (z,y,x,w,v,u) = (f z, f y, f x, f w, f v, f u)

mapTuple7 :: (a -> b) -> (a,a,a,a,a,a,a) -> (b,b,b,b,b,b,b)
mapTuple7 f (z,y,x,w,v,u,t) = (f z, f y, f x, f w, f v, f u, f t)

mapTuple8 :: (a -> b) -> (a,a,a,a,a,a,a,a) -> (b,b,b,b,b,b,b,b)
mapTuple8 f (z,y,x,w,v,u,t,s) = (f z, f y, f x, f w, f v, f u, f t, f s)

mapTuple9 :: (a -> b) -> (a,a,a,a,a,a,a,a,a) -> (b,b,b,b,b,b,b,b,b)
mapTuple9 f (z,y,x,w,v,u,t,s,r) = (f z, f y, f x, f w, f v, f u, f t, f s, f r)

mapTuple10 :: (a -> b) -> (a,a,a,a,a,a,a,a,a,a) -> (b,b,b,b,b,b,b,b,b,b)
mapTuple10 f (z,y,x,w,v,u,t,s,r,q) = (f z, f y, f x, f w, f v, f u, f t, f s, f r, f q)

mapTuple11 :: (a -> b) -> (a,a,a,a,a,a,a,a,a,a,a) -> (b,b,b,b,b,b,b,b,b,b,b)
mapTuple11 f (z,y,x,w,v,u,t,s,r,q,p) = (f z, f y, f x, f w, f v, f u, f t, f s, f r, f q, f p)

mapTuple12 :: (a -> b) -> (a,a,a,a,a,a,a,a,a,a,a,a) -> (b,b,b,b,b,b,b,b,b,b,b,b)
mapTuple12 f (z,y,x,w,v,u,t,s,r,q,p,o) = (f z, f y, f x, f w, f v, f u, f t, f s, f r, f q, f p, f o)



