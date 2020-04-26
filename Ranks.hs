-- Google Code Jam 2020 Round 1b - Join the Ranks
-- https://codingcompetitions.withgoogle.com/codejam/round/000000000019fef2/00000000002d5b64
-- vim: foldmethod=marker

-- pragmas, imports, and utilities {{{1
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad ( forM_         )
import Data.List     ( foldl', group )
import Debug.Trace   ( traceShow     )
import Text.Printf   ( printf        )

getReadList :: Read a => IO [a]
getReadList = map read . words <$> getLine

main :: IO ()
main = do
    [t] <- getReadList :: IO [Int]
    forM_ [1..t] $ \i -> do
        printf "Case #%d: " i
        docase

-- skew heaps {{{1

data Skew a = SkewEmpty | SkewNode a (Skew a) (Skew a)

mkSkew :: Ord a => a -> Skew a
mkSkew a = SkewNode a SkewEmpty SkewEmpty

skewInsert :: Ord a => a -> Skew a -> Skew a
skewInsert a t = skewUnion (mkSkew a) t

skewUnion :: Ord a => Skew a -> Skew a -> Skew a
skewUnion SkewEmpty t2 = t2
skewUnion t1 SkewEmpty = t1
skewUnion t1@(SkewNode a1 l1 r1) t2@(SkewNode a2 l2 r2)
    | a1 <= a2  = SkewNode a1 (skewUnion t2 r1) l1
    | otherwise = SkewNode a2 (skewUnion t1 r2) l2

skewExtract :: Ord a => Skew a -> Maybe (a, Skew a)
skewExtract SkewEmpty = Nothing
skewExtract (SkewNode a l r) = Just (a, skewUnion l r)

-- search {{{1

type Result a = Either Bool a
pattern CUTOFF  = Left True
pattern FAILURE = Left False
pattern SUCCESS a = Right a

data Search a = Search
    { _goal :: a -> Bool
    , _next :: a -> [a]
    }

type SearchFn a = a -> Search a -> Result a

data Heur r a = Heur r a
instance Eq r => Eq (Heur r a) where
    Heur x _ == Heur y _ = x == y
instance Ord r => Ord (Heur r a) where
    Heur x _ <= Heur y _ = x <= y

aStarSearch :: (Show a, Ord r) => (a -> r) -> SearchFn a
aStarSearch heur a Search {..} = go init where
    init = mkSkew $ mk a
    mk a = Heur (heur a) a
    go h = case skewExtract h of
        Nothing -> FAILURE
        Just (Heur _ x, h')
            -- | traceShow x False -> undefined
            | _goal x -> SUCCESS x
            | otherwise -> go $ 
                foldl' (flip skewInsert) h' (mk <$> _next x)

-- solution {{{1

docase :: IO ()
docase = do
    [r,s] <- getReadList :: IO [Int]
    let moves = solve r s
    printf "%d\n" (length moves)
    mapM_ out moves
  where
    out (a,b) = printf "%d %d\n" a b

type State = ([(Int,Int)],[[Int]])

heur :: State -> Int
heur (_,gg) = length gg + inversions where
    inversions = length . filter id $
        zipWith (>) (head <$> gg) (head <$> tail gg)

env :: Search State
env = Search goal next where
    goal (_,gg) = and $ zipWith (<) (head <$> gg) (head <$> tail gg)
    next (mm,gg) =
        [ ((na,nb):mm, gg')
        | let n = length gg
        , a <- [1..n-2]
        , b <- [1..n-1-a]
        , let (aa,xx) = splitAt a gg
        , let (bb,cc) = splitAt b xx
        , let na = length (concat aa)
        , let nb = length (concat bb)
        , let gg' = group . concat $ bb ++ aa ++ cc
        ]

solve :: Int -> Int -> [(Int,Int)]
solve r s = case aStarSearch heur ([],gg) env of
    SUCCESS (mm,_) -> reverse mm
    _ -> error "could not find a solution"
  where
    gg = group . concat $ replicate s [1..r]
