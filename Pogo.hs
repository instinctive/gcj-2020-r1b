-- Google Code Jam 2020 Round 1b - Expogo
-- https://codingcompetitions.withgoogle.com/codejam/round/000000000019fef2/00000000002d5b62
-- vim: foldmethod=marker

-- pragmas, imports, and utilities {{{1
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module Main where

import Control.Monad ( forM_  )
import Text.Printf   ( printf )

getReadList :: Read a => IO [a]
getReadList = map read . words <$> getLine

main :: IO ()
main = do
    [t] <- getReadList :: IO [Int]
    forM_ [1..t] $ \i -> do
        printf "Case #%d: " i
        docase

type Result a = Either Bool a
pattern CUTOFF  = Left True
pattern FAILURE = Left False
pattern SUCCESS a = Right a

data Search a = Search
    { _goal :: a -> Bool
    , _next :: a -> [a]
    }

type SearchFn a = a -> Search a -> Result a

dlSearch :: Int -> SearchFn a
dlSearch d a Search {..} = go False [(0,a)] where
    go cutoff [] = Left cutoff
    go cutoff ((i,x):xx)
        | _goal x   = Right x
        | i >= d    = go True xx
        | otherwise = go cutoff (xx' ++ xx)
      where xx' = (i+1,) <$> _next x

idSearch :: SearchFn a
idSearch a s = go 1 where
    go i = case dlSearch i a s of
        CUTOFF -> go (i+1)
        answer -> answer

-- solution {{{1

docase :: IO ()
docase = do
    [x,y] <- getReadList :: IO [Int]
    case solve (x,y) of
        Nothing -> putStrLn "IMPOSSIBLE"
        Just dd -> putStrLn $ concatMap show dd

data Dir = N | S | E | W deriving (Eq,Show)
type State = ([Dir],(Int,Int))

env :: Search State
env = Search goal next where
    goal (_,(0,0)) = True
    goal _         = False
    next (dd,(x,y))
        | odd x && even y =
            [ (E:dd, (div (x-1) 2, div y 2))
            , (W:dd, (div (x+1) 2, div y 2)) ]
        | even x && odd y =
            [ (N:dd, (div x 2, div (y-1) 2))
            , (S:dd, (div x 2, div (y+1) 2)) ]
        | otherwise = []

solve :: (Int,Int) -> Maybe [Dir]
solve pt = case idSearch ([],pt) env of
    FAILURE        -> Nothing
    SUCCESS (dd,_) -> Just $ reverse dd
