-- Google Code Jam 2020 Round 1b - Join the Ranks
-- https://codingcompetitions.withgoogle.com/codejam/round/000000000019fef2/00000000002d5b64
-- vim: foldmethod=marker

-- pragmas, imports, and utilities {{{1

module Main where

import Control.Monad ( forM_  )
import Text.Printf   ( printf )

getReadList :: Read a => IO [a]
getReadList = map read . words <$> getLine

-- main {{{1

main :: IO ()
main = do
    [t] <- getReadList :: IO [Int]
    forM_ [1..t] $ \i -> do
        printf "Case #%d: " i
        docase

docase :: IO ()
docase = do
    [r,s] <- getReadList :: IO [Int]
    let moves = solve r s
    printf "%d\n" (length moves)
    mapM_ out moves
  where
    out (a,b) = printf "%d %d\n" a b

-- solve {{{1

solve :: Int -> Int -> [(Int,Int)]
solve r s = go [] r where
    go mm 1 = reverse mm
    go mm r = go (reverse mm' ++ mm) (r-1) where
        mm' = 
            [ (a,r-1)
            | i <- [0..s-2]
            , let a = r * (s-1) - i
            ]
