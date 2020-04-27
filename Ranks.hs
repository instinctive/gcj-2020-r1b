-- Google Code Jam 2020 Round 1b - Join the Ranks
-- https://codingcompetitions.withgoogle.com/codejam/round/000000000019fef2/00000000002d5b64
-- vim: foldmethod=marker

-- pragmas, imports, and utilities {{{1
{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative ( (<|>)      )
import Control.Monad       ( forM_      )
import Data.List           ( find       )
import Data.Monoid         ( (<>)       )
import Data.Text           ( Text       )
import Debug.Trace         ( traceShowM )
import Text.Printf         ( printf     )
import qualified Data.Text as T

getReadList :: Read a => IO [a]
getReadList = map read . words <$> getLine

main :: IO ()
main = do
    [t] <- getReadList :: IO [Int]
    forM_ [1..t] $ \i -> do
        printf "Case #%d: " i
        docase

-- solution {{{1

docase :: IO ()
docase = do
    [r,s] <- getReadList :: IO [Int]
    let moves = solve r s
    printf "%d\n" (length moves)
    mapM_ out moves
  where
    out (a,b) = printf "%d %d\n" a b

type Move = (Int,Int)

move :: Move -> Text -> Text
move (a,b) t = bb <> aa <> cc where
    (aa,bbcc) = T.splitAt a t
    (bb,cc)   = T.splitAt b bbcc

next :: Text -> Maybe Move
next t = do
    let top = T.index t 0  -- the top run
    chg <- fwd (/=top) 1   -- top run changes
    nxt <- fwd (==top) chg -- next top run
    let more = do
            end <- fwd (/=top) nxt            -- next top run changes
            prv <- bwd (== T.index t end) nxt -- end of previous match
            let m = (prv+1, end-prv-1)
            -- traceShowM (t,m,chg,nxt,end,prv)
            pure m
    more <|> pure (chg,nxt-chg) -- top and bottom are the same
  where
    len = T.length t
    fwd p i = find q [i..len-1] where q = p . T.index t
    bwd p i = find q [i,i-1..0] where q = p . T.index t

solve :: Int -> Int -> [Move]
solve r s = go [] deck where
    len = r*s
    deck = T.replicate s $ T.pack $ take r ['0'..]
    go mm t = case next t of
        Nothing -> reverse mm
        Just m -> go (m:mm) (move m t)
