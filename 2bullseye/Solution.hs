-- bullseye
-- vim: foldmethod=marker
-- Google CodeJam boilerplate {{{
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.Bits
import Data.Bool
import Data.Char
import Data.Foldable
import Data.Functor
import Data.List
import Data.Maybe
import Data.Tuple
import Data.Traversable
import Debug.Trace
import System.IO
import Text.Printf

import Data.Array         ( Array    )
import Data.Array.ST      ( STUArray )
import Data.Array.Unboxed ( UArray   )
import Data.Map.Strict    ( Map      )
import Data.Set           ( Set      )
import Data.Text          ( Text     )

import qualified Data.Array         as A
import qualified Data.Array.ST      as STA
import qualified Data.Array.Unboxed as UA
import qualified Data.Map.Strict    as M
import qualified Data.Set           as S
import qualified Data.Text          as T
import qualified Data.Text.IO       as T

getRead :: Read a => IO a
getRead = read <$> getLine

getReadList :: Read a => IO [a]
getReadList = map read . words <$> getLine

-- }}}

main :: IO ()
main = do
    [t,a,b] <- getReadList :: IO [Int]
    -- traceShowM (t,a,b)
    replicateM_ (t-1) $ docase a b

-- Find the minimum value in range [lo..hi] that satisfies the test.
-- The test must be true over a single continuous range.
-- 'hi' must initially satisfy the test.
findMinM :: Monad m => (Int -> m Bool) -> Int -> Int -> m Int
findMinM test = go where
    go lo hi
        | lo == hi = pure lo
        | otherwise =
            let md = div (lo + hi) 2 in
            test md >>= bool (go (md+1) hi) (go lo md)

type Pt = (Int,Int)
size = 10^9 :: Int

printpt :: Pt -> IO ()
printpt (x,y) = printf "%d %d\n" x y >> hFlush stdout

bullseye :: Pt -> IO ()
bullseye pt@(x,y) = do
    traceShowM ("bullseye",pt)
    go (pt:cands)
  where
    cands =
        [ (x',y')
        | x' <- [x-1..x+1]
        , y' <- [y-1..y+1]
        , x /= x' || y /= y'
        , x' >= -size && x' <= size
        , y' >= -size && y' <= size
        ]
    go [] = error $ printf "bullseye %s: missed all" (show pt)
    go (q:qq) = do
        printpt q
        getLine >>= \case
            "CENTER" -> pure ()
            "MISS" -> go qq
            "HIT" -> go qq
            s -> error $ printf "bullseye %s: %s" (show pt) s

checkHit :: Pt -> IO Bool
checkHit pt = do
    traceShowM ("checkHit",pt)
    printpt pt
    getLine >>= pure . check
  where
    check :: String -> Bool
    check = \case
        "MISS"   -> False
        "HIT"    -> True
        "CENTER" -> True
        s        -> error $ printf "checkHit %s: %s" (show pt) s

initTarget :: IO Pt
initTarget = do
    hits <- mapM checkHit cands
    case find fst (zip hits cands) of
        Nothing -> error "target not found"
        Just (_,pt) -> pure pt
  where
    h = div size 2
    cands = [
      (-h,-h),        (-h, h),
               (0,0),
      ( h,-h),        ( h, h) ]

docase :: Int -> Int -> IO ()
docase a b = do
    traceShowM "starting"
    (x,y) <- initTarget
    -- traceShowM ("initTarget",(x,y))
    xlo <- findMinM (checkHit . (,y)) (-size) x
    ylo <- findMinM (checkHit . (x,)) (-size) y
    xhi <- negate <$> findMinM (checkHit . (,y) . negate) (-size) (-x)
    yhi <- negate <$> findMinM (checkHit . (x,) . negate) (-size) (-y)
    -- traceShowM ((xlo,xhi),(ylo,yhi))
    let xmd = div (xlo + xhi) 2
    let ymd = div (ylo + yhi) 2
    bullseye (xmd,ymd)
