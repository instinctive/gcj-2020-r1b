-- Google Code Jam 2020 Round 1b - Blindfolded Bullseye
-- https://codingcompetitions.withgoogle.com/codejam/round/000000000019fef2/00000000002d5b63
-- vim: foldmethod=marker

-- pragmas, imports, and utilities {{{1

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}

module Main where

import Control.Monad              ( replicateM_, void           )
import Control.Monad.Trans.Class  ( lift                        )
import Control.Monad.Trans.Except ( ExceptT, runExceptT, except )
import Control.Monad.Trans.Reader ( ReaderT, runReaderT, ask    )
import Debug.Trace                ( traceShowM                  )
import System.IO                  ( hFlush, stdout              )
import Text.Printf                ( printf                      )

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM bm tm em = bm >>= \b -> if b then tm else em

findMinM :: Monad m => (Int -> m Bool) -> Int -> Int -> m Int
findMinM test = go where
    go lo hi
        | lo == hi = pure lo
        | otherwise = do
            let md = div (lo + hi) 2
            ifM (test md) (go lo md) (go (md+1) hi)

getReadList :: Read a => IO [a]
getReadList = map read . words <$> getLine

-- types {{{1

type Pt = (Int,Int)

type Answer = Either Bool Bool
pattern CENTER = Left True
pattern WRONG  = Left False
pattern HIT    = Right True
pattern MISS   = Right False

-- The monad for interactions with the judge. We ask about a point,
-- and the response is in `ExceptT e m a`, where `e` is the `Left` of
-- the `Answer` type and `a` is the `Right` of the `Answer` type.
class Monad m => AnswerM m where
    isHit :: Pt -> ExceptT Bool m Bool

-- parse and pretty-print {{{2

parse :: String -> Answer
parse = \case
    "CENTER" -> CENTER
    "WRONG"  -> WRONG
    "HIT"    -> HIT
    "MISS"   -> MISS
    s -> error $ printf "invalid answer: %s" (show s)

pretty :: Answer -> String
pretty = \case
    CENTER -> "CENTER"
    WRONG  -> "WRONG"
    HIT    -> "HIT"
    MISS   -> "MISS"

-- main {{{1

kSize = 10^9 :: Int -- size is constant in codejam problem

main :: IO ()
main = do
    (t:_) <- getReadList :: IO [Int]
    replicateM_ t $ runExceptT $ solve kSize

instance AnswerM IO where
    isHit pt = lift (query pt) >>= except

query :: Pt -> IO Answer
query (x,y) = do
    printf "%d %d\n" x y
    hFlush stdout
    parse <$> getLine

-- test {{{1

newtype Target = Target (Int,Int,(Int,Int)) deriving Show

test :: Monad m => Int -> Int -> (Int,Int) -> m ()
test size r ctr = runReaderT go env where
    go = void $ runExceptT $ solve size
    env = Target (size,r,ctr)

instance Monad m => AnswerM (ReaderT Target m) where 
    isHit pt = do
        ans <- inTarget pt <$> lift ask
        traceShowM (pt, pretty ans)
        except ans

inTarget :: Pt -> Target -> Answer
inTarget (x,y) (Target (size,r,(x0,y0)))
    | x < -size || x > size || y < -size || y > size = WRONG
    | u == 0 && v == 0 = CENTER
    | u*u + v*v > r*r  = MISS
    | otherwise        = HIT
  where
    u = x - x0
    v = y - y0

-- solve {{{1

type Solve a = forall m . AnswerM m => Int -> ExceptT Bool m a

solve :: Solve ()
solve size = do
    (x,y) <- findTarget size
    xlo <- findMinM (isHit . (,y)) (-size) x
    ylo <- findMinM (isHit . (x,)) (-size) y
    xhi <- negate <$> findMinM (isHit . (,y) . negate) (-size) (-x)
    yhi <- negate <$> findMinM (isHit . (x,) . negate) (-size) (-y)
    let xctr = div (xlo + xhi) 2
    let yctr = div (ylo + yhi) 2
    void $ isHit (xctr,yctr) -- will leave ExceptT on CENTER
    error "missed on bullseye hit"

findTarget :: Solve Pt
findTarget size = go $ (0,0) : quads
  where
    quads = (,) <$> [-h,h] <*> [-h,h] where h = div size 2
    go [] = error "could not find target" -- should not be possible
    go (pt:more) = ifM (isHit pt) (pure pt) (go more)
