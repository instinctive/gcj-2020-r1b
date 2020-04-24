-- Google Code Jam 2020 Round 1b - Blindfolded Bullseye
-- https://codingcompetitions.withgoogle.com/codejam/round/000000000019fef2/00000000002d5b63
-- vim: foldmethod=marker

-- pragmas, imports, and utilities {{{1

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE PatternSynonyms   #-}
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

type Pt        = (Int,Int)

-- If the judge responds CENTER or WRONG, then we are done,
-- and the Left result will break out of our monad.
-- The Right result is True iff the query point was a hit.
type Answer    = Either Bool Bool

pattern CENTER = Left True
pattern WRONG  = Left False
pattern HIT    = Right True
pattern MISS   = Right False

parse :: String -> Answer
parse = \case
    "CENTER" -> CENTER
    "WRONG"  -> WRONG
    "HIT"    -> HIT
    "MISS"   -> MISS
    s -> error $ printf "invalid answer: %s" (show s)

-- The returned Bool is True iff we hit the CENTER,
-- and False iff we received WRONG.
type Return = ExceptT Bool

class Monad m => AnswerM m where
    isHit :: Pt -> Return m Bool

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

test :: Monad m => Int -> (Int,Int) -> m ()
test r ctr = runReaderT go env where
    go = do
        final <- runExceptT $ solve (2*r)
        traceShowM final
        pure ()
    env = Target (r,ctr)

newtype Target = Target (Int,(Int,Int)) deriving Show

instance Monad m => AnswerM (ReaderT Target m) where 
    isHit (x,y) = do
        Target (r, (x0,y0)) <- lift ask
        let ans = check (x-x0) (y-y0) r
        traceShowM ((x,y),ans)
        except ans
      where
        check 0 0 _ = CENTER
        check u v r 
            | u*u + v*v > r*r = MISS
            | otherwise       = HIT

-- solve {{{1

solve :: AnswerM m => Int -> Return m ()
solve size = findTarget size >>= \case
    Nothing -> error "could not find target"
    Just (x,y) -> do
        xlo <- findMinM (isHit . (,y)) (-size) x
        ylo <- findMinM (isHit . (x,)) (-size) y
        xhi <- negate <$> findMinM (isHit . (,y) . negate) (-size) (-x)
        yhi <- negate <$> findMinM (isHit . (x,) . negate) (-size) (-y)
        let xctr = div (xlo + xhi) 2
        let yctr = div (ylo + yhi) 2
        void $ isHit (xctr,yctr) -- should break from Outer on CENTER result
        error "missed on bullseye hit"

findTarget :: AnswerM m => Int -> Return m (Maybe Pt)
findTarget size = go $ (0,0) : quads
  where
    quads = (,) <$> [-h,h] <*> [-h,h] where h = div size 2
    go [] = pure Nothing
    go (pt:more) =
        ifM (isHit pt) (pure $ Just pt) (go more)
