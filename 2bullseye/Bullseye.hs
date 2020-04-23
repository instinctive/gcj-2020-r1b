-- Google Code Jam 2020 Round 1b - Blindfolded Bullseye
-- https://codingcompetitions.withgoogle.com/codejam/round/000000000019fef2/00000000002d5b63
-- vim: foldmethod=marker

-- pragmas, imports, and utilities 
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TupleSections        #-}

module Main where

import Control.Monad              ( replicateM_, mzero, void )
import Control.Monad.Trans.Class  ( lift                     )
import Control.Monad.Trans.Maybe  ( MaybeT, runMaybeT        )
import Control.Monad.Trans.Reader ( ReaderT, runReaderT, ask )
import Debug.Trace                ( traceShowM               )
import System.IO                  ( hFlush, stdout           )
import Text.Printf                ( printf                   )
import Text.Read                  ( readMaybe                )

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

-- types 
type Pt = (Int,Int)

data Ans = WRONG | MISS | HIT | CENTER deriving (Eq,Read,Show)

class Monad m => MonadIntf m where
    getInit :: m (Int,Int,Int)
    isHit   :: Pt -> m Ans

instance MonadIntf IO where 
    getInit = do
        [t,a,b] <- getReadList :: IO [Int]
        pure (t,a,b)
    isHit (x,y) = do
        printf "%d %d\n" x y 
        hFlush stdout
        raw <- getLine
        maybe (err raw) pure $ readMaybe raw
      where err = error . printf "bad answer: %s"

-- testing 
test :: Monad m => Int -> (Int,Int) -> m ()
test r ctr = runReaderT go env where
    go = void $ runMaybeT $ solve (2*r)
    env = Target (r,ctr)

newtype Target = Target (Int,(Int,Int)) deriving Show

instance Monad m => MonadIntf (ReaderT Target m) where 
    getInit = pure (1,0,0)
    isHit (x,y) = do
        Target (r, (x0,y0)) <- ask
        let ans = check (x-x0) (y-y0) r
        -- traceShowM ((x,y),ans)
        pure ans
      where
        check 0 0 _ = CENTER
        check u v r 
            | u*u + v*v > r*r = MISS
            | otherwise       = HIT

-- solution 
kSize = 10^9 :: Int -- size is constant in codejam problem

main :: IO ()
main = codeJam kSize

codeJam :: MonadIntf m => Int -> m ()
codeJam size = do
    (t,_,_) <- getInit
    replicateM_ t $ runMaybeT $ solve size

isHit' :: MonadIntf m => Pt -> MaybeT m Bool
isHit' pt = lift (isHit pt) >>= \case
    WRONG  -> error "got WRONG answer"
    CENTER -> mzero -- bullseye so now we break from MaybeT
    ans -> lift . pure $ ans == HIT

findTarget :: MonadIntf m => Int -> MaybeT m (Maybe Pt)
findTarget size = go $ (0,0) : quads
  where
    quads = (,) <$> [-h,h] <*> [-h,h] where h = div size 2
    go [] = pure Nothing
    go (pt:more) =
        ifM (isHit' pt) (pure $ Just pt) (go more)

solve :: MonadIntf m => Int -> MaybeT m ()
solve size = findTarget size >>= \case
    Nothing -> error "could not find target"
    Just (x,y) -> do
        xlo <- findMinM (isHit' . (,y)) (-size) x
        ylo <- findMinM (isHit' . (x,)) (-size) y
        xhi <- negate <$> findMinM (isHit' . (,y) . negate) (-size) (-x)
        yhi <- negate <$> findMinM (isHit' . (x,) . negate) (-size) (-y)
        let xctr = div (xlo + xhi) 2
        let yctr = div (ylo + yhi) 2
        -- traceShowM ((xlo,xhi),(ylo,yhi))
        _ <- isHit' (xctr,yctr) -- should exit MaybeT on CENTER result
        error "missed on bullseye hit"
