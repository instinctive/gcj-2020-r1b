-- Google Code Jam 2020 Round 1b - Expogo
-- https://codingcompetitions.withgoogle.com/codejam/round/000000000019fef2/00000000002d5b62
-- vim: foldmethod=marker

-- pragmas, imports, and utilities {{{1
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TupleSections         #-}

module Main where

import Control.Monad              ( forM_               )
import Control.Monad.Trans.Reader ( ReaderT, runReaderT )
import Text.Printf                ( printf              )

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM bm tm em = bm >>= \b -> if b then tm else em

getReadList :: Read a => IO [a]
getReadList = map read . words <$> getLine

class Monad m => SearchM a m where
    goal :: a -> m Bool
    next :: a -> m [a]

type Result a = Either Bool a

dfSearchM :: SearchM a m => a -> m (Result a)
dfSearchM a = go [a] where
    go [] = pure (Left False)
    go (x:xx) = ifM (goal x) (pure $ Right x) $
        next x >>= go . (++xx)

dlSearchM :: SearchM a m => Int -> a -> m (Result a)
dlSearchM d a = go False [(0,a)] where
    go limit [] = pure (Left limit)
    go limit ((i,x):xx) = ifM (goal x) (pure $ Right x) $
        if i >= d
        then go True xx
        else next x >>= go limit . (++xx) . map (i+1,)

idSearchM :: SearchM a m => a -> m (Result a)
idSearchM a = go 1 where
    go i = dlSearchM i a >>= \case
        Left True -> go (i+1)
        answer -> pure answer

-- types {{{1

data Env = Env
data Dir = N | S | E | W deriving (Eq,Show)
type State = ([Dir],(Int,Int))

instance Monad m => SearchM State (ReaderT Env m) where
    goal (_,xy) = pure $ xy == (0,0)
    next (dd,(x,y))
        | odd x && even y = pure
            [ (E:dd, (div (x-1) 2, div y 2))
            , (W:dd, (div (x+1) 2, div y 2)) ]
        | even x && odd y = pure
            [ (N:dd, (div x 2, div (y-1) 2))
            , (S:dd, (div x 2, div (y+1) 2)) ]
        | otherwise = pure []

-- main {{{1

main :: IO ()
main = do
    [t] <- getReadList :: IO [Int]
    forM_ [1..t] $ \i -> do
        printf "Case #%d: " i
        docase

docase :: IO ()
docase = do
    [x,y] <- getReadList :: IO [Int]
    case solve (x,y) of
        Nothing -> putStrLn "IMPOSSIBLE"
        Just dd -> putStrLn $ concatMap show dd

-- solve {{{1

solve :: (Int,Int) -> Maybe [Dir]
solve pt =
    let init = ([],pt) :: State in
    runReaderT (idSearchM init) Env >>= \case
        Left _       -> Nothing
        Right (dd,_) -> Just $ reverse dd
