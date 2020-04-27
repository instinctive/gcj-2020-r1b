-- Google Code Jam 2020 Round 1b - Join the Ranks
-- https://codingcompetitions.withgoogle.com/codejam/round/000000000019fef2/00000000002d5b64
-- vim: foldmethod=marker

-- pragmas, imports, and utilities {{{1
{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad ( forM_                    )
import Data.List     ( foldl', minimumBy, tails )
import Data.Monoid   ( (<>)                     )
import Data.Ord      ( comparing                )
import Data.Text     ( Text                     )
import Debug.Trace   ( traceShow, traceShowId   )
import Text.Printf   ( printf                   )
import qualified Data.Text as T

getReadList :: Read a => IO [a]
getReadList = map read . words <$> getLine

main :: IO ()
main = do
    [t] <- getReadList :: IO [Int]
    if t == 0 then do
        [nr,ns] <- getReadList :: IO [Int]
        table nr ns
    else forM_ [1..t] $ \i -> do
        printf "Case #%d: " i
        docase

-- search {{{1

type Result a = Either Bool a
pattern CUTOFF  = Left True
pattern FAILURE = Left False
pattern SUCCESS a = Right a

data Search a = Search
    { _isGoal :: a -> Bool
    , _spawn  :: a -> [a]
    }

type SearchFn a = a -> Search a -> Result a

greedy :: Ord r => (a -> r) -> SearchFn a
greedy heur a Search {..} = go a where
    go x | _isGoal x = SUCCESS x
    go x | otherwise = go best where
        best = minimumBy (comparing heur) (_spawn x)

-- solution {{{1

docase :: IO ()
docase = do
    [r,s] <- getReadList :: IO [Int]
    let moves = solve r s
    printf "%d\n" (length moves)
    mapM_ out moves
  where
    out (a,b) = printf "%d %d\n" a b

type Move = (Int,Int) -- (A,B)
type Deck = Text

mkDeck :: Int -> Int -> Deck
mkDeck r s = T.replicate s $ T.pack $ take r ['0'..]

move :: Move -> Text -> Text
move (a,b) t = bb <> aa <> cc where
    (aa,bbcc) = T.splitAt a t
    (bb,cc)   = T.splitAt b bbcc

score :: Text -> Int
score t = snd $ foldl' f (' ',0) [0..T.length t-1] where
    f z@(c,s) i
        | q < c = (q,s+2) -- inversion
        | q > c = (q,s+1) -- new run
        | otherwise = z   -- same run
      where q = T.index t i

runs :: Text -> [Int] -- start positions of runs
runs t = reverse . snd $ foldl' f (' ',[]) [0..T.length t-1] where
    f z@(c,ii) i
        | q == c = z
        | otherwise = (q,i:ii)
      where q = T.index t i

data State = State
    { _moves :: [Move]
    , _score :: Int
    , _runs  :: [Int]
    , _deck  :: Deck
    } deriving Show

mkState :: [Move] -> Deck -> State
mkState moves deck = State moves thscore (reverse thruns) deck where
    (_,thscore,thruns) = foldl' f (' ',0,[]) [0..T.length deck - 1]
    f z@(c,!s,!rr) i
        | q < c = (q,s+2,i:rr) -- inversion
        | q > c = (q,s+1,i:rr) -- new run
        | otherwise = z        -- same run
      where q = T.index deck i

isGoal :: Int -> State -> Bool
isGoal r State {..} = _score == r

spawn :: State -> [State]
spawn State {..} =
    [ mkState ((a,b):_moves) (move (a,b) _deck)
    | (a:more) <- tails (tail _runs)
    , (x:xtra) <- tails more
    , let b = x - a
    ]

mkEnv :: Int -> Search State
mkEnv r = Search (isGoal r) spawn

solve :: Int -> Int -> [Move]
solve r s = case greedy heur start env of
    SUCCESS State {..} -> reverse _moves
    _ -> error "could not find a solution"
  where
    heur  = _score
    start = mkState [] (mkDeck r s)
    env   = mkEnv r

table :: Int -> Int -> IO ()
table nr ns = do
    printf "      " >> forM_ [2..nr] (printf "R=%02d ") >> newline
    forM_ [2..ns] $ \s -> do
        printf "S=%02d: " s
        forM_ [2..nr] $ \r -> printf "  %2d " (length $ solve r s)
        newline
  where newline = putStrLn ""
