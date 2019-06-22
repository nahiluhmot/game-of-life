module GameOfLife.Grid
  ( Grid(..)
  , blank
  , random
  , next
  ) where

import Prelude hiding (replicate)

import Data.Tuple (swap)

import System.Random (RandomGen)
import qualified System.Random as R

import Data.Vector.Unboxed (Vector, (!), imap, replicate, unfoldrN)

import GameOfLife.Cell (Cell, dead, isAlive, toCell)

-- Represent the grid using a single (unboxed) 'Vector' of 'Cell's. I went with
-- a single vector instead of a matrix to avoid boxing.
data Grid =
  Grid { cells :: Vector Cell
       , width :: Int
       , height :: Int
       } deriving (Eq, Show)

-- Build a new blank 'Grid'.
blank :: Int -> Int -> Grid
blank x y =
  Grid (replicate (x * y) dead) x y

-- Generate a random 'Grid'.
random :: RandomGen g => g -> Int -> Int -> Grid
random gen x y =
  let
    cs = unfoldrN (x * y) (Just . R.random) gen
  in
    Grid cs x y

-- Generate the next iteration of the 'Grid'.
next :: Grid -> Grid
next (Grid cs w h) =
  let
    nextCell idx cell
      | isAlive cell = toCell $ covers 2 3 $ liveNeighbors idx
      | otherwise = toCell $ liveNeighbors idx == 3

    covers lower upper subject =
      (subject >= lower) && (subject <= upper)

    liveNeighbors :: Int -> Int
    liveNeighbors idx =
      let
        (x, y) = toXY idx
        left
          | x == 0 = 0
          | cellAlive $ toIdx (x - 1) y = 1
          | otherwise = 0
        right
          | (x + 1) == w = 0
          | cellAlive $ toIdx (x + 1) y = 1
          | otherwise = 0
        up
          | y == 0 = 0
          | cellAlive $ toIdx x (y - 1) = 1
          | otherwise = 0
        down
          | (y + 1) == h = 0
          | cellAlive $ toIdx x (y + 1) = 1
          | otherwise = 0
      in
        left + right + up + down

    cellAlive idx =
      isAlive $ cs ! idx

    toXY idx =
      swap $ idx `divMod` h

    toIdx x y =
      x + (y * h)
  in
    Grid (imap nextCell cs) w h
