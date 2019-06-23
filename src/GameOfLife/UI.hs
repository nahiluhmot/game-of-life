{-# LANGUAGE LambdaCase #-}

module GameOfLife.UI
  ( UIConf(..)
  , UIControl(..)
  , runUI
  ) where

import Control.Monad (forever)

import Control.Monad.State (StateT, evalStateT, gets, lift, modify)

import Graphics.Vty.Attributes (defAttr)
import Graphics.Vty.Image (Image, (<|>), (<->), char, emptyImage)
import Graphics.Vty.Picture (Picture, picForImage)

import System.Random (RandomGen)

import GameOfLife.Cell (Cell, isAlive)
import GameOfLife.Grid (Grid)
import qualified GameOfLife.Grid as G

-- Configuration for the UI.
data UIConf rng m =
  UIConf { nextControl :: !(m UIControl)
         , render :: !(Picture -> m ())
         , randGen :: !rng
         , initialSize :: !(Int, Int)
         }

-- Control instructions for the UI.
data UIControl
  = NextIteration
  | Resize Int Int
  deriving (Eq, Show)

type UIT rng m
  = StateT (UIConf rng m, Grid) m

-- Run the UI with the given configuration.
runUI :: (RandomGen rng, Monad m) => UIConf rng m -> m ()
runUI conf =
  let
    initialGrid =
      uncurry (G.random $ randGen conf) $ initialSize conf

    initialState =
      (conf, initialGrid)
  in
    evalStateT uiLoop initialState

uiLoop :: (RandomGen rng, Monad m) => UIT rng m ()
uiLoop =
  forever $
    -- heavy 'lift'ing required because we wrap ContT (StateT m)
    getsConf nextControl >>= lift >>= \case
      NextIteration -> nextIteration
      Resize x y -> resizeGrid x y

nextIteration :: Monad m => UIT rng m ()
nextIteration =
  getGrid >>= \grid ->
    let
      newGrid = G.next grid
    in
      putGrid newGrid >> renderGrid newGrid

resizeGrid :: (RandomGen rng, Monad m) => Int -> Int -> UIT rng m ()
resizeGrid x y =
  getsConf randGen >>= \rng ->
    let
      grid = G.random rng x y
    in
      putGrid grid >> renderGrid grid

renderGrid :: Monad m => Grid -> UIT rng m ()
renderGrid grid =
  getsConf render >>=
    lift . ($ picForImage $ drawGrid grid)

drawGrid :: Grid -> Image
drawGrid =
  let
    g [] _ _ cell = [drawCell cell]
    g is 0 _ cell = drawCell cell : is
    g (i:is) _ _ cell = (drawCell cell <|> i) : is
  in
    foldr (<->) emptyImage . G.foldWithCoord g []

drawCell :: Cell -> Image
drawCell cell
  | isAlive cell = char defAttr '█'
  | otherwise = char defAttr ' '

getsConf :: Monad m => (UIConf rng m -> a) -> UIT rng m a
getsConf =
  gets . (. fst)

putGrid :: Monad m => Grid -> UIT rng m ()
putGrid =
  modify . fmap . const

getGrid :: Monad m => UIT rng m Grid
getGrid =
  gets snd
