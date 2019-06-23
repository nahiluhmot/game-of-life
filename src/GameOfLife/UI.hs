{-# LANGUAGE LambdaCase #-}

module GameOfLife.UI
  ( UIConf(..)
  , UIControl(..)
  , runUI
  ) where

import Control.Monad (forever)

import Control.Monad.Cont (ContT, callCC, runContT)
import Control.Monad.State.Strict (StateT, evalStateT, gets, lift, modify)

import Graphics.Vty.Attributes (defAttr)
import Graphics.Vty.Image (Image, (<->), text, emptyImage)
import Graphics.Vty.Picture (Picture, picForImage)

import System.Random (RandomGen)

import Data.Text.Lazy.Builder (singleton, toLazyTextWith)

import GameOfLife.Cell (Cell, isAlive)
import GameOfLife.Grid (Grid(..))
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
  | Refresh
  | Resize Int Int
  | StopUI
  deriving (Eq, Show)

type ContUI rng m
  = ContT () (StateT (UIConf rng m, Grid) m)

-- Run the UI with the given configuration.
runUI :: (RandomGen rng, Monad m) => UIConf rng m -> m ()
runUI conf =
  let
    initialGrid =
      uncurry (G.random $ randGen conf) $ initialSize conf

    initialState =
      (conf, initialGrid)
  in
    evalStateT (runContT uiLoop pure) initialState

uiLoop :: (RandomGen rng, Monad m) => ContUI rng m ()
uiLoop =
  callCC $ \shutdown ->
    forever $
      -- heavy 'lift'ing required because we wrap ContT (StateT m)
      getsConf nextControl >>= lift . lift >>= \case
        NextIteration -> nextIteration
        Resize x y -> resizeGrid x y
        Refresh -> refreshGrid
        StopUI -> shutdown ()

nextIteration :: Monad m => ContUI rng m ()
nextIteration =
  getGrid >>= \grid ->
    renderGrid grid >> putGrid (G.next grid)

refreshGrid :: (RandomGen rng, Monad m) => ContUI rng m ()
refreshGrid =
  getGrid >>= \grid ->
    resizeGrid (width grid) (height grid)

resizeGrid :: (RandomGen rng, Monad m) => Int -> Int -> ContUI rng m ()
resizeGrid x y =
  getsConf randGen >>= \rng ->
    let
      grid = G.random rng x y
    in
      renderGrid grid >> putGrid (G.next grid)

renderGrid :: Monad m => Grid -> ContUI rng m ()
renderGrid grid =
  getsConf render >>=
    lift . lift . ($ picForImage $ drawGrid grid)

drawGrid :: Grid -> Image
drawGrid grid =
  let
    g (image, _) 0 0 cell =
      (image, singleton (drawCell cell))
    g (image, builder) 0 _ cell =
      (image <-> builderToLine builder, singleton (drawCell cell))
    g (image, builder) _ _ cell =
      (image, builder <> singleton (drawCell cell))

    builderToLine =
      text defAttr . toLazyTextWith (width grid)

    (butLast, lastLine) = G.foldWithCoord g (emptyImage, mempty) grid
  in
    butLast <-> builderToLine lastLine

drawCell :: Cell -> Char
drawCell cell
  | isAlive cell = '█'
  | otherwise = ' '

getsConf :: Monad m => (UIConf rng m -> a) -> ContUI rng m a
getsConf =
  gets . (. fst)

putGrid :: Monad m => Grid -> ContUI rng m ()
putGrid =
  modify . fmap . const

getGrid :: Monad m => ContUI rng m Grid
getGrid =
  gets snd
