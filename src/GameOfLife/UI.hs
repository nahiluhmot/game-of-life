{-# LANGUAGE LambdaCase #-}

module GameOfLife.UI
  ( UIConf(..)
  , UIControl(..)
  , runUI
  ) where

import Control.Monad (forever)

import Control.Monad.Cont (ContT, callCC, runContT)
import Control.Monad.State.Strict (StateT, evalStateT, gets, lift, modify)

import Data.Vector.Unboxed ((!))

import Graphics.Vty.Attributes (defAttr)
import Graphics.Vty.Image (Image, (<->), text)
import Graphics.Vty.Picture (picForImage)

import System.Random (RandomGen)

import Data.Text.Lazy.Builder (singleton, toLazyTextWith)

import GameOfLife.Cell (Cell)
import GameOfLife.Grid (Grid(..))
import qualified GameOfLife.Grid as G
import GameOfLife.Render (RenderControl(..))

-- Configuration for the UI.
data UIConf rng m =
  UIConf { nextControl :: !(m UIControl)
         , toRenderer :: !(RenderControl -> m ())
         , getRandGen :: !(m rng)
         , initialSize :: !(Int, Int)
         }

-- Control instructions for the UI.
data UIControl
  = NextIteration
  | Refresh
  | Clear
  | Resize Int Int
  | StopUI
  deriving (Eq, Show)

type ContUI rng m
  = ContT () (StateT (UIConf rng m, Grid) m)

-- Run the UI with the given configuration.
runUI :: (RandomGen rng, Monad m) => UIConf rng m -> m ()
runUI conf =
  getRandGen conf >>= \gen ->
    let
      (x, y) = initialSize conf

      initialGrid =
        G.random gen (x * 2) (y * 2)

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
        Resize x y -> resizeGrid (x * 2) (y * 2)
        Refresh -> refreshGrid
        Clear -> clearGrid
        StopUI -> stopRenderer >> shutdown ()

nextIteration :: Monad m => ContUI rng m ()
nextIteration =
  getGrid >>= \grid ->
    renderGrid grid >> putGrid (G.next grid)

refreshGrid :: (RandomGen rng, Monad m) => ContUI rng m ()
refreshGrid =
  getGrid >>= \grid ->
    resizeGrid (width grid) (height grid)

clearGrid :: (RandomGen rng, Monad m) => ContUI rng m ()
clearGrid =
  getGrid >>= \grid ->
    let
      newGrid = G.blank (width grid) (height grid)
    in
      renderGrid newGrid >> putGrid newGrid

resizeGrid :: (RandomGen rng, Monad m) => Int -> Int -> ContUI rng m ()
resizeGrid x y =
  getsConf getRandGen >>= lift . lift >>= \rng ->
    let
      grid = G.random rng x y
    in
      renderGrid grid >> putGrid (G.next grid)

renderGrid :: Monad m => Grid -> ContUI rng m ()
renderGrid grid =
  getsConf toRenderer >>=
    lift . lift . ($ Draw . picForImage $ drawGrid grid)

stopRenderer :: Monad m => ContUI rng m ()
stopRenderer =
  getsConf toRenderer >>=
    lift . lift . ($ StopRender)

drawGrid :: Grid -> Image
drawGrid g@(Grid cs w h) =
  let
    go (-2) 0 builder image = combine builder image
    go (-2) y builder image = go (w - 2) (y - 2) mempty (Just $ combine builder image)
    go x y builder image =
      go (x - 2) y (singleton (uncurry4 drawCells (boxAt x y)) <> builder) image

    combine builder Nothing = builderToLine builder
    combine builder (Just image) = builderToLine builder <-> image

    boxAt x y =
      ( cs ! G.toIdx g x y
      , cs ! G.toIdx g (x + 1) y
      , cs ! G.toIdx g x (y + 1)
      , cs ! G.toIdx g (x + 1) (y + 1)
      )

    uncurry4 f (a, b, c, d) = f a b c d

    builderToLine =
      text defAttr . toLazyTextWith w
  in
    go (w - 2) (h - 2) mempty Nothing

drawCells :: Cell -> Cell -> Cell -> Cell -> Char
drawCells False False False False = ' '
drawCells False False False True = '▗'
drawCells False False True False = '▖'
drawCells False True False False = '▝'
drawCells True False False False = '▘'
drawCells False False True True = '▄'
drawCells False True False True = '▐'
drawCells True False False True = '▚'
drawCells False True True False = '▞'
drawCells True False True False = '▌'
drawCells True True False False = '▀'
drawCells False True True True = '▟'
drawCells True False True True = '▙'
drawCells True True False True = '▜'
drawCells True True True False = '▛'
drawCells True True True True = '█'

getsConf :: Monad m => (UIConf rng m -> a) -> ContUI rng m a
getsConf =
  gets . (. fst)

putGrid :: Monad m => Grid -> ContUI rng m ()
putGrid =
  modify . fmap . const

getGrid :: Monad m => ContUI rng m Grid
getGrid =
  gets snd
