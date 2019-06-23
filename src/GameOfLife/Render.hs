{-# LANGUAGE LambdaCase #-}

module GameOfLife.Render
  ( RenderConf(..)
  , RenderControl(..)
  , runRenderer
  ) where

import Control.Monad (forever)

import Control.Monad.Cont (ContT(..), callCC, lift)
import Control.Monad.Reader (ReaderT(..), ask)

import Graphics.Vty.Picture (Picture)

data RenderConf m =
  RenderConf { renderControl :: !(m RenderControl)
             , outputPicture :: !(Picture -> m ())
             }

data RenderControl
  = Draw Picture
  | StopRender

type RenderT m = ContT () (ReaderT (RenderConf m) m)

runRenderer :: Monad m => RenderConf m -> m ()
runRenderer =
  runReaderT (runContT renderLoop pure)

renderLoop :: Monad m => RenderT m ()
renderLoop =
  callCC $ \shutdown ->
    ask >>= \(RenderConf nextEvent render) ->
      forever $
        lift (lift nextEvent) >>= \case
          Draw pic -> lift $ lift $ render pic
          StopRender -> shutdown ()
