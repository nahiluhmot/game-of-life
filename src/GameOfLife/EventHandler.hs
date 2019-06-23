{-# LANGUAGE LambdaCase #-}

module GameOfLife.EventHandler
  ( EventHandlerConf(..)
  , runEventHandler
  ) where

import Control.Monad (forever)

import Control.Monad.Cont (ContT(..), callCC, lift)
import Control.Monad.Reader (ReaderT(..), asks)

import Graphics.Vty.Input

import GameOfLife.Timer (TimerControl(..))
import GameOfLife.UI (UIControl(..))

-- Configuration for the event loop.
data EventHandlerConf m
  = EventHandlerConf { nextEvent :: !(m Event)
                     , timerCtrl :: !(TimerControl -> m ())
                     , uiCtrl :: !(UIControl -> m ())
                     }

type EventT r m = ContT r (ReaderT (EventHandlerConf m) m)

-- Run the event loop with the given configuration.
runEventHandler :: Monad m => EventHandlerConf m -> m ()
runEventHandler =
  runReaderT $ runContT eventLoop pure

eventLoop :: Monad m => EventT r m ()
eventLoop =
  callCC $ \shutdown ->
    forever $
      asks nextEvent >>= lift . lift >>= \case
        (EvResize x y) -> uiSend (Resize x y)
        (EvKey (KChar '+') _) -> timerSend IncDelay
        (EvKey (KChar '-') _) -> timerSend DecDelay
        (EvKey (KChar 'q') _) -> timerSend StopTimer >> uiSend StopUI >> shutdown ()
        _ -> pure ()

timerSend :: Monad m => TimerControl -> EventT r m ()
timerSend ctrl =
  asks timerCtrl >>=
    lift . lift . ($ ctrl)

uiSend :: Monad m => UIControl -> EventT r m ()
uiSend ctrl =
  asks uiCtrl >>=
    lift . lift . ($ ctrl)
