{-# LANGUAGE LambdaCase #-}

module GameOfLife.App
  ( runApp
  ) where

import Control.Concurrent.STM (STM, atomically, retry)
import Control.Concurrent.STM.TQueue (TQueue, flushTQueue, newTQueueIO, tryReadTQueue, writeTQueue)
import Control.Concurrent.Async (async, wait)

import qualified Graphics.Vty as V
import Graphics.Vty.Config (defaultConfig)
import Graphics.Vty.Output.Interface (displayBounds)

import System.Random (RandomGen, newStdGen)

import GameOfLife.EventHandler (EventHandlerConf(..), runEventHandler)
import GameOfLife.Timer (TimerConf(..), TimerControl, runTimer)
import GameOfLife.UI (UIConf(..), UIControl(..), runUI)

runApp :: IO ()
runApp = do
  gen <- newStdGen

  eventToTimer <- newTQueueIO
  eventToUI <- newTQueueIO
  timerToUI <- newTQueueIO

  vty <- V.mkVty defaultConfig
  size <- displayBounds $ V.outputIface vty

  eventHandlerFuture <- async $ eventHandlerThread vty eventToTimer eventToUI
  timerFuture <- async $ timerThread eventToTimer timerToUI
  uiFuture <- async $ uiThread vty [eventToUI, timerToUI] gen size

  wait eventHandlerFuture
  wait timerFuture
  wait uiFuture

  V.shutdown vty

eventHandlerThread :: V.Vty -> TQueue TimerControl -> TQueue UIControl -> IO ()
eventHandlerThread vty timerQueue uiQueue =
  runEventHandler
    EventHandlerConf { nextEvent = V.nextEvent vty
                     , timerCtrl = atomically . writeTQueue timerQueue
                     , uiCtrl = atomically . writeTQueue uiQueue
                     }

timerThread :: TQueue TimerControl -> TQueue UIControl -> IO ()
timerThread timerQueue uiQueue =
  runTimer
    TimerConf { action = atomically $ writeTQueue uiQueue NextIteration
              , usDelay = 500 * 1000 -- 1/2 second
              , readCtrlMsgs = atomically $ flushTQueue timerQueue
              }

uiThread :: RandomGen g => V.Vty -> [TQueue UIControl] -> g -> (Int, Int) -> IO ()
uiThread vty queues gen size =
  runUI
    UIConf { nextControl = atomically $ readQueues queues
           , render = V.update vty
           , randGen = gen
           , initialSize = size
           }

-- Tried to read from the queues in order, retrying until one is successful.
readQueues :: [TQueue a] -> STM a
readQueues =
  foldr readOrElse retry

readOrElse :: TQueue a -> STM a -> STM a
readOrElse queue alternative =
  tryReadTQueue queue >>= \case
    Nothing -> alternative
    Just val -> pure val
