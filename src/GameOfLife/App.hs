{-# LANGUAGE LambdaCase #-}

module GameOfLife.App
  ( runApp
  ) where

import Control.Concurrent.STM (STM, atomically, retry)
import Control.Concurrent.STM.TBQueue (TBQueue, flushTBQueue, isFullTBQueue, newTBQueueIO, tryReadTBQueue, writeTBQueue)
import Control.Concurrent.Async (async, wait)

import qualified Graphics.Vty as V
import Graphics.Vty.Config (defaultConfig)
import Graphics.Vty.Output (hideCursor, showCursor)
import Graphics.Vty.Output.Interface (displayBounds)

import System.Random (RandomGen, newStdGen)

import GameOfLife.EventHandler (EventHandlerConf(..), runEventHandler)
import GameOfLife.Timer (TimerConf(..), TimerControl, runTimer)
import GameOfLife.UI (UIConf(..), UIControl(..), runUI)

runApp :: IO ()
runApp = do
  gen <- newStdGen

  -- Allow event queues to build up for responsiveness.
  eventToTimer <- newTBQueueIO 1024
  eventToUI <- newTBQueueIO 1024
  -- The timer queue controls the refresh rate of the UI. It doesn't do us any
  -- good to allow it to build up. Maxing out at one keeps the application
  -- responsive.
  timerToUI <- newTBQueueIO 1

  vty <- V.mkVty defaultConfig
  size <- displayBounds $ V.outputIface vty

  hideCursor $ V.outputIface vty

  eventHandlerFuture <- async $ eventHandlerThread vty eventToTimer eventToUI
  timerFuture <- async $ timerThread eventToTimer timerToUI
  uiFuture <- async $ uiThread vty [eventToUI, timerToUI] gen size

  wait eventHandlerFuture
  wait timerFuture
  wait uiFuture

  showCursor $ V.outputIface vty

  V.shutdown vty

eventHandlerThread :: V.Vty -> TBQueue TimerControl -> TBQueue UIControl -> IO ()
eventHandlerThread vty timerQueue uiQueue =
  runEventHandler
    EventHandlerConf { nextEvent = V.nextEvent vty
                     , timerCtrl = atomically . writeTBQueue timerQueue
                     , uiCtrl = atomically . writeTBQueue uiQueue
                     }

timerThread :: TBQueue TimerControl -> TBQueue UIControl -> IO ()
timerThread timerQueue uiQueue =
  runTimer
    TimerConf { action = atomically $ tryWriteQueue uiQueue NextIteration
              , usDelay = 100 * 1000 -- 1/10 second
              , readCtrlMsgs = atomically $ flushTBQueue timerQueue
              }

uiThread :: RandomGen g => V.Vty -> [TBQueue UIControl] -> g -> (Int, Int) -> IO ()
uiThread vty queues gen size =
  runUI
    UIConf { nextControl = atomically $ readQueues queues
           , render = V.update vty
           , randGen = gen
           , initialSize = size
           }

tryWriteQueue :: TBQueue a -> a -> STM ()
tryWriteQueue queue msg =
  isFullTBQueue queue >>= \case
    False -> writeTBQueue queue msg
    True -> pure ()

-- Tried to read from the queues in order, retrying until one is successful.
readQueues :: [TBQueue a] -> STM a
readQueues =
  foldr readOrElse retry

readOrElse :: TBQueue a -> STM a -> STM a
readOrElse queue alternative =
  tryReadTBQueue queue >>= \case
    Nothing -> alternative
    Just val -> pure val
