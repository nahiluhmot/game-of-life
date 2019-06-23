module GameOfLife.App
  ( runApp
  ) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (TQueue, flushTQueue, newTQueueIO, readTQueue, writeTQueue)
import Control.Concurrent.Async (async, cancel, wait)

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

  timerQueue <- newTQueueIO
  uiQueue <- newTQueueIO

  vty <- V.mkVty defaultConfig
  size <- displayBounds $ V.outputIface vty

  eventHandlerFuture <- async $ eventHandlerThread vty timerQueue uiQueue
  timerFuture <- async $ timerThread timerQueue uiQueue
  uiFuture <- async $ uiThread vty uiQueue gen size

  wait eventHandlerFuture

  cancel timerFuture
  cancel uiFuture

  V.shutdown vty

eventHandlerThread :: V.Vty -> TQueue TimerControl -> TQueue UIControl -> IO ()
eventHandlerThread vty timerQueue uiQueue =
  runEventHandler $
    EventHandlerConf { nextEvent = V.nextEvent vty
                     , timerCtrl = atomically . writeTQueue timerQueue
                     , uiCtrl = atomically . writeTQueue uiQueue
                     }

timerThread :: TQueue TimerControl -> TQueue UIControl -> IO ()
timerThread timerQueue uiQueue =
  runTimer $
    TimerConf { action = atomically $ writeTQueue uiQueue NextIteration
              , usDelay = 500 * 1000 -- 1/2 second
              , readCtrlMsgs = atomically $ flushTQueue timerQueue
              }

uiThread :: RandomGen g => V.Vty -> TQueue UIControl -> g -> (Int, Int) -> IO ()
uiThread vty uiQueue gen size =
  runUI $
    UIConf { nextControl = atomically $ readTQueue uiQueue
           , render = V.update vty
           , randGen = gen
           , initialSize = size
           }
