{-# LANGUAGE LambdaCase #-}

module GameOfLife.App
  ( runApp
  ) where

import Control.Exception.Base (displayException)

import Control.Concurrent.Async (Async, async, waitCatch)

import Control.Concurrent.STM (STM, atomically, retry)
import Control.Concurrent.STM.TBQueue (TBQueue, flushTBQueue, isFullTBQueue, newTBQueueIO, tryReadTBQueue, writeTBQueue)

import qualified Graphics.Vty as V
import Graphics.Vty.Config (defaultConfig)
import Graphics.Vty.Output (hideCursor, showCursor)
import Graphics.Vty.Output.Interface (displayBounds)

import System.Random (newStdGen)

import GameOfLife.EventHandler (EventHandlerConf(..), runEventHandler)
import GameOfLife.Render (RenderConf(..), RenderControl(..), runRenderer)
import GameOfLife.Timer (TimerConf(..), TimerControl, runTimer)
import GameOfLife.UI (UIConf(..), UIControl(..), runUI)

runApp :: IO Bool
runApp = do
  -- Allow event queues to build up for responsiveness.
  eventToTimer <- newTBQueueIO 1024
  eventToUI <- newTBQueueIO 1024
  -- Event only sends shutdowns to render
  eventToRender <- newTBQueueIO 1
  -- The timer queue controls the refresh rate of the UI. It doesn't do us any
  -- good to allow it to build up. Maxing out at one keeps the application
  -- responsive, and allows us to pause more easily.
  timerToUI <- newTBQueueIO 1
  uiToRender <- newTBQueueIO 1

  vty <- V.mkVty defaultConfig
  size <- displayBounds $ V.outputIface vty

  hideCursor $ V.outputIface vty

  eventHandlerFuture <- async $ eventHandlerThread vty eventToTimer eventToUI
  timerFuture <- async $ timerThread eventToTimer timerToUI
  uiFuture <- async $ uiThread [eventToUI, timerToUI] uiToRender size
  renderFuture <- async $ renderThread [eventToRender, uiToRender] vty

  eventResult <- wait' "event handler" eventHandlerFuture
  timerResult <- wait' "timer" timerFuture
  uiResult <- wait' "ui" uiFuture
  renderResult <- wait' "render" renderFuture

  showCursor $ V.outputIface vty

  V.shutdown vty

  let
    combine (Right _) es = es
    combine (Left e) es = e : es
    errs = foldr combine [] [eventResult, timerResult, uiResult, renderResult]

  case errs of
    [] -> pure True
    es -> False <$ mapM_ putStrLn es

wait' :: String -> Async a -> IO (Either String a)
wait' name =
  let
    showErr (Right val) = Right val
    showErr (Left ex) = Left $ "Error in " ++ name ++ " thread: " ++ displayException ex
  in
    fmap showErr . waitCatch


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
              , refreshRate = 15
              , minRefreshRate = 1
              , maxRefreshRate = 60
              , readCtrlMsgs = atomically $ flushTBQueue timerQueue
              , paused = False
              }

uiThread :: [TBQueue UIControl] -> TBQueue RenderControl -> (Int, Int) -> IO ()
uiThread queues renderQueue size =
  runUI
    UIConf { nextControl = atomically $ readQueues queues
           , toRenderer = atomically . writeTBQueue renderQueue
           , getRandGen = newStdGen
           , initialSize = size
           }

renderThread :: [TBQueue RenderControl] -> V.Vty -> IO ()
renderThread queues vty =
  runRenderer
    RenderConf { renderControl = atomically $ readQueues queues
               , outputPicture = V.update vty
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
