module GameOfLife.Timer
  ( TimerConf(..)
  , TimerControl(..)
  , runTimer
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (TQueue, flushTQueue)

import Control.Monad.Cont (ContT, callCC, runContT)
import Control.Monad.State (StateT, evalStateT, get, gets, liftIO, modify)

type Timer = ContT () (StateT TimerConf IO)

data TimerConf =
  TimerConf { action :: IO ()
            , usDelay :: Int
            , queue :: TQueue TimerControl
            }

data TimerControl
  = Shutdown
  | IncDelay
  | DecDelay
  deriving (Eq, Show)

runTimer :: TimerConf -> IO ()
runTimer =
  evalStateT $ runContT tick pure

tick :: Timer ()
tick =
  let
    tickOnce :: Timer () -> Timer ()
    tickOnce exit =
       readCtrlMessages >>= mapM_ (handleCtrlMessage exit) >> performAndSleep

    performAndSleep =
      get >>= \(TimerConf perform delay _) ->
        liftIO (perform >> threadDelay delay)

    readCtrlMessages =
      gets queue >>= liftIO . atomically . flushTQueue

    handleCtrlMessage :: Timer () -> TimerControl -> Timer ()
    handleCtrlMessage exit Shutdown =
      exit
    handleCtrlMessage _ IncDelay =
      modify $ \conf -> conf { usDelay = incDelay (usDelay conf) }
    handleCtrlMessage _ DecDelay =
      modify $ \conf -> conf { usDelay = decDelay (usDelay conf) }
  in
    callCC $ \exit -> forever (tickOnce (exit ()))

incDelay :: Int -> Int
incDelay curr
  | curr >= second = curr + second
  | otherwise = curr * 2

decDelay :: Int -> Int
decDelay curr
  | millisecond > curr = 0
  | otherwise = curr `div` 2

millisecond :: Int
millisecond =
  1000

second :: Int
second =
  1000 * 1000
