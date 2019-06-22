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
  | SetDelay Int
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
    handleCtrlMessage _ (SetDelay delay) =
      modify $ \conf -> conf { usDelay = delay }
  in
    callCC $ \exit -> forever (tickOnce (exit ()))
