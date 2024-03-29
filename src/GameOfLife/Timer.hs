module GameOfLife.Timer
  ( TimerConf(..)
  , TimerControl(..)
  , runTimer
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, unless)

import Control.Monad.Cont (ContT, callCC, runContT)
import Control.Monad.State.Strict (StateT, evalStateT, get, gets, liftIO, modify)

type Timer = ContT () (StateT TimerConf IO)

data TimerConf =
  TimerConf { action :: !(IO ())
            , readCtrlMsgs :: !(IO [TimerControl])
            , refreshRate :: !Int -- hz
            , minRefreshRate :: !Int -- hz
            , maxRefreshRate :: !Int -- hz
            , paused :: !Bool
            }

data TimerControl
  = StopTimer
  | IncFreq
  | DecFreq
  | Pause
  | Resume
  deriving (Eq, Show)

runTimer :: TimerConf -> IO ()
runTimer =
  evalStateT $ runContT tick pure

tick :: Timer ()
tick =
  let
    tickOnce :: Timer () -> Timer ()
    tickOnce exit =
      msgBatch >>= mapM_ (handleCtrlMessage exit) >> performAndSleep

    msgBatch =
      gets readCtrlMsgs >>= liftIO

    handleCtrlMessage :: Timer () -> TimerControl -> Timer ()
    handleCtrlMessage exit StopTimer =
      exit
    handleCtrlMessage _ Pause =
      modify $ \conf ->
        conf { paused = True }
    handleCtrlMessage _ Resume =
      modify $ \conf ->
        conf { paused = False }
    handleCtrlMessage _ IncFreq =
      modify $ \conf ->
        let
          maxRate = maxRefreshRate conf
          curr = refreshRate conf
          delta = 1
          rate
            | (curr + delta) >= maxRate = maxRate
            | otherwise = curr + delta
        in
          conf { refreshRate = rate }
    handleCtrlMessage _ DecFreq =
      modify $ \conf ->
        let
          minRate = minRefreshRate conf
          curr = refreshRate conf
          delta = 1
          rate
            | (curr - delta) <= minRate = minRate
            | otherwise = curr - delta
        in
          conf { refreshRate = rate }

    performAndSleep =
      get >>= \conf ->
        liftIO $
          unless (paused conf) (action conf) >>
            threadDelay (hzToUsec (refreshRate conf))
  in
    callCC $ \exit -> forever $ tickOnce (exit ())

hzToUsec :: Int -> Int
hzToUsec hz =
  (1000 * 1000) `div` hz
