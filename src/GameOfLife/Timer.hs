module GameOfLife.Timer
  ( TimerConf(..)
  , TimerControl(..)
  , runTimer
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)

import Control.Monad.State (StateT, evalStateT, get, liftIO, modify)

type Timer = StateT TimerConf IO

data TimerConf =
  TimerConf { action :: !(IO ())
            , usDelay :: !Int
            , readCtrlMsgs :: !(IO [TimerControl])
            }

data TimerControl
  = IncDelay
  | DecDelay
  deriving (Eq, Show)

runTimer :: TimerConf -> IO ()
runTimer =
  evalStateT tick

tick :: Timer ()
tick =
  forever $
    get >>= \(TimerConf perform delay getMessages) ->
      liftIO getMessages >>=
        mapM_ handleCtrlMessage >>
          liftIO (perform >> threadDelay delay)

handleCtrlMessage :: TimerControl -> Timer ()
handleCtrlMessage IncDelay =
  modify $ \conf -> conf { usDelay = incDelay (usDelay conf) }
handleCtrlMessage DecDelay =
  modify $ \conf -> conf { usDelay = decDelay (usDelay conf) }

incDelay :: Int -> Int
incDelay curr
  | curr >= second = curr + second
  | otherwise = curr * 2

decDelay :: Int -> Int
decDelay curr
  | millisecond >= curr = millisecond
  | otherwise = curr `div` 2

millisecond :: Int
millisecond =
  1000

second :: Int
second =
  1000 * 1000
