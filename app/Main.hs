{-# LANGUAGE LambdaCase #-}

module Main where

import System.Exit

import GameOfLife (runApp)

main :: IO ()
main =
  runApp >>= \case
    False -> exitWith $ ExitFailure 1
    True -> exitWith ExitSuccess
