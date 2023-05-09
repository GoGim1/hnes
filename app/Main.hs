module Main where

import Emulator
import Control.Monad.State

main :: IO ()
main = evalStateT run initEmulator

