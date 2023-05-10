module Main where

import Control.Monad.State
import Emulator

main :: IO ()
main = evalStateT run initEmulator
