module Main where

import Cartridge
import Control.Lens
import Control.Monad.State
import qualified Cpu
import Emulator

main :: IO ()
main = flip evalStateT initEmulator $ do
  c <- lift $ readCartridge "roms/mario.nes"
  cartridge .= c
  Cpu.run
