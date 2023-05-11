{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}

module Emulator where

import Cartridge
import Control.Lens
import Control.Monad.State
import Data.Sequence as Seq
import Data.Word
import Utils

type Address = Word16

-- https://www.nesdev.org/wiki/CPU_registers
data Cpu = Cpu
  { _accumulator :: Word8,
    _indexX :: Word8,
    _indexY :: Word8,
    _programCounter :: Address,
    _stackPointer :: Word8,
    _status :: Word8
  }

makeLenses ''Cpu

instance Show Cpu where
  show (Cpu a x y pc sp p) =
    unlines
      [ "accumulator: " ++ showHexWord8 a,
        "indexX: " ++ showHexWord8 x,
        "indexY: " ++ showHexWord8 y,
        "programCounter: " ++ showHex pc,
        "stackPointer: " ++ showHexWord8 sp,
        "status: " ++ showBinaryWord8 p
      ]

type Ram = Seq Word8

data Emulator = Emulator
  { _cartridge :: Cartridge,
    _cpu :: Cpu,
    _ram :: Ram
  }
  deriving (Show)

makeLenses ''Emulator

-- https://www.nesdev.org/wiki/CPU_power_up_state
initCpu :: Cpu
initCpu = Cpu 0 0 0 0 0xfd 0x34

initRam :: Ram
initRam = Seq.replicate 0x800 0

initEmulator :: Emulator
initEmulator = Emulator emptyCartridge initCpu initRam

type EmulatorState = StateT Emulator IO

readRam :: Word16 -> EmulatorState Word8
readRam a = do
  r <- use ram
  return . Seq.index r . word16ToInt $ a

writeRam :: Word16 -> Word8 -> EmulatorState ()
writeRam a v = ram %= update (word16ToInt a) v

readPrgRom :: Word16 -> EmulatorState Word8
readPrgRom a = do
  r <- use $ cartridge . prgRom
  return . Seq.index r . word16ToInt $ a
