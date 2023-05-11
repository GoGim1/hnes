{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE MultiWayIf #-}

module Cpu where

import Cartridge
import Control.Lens
import Control.Monad.State
import Data.Bits
import Data.Word
import Emulator
import Mapper
import Utils

-- https://www.nesdev.org/wiki/CPU_interrupts#Interrupt_hijacking
nmiAddr :: Address
nmiAddr = 0xfffa

resetAddr :: Address
resetAddr = 0xfffc

irqAddr :: Address
irqAddr = 0xfffe

brkAddr :: Address
brkAddr = 0xfffe

-- https://www.nesdev.org/wiki/Status_flags
setC :: Cpu -> Cpu
setC c = c {_status = setBit (c ^. status) 0}

setZ :: Cpu -> Cpu
setZ c = c {_status = setBit (c ^. status) 1}

setI :: Cpu -> Cpu
setI c = c {_status = setBit (c ^. status) 2}

setD :: Cpu -> Cpu
setD c = c {_status = setBit (c ^. status) 3}

setV :: Cpu -> Cpu
setV c = c {_status = setBit (c ^. status) 6}

setN :: Cpu -> Cpu
setN c = c {_status = setBit (c ^. status) 7}

clearC :: Cpu -> Cpu
clearC c = c {_status = clearBit (c ^. status) 0}

clearZ :: Cpu -> Cpu
clearZ c = c {_status = clearBit (c ^. status) 1}

clearI :: Cpu -> Cpu
clearI c = c {_status = clearBit (c ^. status) 2}

clearD :: Cpu -> Cpu
clearD c = c {_status = clearBit (c ^. status) 3}

clearV :: Cpu -> Cpu
clearV c = c {_status = clearBit (c ^. status) 6}

clearN :: Cpu -> Cpu
clearN c = c {_status = clearBit (c ^. status) 7}

cFlag :: Cpu -> Bool
cFlag c = testBit (c ^. status) 0

zFlag :: Cpu -> Bool
zFlag c = testBit (c ^. status) 1

iFlag :: Cpu -> Bool
iFlag c = testBit (c ^. status) 2

dFlag :: Cpu -> Bool
dFlag c = testBit (c ^. status) 3

vFlag :: Cpu -> Bool
vFlag c = testBit (c ^. status) 6

nFlag :: Cpu -> Bool
nFlag c = testBit (c ^. status) 7

-- https://www.nesdev.org/obelisk-6502-guide/addressing.html
data AdressingMode
  = Implicit
  | Accumulator
  | Immediate
  | ZeroPage
  | ZeroPageX
  | ZeroPageY
  | Relative
  | Absolute
  | AbsoluteX
  | AbsoluteY
  | Indirect

-- https://www.nesdev.org/wiki/CPU_memory_map
cpuRead :: Word16 -> EmulatorState Word8
cpuRead a =
  if
      | a >= 0 && a < 0x2000 -> readRam (a .&. 0x7ff) -- internal ram
      | a >= 0x2000 && a < 0x4000 -> undefined -- ppu register
      | a >= 0x4000 && a < 0x4018 -> undefined -- apu and io register
      | a >= 0x4018 && a < 0x4020 -> undefined -- normally disabled
      | a >= 0x4020 && a <= 0xffff -> do
          mt <- getMapperType <$> use (cartridge . header . mapperNumber)
          cpuRead' mt a
      | otherwise -> error $ "handle addr " ++ show a

-- https://www.nesdev.org/wiki/NROM
cpuRead' :: MapperType -> Word16 -> EmulatorState Word8
cpuRead' Mapper000 a = do
  prgRomMask <- gets $ subtract 1 . view (cartridge . header . prgRomSize)
  let prgRomAddr = (a - 0x8000) .&. fromIntegral prgRomMask
  if
      | a >= 0x6000 && a < 0x8000 -> undefined -- Family Basic only
      | a >= 0x8000 && a <= 0xffff -> readPrgRom prgRomAddr
      | otherwise -> error $ "handle addr " ++ show a

cpuReadWord16 :: Word16 -> EmulatorState Word16
cpuReadWord16 a = do
  low <- fromIntegral <$> cpuRead a
  high <- fromIntegral <$> cpuRead (a + 1)
  return $ (high `shiftL` 8) .|. low

-- https://www.nesdev.org/wiki/CPU_power_up_state#After_reset
reset :: EmulatorState ()
reset = do
  (cpu . stackPointer) %= subtract 3
  cpu %= setI
  addr <- cpuReadWord16 resetAddr
  (cpu . programCounter) .= addr

step :: EmulatorState ()
step = do
  ins <- use (cpu . programCounter) >>= cpuRead
  lift . print . showHexWord8 $ ins

run :: EmulatorState ()
run = do
  reset
  forM_ [0 ..] (const step)
