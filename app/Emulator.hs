{-# LANGUAGE TemplateHaskell, MultiWayIf #-}

module Emulator where

import Cartridge
import qualified Cpu
import Data.Bits
import Data.Word
import Data.Sequence as Seq
import Mapper
import Utils
import Control.Lens
import Control.Monad.State

data Emulator = Emulator {
    _cartridge :: Cartridge,
    _cpu :: Cpu.Cpu,
    _ram :: Cpu.Ram
} deriving (Show)

makeLenses ''Emulator

type EmulatorState = StateT Emulator IO

initEmulator :: Emulator
initEmulator = Emulator emptyCartridge Cpu.initCpu Cpu.initRam

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

-- https://www.nesdev.org/wiki/CPU_memory_map
cpuRead :: Word16 -> EmulatorState Word8
cpuRead a = if  
    | a >= 0 && a < 0x2000 -> readRam (a .&. 0x7ff)             -- internal ram
    | a >= 0x2000 && a < 0x4000 -> undefined                    -- ppu register
    | a >= 0x4000 && a < 0x4018 -> undefined                    -- apu and io register
    | a >= 0x4018 && a < 0x4020 -> undefined                    -- normally disabled
    | a >= 0x4020 && a <= 0xffff -> do 
        mt <- getMapperType <$> use (cartridge . header . mapperNumber)
        cpuRead' mt a
    | otherwise -> error $ "handle addr " ++ show a

-- https://www.nesdev.org/wiki/NROM
cpuRead' :: MapperType -> Word16 -> EmulatorState Word8
cpuRead' Mapper000 a = do
    prgRomMask  <- gets $ subtract 1 . view (cartridge . header . prgRomSize)
    let prgRomAddr = (a - 0x8000) .&. fromIntegral prgRomMask
    if  | a >= 0x6000 && a < 0x8000 -> undefined                -- Family Basic only
        | a >= 0x8000 && a <= 0xffff -> readPrgRom prgRomAddr
        | otherwise -> error $ "handle addr " ++ show a 

cpuReadWord16 :: Word16 -> EmulatorState Word16
cpuReadWord16 a = do 
    low <- fromIntegral <$> cpuRead a 
    high <- fromIntegral <$> cpuRead (a+1) 
    return $ (high `shiftL` 8) .|. low

-- https://www.nesdev.org/wiki/CPU_power_up_state#After_reset
reset :: EmulatorState ()
reset = do
    (cpu . Cpu.stackPointer) %= subtract 3
    cpu %= Cpu.setI
    addr <- cpuReadWord16 Cpu.resetAddr
    (cpu . Cpu.programCounter) .= addr

step :: EmulatorState ()
step = do
    ins <- use (cpu . Cpu.programCounter) >>= cpuRead
    lift . print . showHexWord8 $ ins

run :: EmulatorState ()
run = do
    c <- lift $ readCartridge "roms/mario.nes"
    cartridge .= c
    reset
    forM_ [0 .. ] (const step)