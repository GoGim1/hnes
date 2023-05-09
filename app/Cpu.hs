 {-# LANGUAGE BinaryLiterals, TemplateHaskell #-}

module Cpu where

import Control.Lens
import Data.Bits
import Data.Sequence
import Data.Word
import Utils
import Text.Printf

type Address = Word16

-- https://www.nesdev.org/wiki/CPU_registers
data Cpu = Cpu {
    _accumulator :: Word8,
    _indexX :: Word8,
    _indexY :: Word8,
    _programCounter :: Address,
    _stackPointer :: Word8,
    _status :: Word8
}

makeLenses ''Cpu

instance Show Cpu where
   show c@(Cpu a x y pc sp p) = 
    unlines [
        "accumulator: " ++ showHexWord8 a,
        "indexX: " ++ showHexWord8 x,
        "indexY: " ++ showHexWord8 y,
        "programCounter: " ++ showHex pc,
        "stackPointer: " ++ showHexWord8 sp,
        "status: " ++ showHexWord8 p,
        printf "C: %s, Z: %s, I: %s, D: %s, V: %s, N: %s" 
            (show $ cFlag c) (show $ zFlag c) (show $ iFlag c)
            (show $ dFlag c) (show $ vFlag c) (show $ nFlag c)
    ]


-- https://www.nesdev.org/wiki/CPU_interrupts#Interrupt_hijacking
nmiAddr :: Address
nmiAddr = 0xfffa

resetAddr :: Address
resetAddr = 0xfffc

irqAddr :: Address
irqAddr = 0xfffe

brkAddr :: Address
brkAddr = 0xfffe

-- https://www.nesdev.org/wiki/CPU_power_up_state
initCpu :: Cpu
initCpu = Cpu 0 0 0 0 0xfd 0x34

-- https://www.nesdev.org/wiki/Status_flags
setC :: Cpu -> Cpu
setC c = c { _status = setBit (c ^. status) 0 } 

setZ :: Cpu -> Cpu
setZ c = c { _status = setBit (c ^. status) 1 } 

setI :: Cpu -> Cpu
setI c = c { _status = setBit (c ^. status) 2 } 

setD :: Cpu -> Cpu
setD c = c { _status = setBit (c ^. status) 3 } 

setV :: Cpu -> Cpu
setV c = c { _status = setBit (c ^. status) 6 } 

setN :: Cpu -> Cpu
setN c = c { _status = setBit (c ^. status) 7 }

clearC :: Cpu -> Cpu
clearC c = c { _status = clearBit (c ^. status) 0 }

clearZ :: Cpu -> Cpu
clearZ c = c { _status = clearBit (c ^. status) 1 }

clearI :: Cpu -> Cpu
clearI c = c { _status = clearBit (c ^. status) 2 }

clearD :: Cpu -> Cpu
clearD c = c { _status = clearBit (c ^. status) 3 }

clearV :: Cpu -> Cpu
clearV c = c { _status = clearBit (c ^. status) 6 }

clearN :: Cpu -> Cpu
clearN c = c { _status = clearBit (c ^. status) 7 }

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

type Ram = Seq Word8

initRam :: Ram
initRam = Data.Sequence.replicate 0x800 0

-- https://www.nesdev.org/obelisk-6502-guide/addressing.html
data AdressingMode = Implicit 
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



