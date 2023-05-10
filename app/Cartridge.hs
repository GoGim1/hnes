{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE TemplateHaskell #-}

module Cartridge where

import Control.Lens
import Control.Monad.Extra
import Control.Monad.State
import Data.Bits
import qualified Data.ByteString.Lazy as BS
import qualified Data.Sequence as Seq
import Data.Word
import Utils

-- https://www.nesdev.org/wiki/INES

data MirroringType = Horizontal | Vertical deriving (Show)

data Header = Header
  { _magicNumber :: [Word8],
    _prgRomSize :: Int,
    _chrRomSize :: Int,
    _mirroring :: MirroringType,
    _hasTrainer :: Bool,
    _hasPrgRam :: Bool,
    _enableFourScreen :: Bool,
    _mapperNumber :: Int
    -- todo: add more flags
  }
  deriving (Show)

emptyHeader :: Header
emptyHeader = Header [] 0 0 Horizontal False False False 0

makeLenses ''Header

data Cartridge = Cartridge
  { _header :: Header,
    _trainer :: Seq.Seq Word8,
    _prgRom :: Seq.Seq Word8,
    _chrRom :: Seq.Seq Word8
    -- todo: PlayChoice INST-ROM and PlayChoice PROM
    -- todo: Some ROM-Images additionally contain a 128-byte (or sometimes 127-byte) title
    --      at the end of the file.
  }

makeLenses ''Cartridge

instance Show Cartridge where
  show (Cartridge h t p c) =
    unlines
      [ show h,
        "trainer size: " ++ (show . length $ t),
        "prgRom size: " ++ (show . length $ p),
        "chrRom size: " ++ (show . length $ c)
      ]

emptyCartridge :: Cartridge
emptyCartridge = Cartridge emptyHeader Seq.empty Seq.empty Seq.empty

type CartridgeState = StateT Cartridge IO

setMagicNumber :: [Word8] -> CartridgeState ()
setMagicNumber d =
  if n == [0x4E, 0x45, 0x53, 0x1A]
    then (header . magicNumber) .= n
    else error "error: magic number incorrect."
  where
    n = take 4 d

setPrgRomSize :: [Word8] -> CartridgeState ()
setPrgRomSize d = (header . prgRomSize) .= word8ToInt (d !! 4) * 16 * 1024

setChrRomSize :: [Word8] -> CartridgeState ()
setChrRomSize d = (header . chrRomSize) .= word8ToInt (d !! 5) * 8 * 1024

setMirroring :: [Word8] -> CartridgeState ()
setMirroring d = do
  let flag6 = d !! 6
  header . mirroring .= if testBit flag6 0 then Vertical else Horizontal

setHasPrgRam :: [Word8] -> CartridgeState ()
setHasPrgRam d = do
  let flag6 = d !! 6
  header . hasPrgRam .= testBit flag6 1

setHasTrainer :: [Word8] -> CartridgeState ()
setHasTrainer d = do
  let flag6 = d !! 6
  header . hasTrainer .= testBit flag6 2

setEnableFourScreen :: [Word8] -> CartridgeState ()
setEnableFourScreen d = do
  let flag6 = d !! 6
  header . enableFourScreen .= testBit flag6 3

setMapperNumber :: [Word8] -> CartridgeState ()
setMapperNumber d = do
  let flag6 = d !! 6
  let flag7 = d !! 7
  let lower = shiftR flag6 4
  let upper = flag7 .&. 0b11110000
  header . mapperNumber .= word8ToInt (upper .&. lower)

setTrainer :: [Word8] -> CartridgeState ()
setTrainer d =
  ifM
    (use (header . hasTrainer))
    (trainer .= Seq.fromList (take 512 (drop 16 d)))
    (trainer .= Seq.empty)

setPrgRom :: [Word8] -> CartridgeState ()
setPrgRom d = do
  h <- use header
  let trainerSize = if h ^. hasTrainer then 512 else 0
  let prgRomData = take (h ^. prgRomSize) (drop (16 + trainerSize) d)
  prgRom .= Seq.fromList prgRomData

setChrRom :: [Word8] -> CartridgeState ()
setChrRom d = do
  h <- use header
  let trainerSize = if h ^. hasTrainer then 512 else 0
  let prgRomSize' = h ^. prgRomSize
  let chrRomSize' = h ^. chrRomSize
  let chrRomData = take chrRomSize' (drop (16 + trainerSize + prgRomSize') d)
  chrRom .= Seq.fromList chrRomData

readCartridge :: String -> IO Cartridge
readCartridge fileName = do
  d <- BS.readFile fileName <&> BS.unpack
  flip execStateT emptyCartridge $ do
    setMagicNumber d
    setPrgRomSize d
    setChrRomSize d
    setMirroring d
    setHasPrgRam d
    setHasTrainer d
    setEnableFourScreen d
    setMapperNumber d
    setTrainer d
    setPrgRom d
    setChrRom d