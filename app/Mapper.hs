module Mapper where

data MapperType = Mapper000 deriving (Enum, Show)

getMapperType :: Int -> MapperType
getMapperType = toEnum
