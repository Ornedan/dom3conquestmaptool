module Types where

import Data.Map
import Data.Word


--------------------------------------------------------------------------------

data Colour = Colour { red   :: Word8,
                       green :: Word8,
                       blue  :: Word8 }
            deriving (Show)

data Config = Config { borderColour :: Colour,
                       wraparound   :: Bool,
                       nations      :: Map String Colour,
                       provinces    :: Map Int String }
            deriving (Show)
