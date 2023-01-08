module Hash (MurmurHash, Hashable, Hasher, newHash, addHash, endHash, thenHash) where
import Data.Word
import Data.Bits
import Data.Coerce (coerce)
import Data.List (foldl')
import Data.Char (ord)
import Numeric (showHex)

class Hasher h where
    newHash :: h
    endHash :: h -> Word32
    addHash :: h -> Word32 -> h

class Hashable a where
    thenHash :: Hasher h => h -> a -> h

newtype MurmurHash = MurmurHashCons Word32

xorShift :: Int -> Word32 -> Word32
xorShift s w = w `xor` (w `shiftR` s)

hashFoldable :: (Hasher h, Hashable a, Foldable f) => h -> f a -> h
hashFoldable = foldl' thenHash

murmurM :: Word32
murmurM = 0x5bd1e995

murmurR :: Int
murmurR = 24

instance Show MurmurHash where
    show (MurmurHashCons h) = "Hash 0x" ++ showHex h ""

instance Hasher MurmurHash where
    newHash = MurmurHashCons 0x8008135F
    endHash = xorShift 15 . (* murmurM) . xorShift 13 . coerce
    addHash (MurmurHashCons h) = MurmurHashCons . ((h * murmurM) `xor`) . (* murmurM) . xorShift murmurR . (* murmurM)

instance Hashable Char where
    thenHash h c = addHash h ((fromIntegral $ ord c) :: Word32)

instance Hashable a => Hashable [a] where
    thenHash = hashFoldable
