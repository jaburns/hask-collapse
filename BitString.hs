
module BitString (

    BitString,

    bytesFromBits,   -- :: ByteString -> BitString
    bitsFromBytes,   -- :: BitString -> ByteString

    integerFromBits, -- :: BitString -> Integer
    bitsFromInteger, -- :: Integer -> BitString

    bitsPack,   -- :: ByteString -> (Int, BitString) --Int being overflow bit count
    bitsUnpack, -- :: Int -> BitString -> ByteString

  ) where


import Data.Bits
import Data.Word
import Data.List
import Data.Monoid
import qualified Data.ByteString as BS


-- A BitString is simply a ByteString which contains only 0x00 and 0x01
newtype BitString = BitString { bytesFromBits :: BS.ByteString }
    deriving (Eq)

instance Show BitString where
    show = concatMap show . BS.unpack . bytesFromBits 

instance Monoid BitString where
    mempty = BitString BS.empty
    mappend a b = BitString $ (bytesFromBits a) `BS.append` (bytesFromBits b)


-- Expose the BitString constructor through a function.
bitsFromBytes = BitString


-- Takes a ByteString where each byte represents a single bit, and
-- reconstructs an integer.  (Little endian)
integerFromBits :: BitString -> Integer
integerFromBits = sum . zipWith zf [0..] . BS.unpack . bytesFromBits
  where zf i b = if b > 0 then bit i else 0


-- Appends 0x00 on to the end of a ByteString to pad it to the desired size 'n'.
appendZeros :: Int -> BS.ByteString -> BS.ByteString
appendZeros n str = str `BS.append` zeros (n - BS.length str)
  where
    zeros n
     | n <= 0    = BS.empty
     | otherwise = BS.singleton 0 `BS.append` zeros (n-1) 


-- Given an arbitrary integer, this function will return a bit string
-- of the minimum length required to represent it in little endian notation.
bitsFromInteger :: Int -> Integer -> BitString
bitsFromInteger n 0   = BitString . (appendZeros n) . BS.singleton $ 0
bitsFromInteger n int = BitString . (appendZeros n) $ go 0
  where
    go i
     | bit i > int = BS.empty
     | otherwise   = BS.singleton (if testBit int i then 1 else 0) `BS.append` go (i+1)


-- Given a bit string of arbitrary length this function will pack the bits in to bytes
-- padding the end with zeros.  The number of bits which overflowed in to the padded
-- byte are returned fst.  There is no padding byte if 0 is returned.
bitsPack :: BitString -> (Int, BS.ByteString)
bitsPack bits =
    ((BS.length . bytesFromBits $ bits) `mod` 8, go . bytesFromBits $ bits) 
  where
    go inp
      | BS.length inp <= 8 = BS.singleton $ packByte inp
      | otherwise = BS.singleton (packByte (BS.take 8 inp)) `BS.append` go (BS.drop 8 inp)

    packByte = fromIntegral . integerFromBits . bitsFromBytes


-- Unpacks a bytestring in to its constituent bits, ignoring the {pad} higher order
-- bits in the final byte.
bitsUnpack :: Int -> BS.ByteString -> BitString
bitsUnpack over bytes
  | over == 0 = (BitString . unpackInit) bytes
  | otherwise = BitString $
    (unpackInit . BS.init) bytes `BS.append` (unpackLast . BS.last) bytes
  where
    unpackInit = foldl' BS.append BS.empty
               . map (bytesFromBits . bitsFromWord8) . BS.unpack

    unpackLast = BS.take over . bytesFromBits . bitsFromWord8

    bitsFromWord8 = bitsFromInteger 8 . toInteger


-- A test method which should return true for arbitrary BitString inputs.
packUnpack bits = bitsUnpack pad bytes == bits
  where (pad,bytes) = bitsPack bits


