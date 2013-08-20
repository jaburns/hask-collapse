{-

    Compressed stream format description:

    Header:

      Section (I) - Code point counts per key bit size:

        Bytes at address 03 and up: P A B C D E ... 0xFF

        Each byte in this section is interpreted as an unsigned 8-bit word.

        P - This value byte defines the number of bits which overflow in to
            the padding byte at the end of the data section.
        A - This contains the number of 1 bit code points.
        B - This contains the number of 2 bit code points.
        C - etc...

        The section ends on the sentinel value 0xFF

      Section (II) - Code point keys

        This section contains the keys sorted first by their length in
        bits, and then by their magnitude.  Once all the key values
        have been exhausted, the last byte is padded, and section III
        starts immediately after.

      Section (III) - Code point values

        This section consists of all the byte values which are associated
        with the keys provided in the previous section.  They are arranged
        in the same order as their keys.  This is the final section of
        the encoding table, and the compressed data follows immediately after.
        
    Contents:  The rest of the stream simply contains the Huffman encoded data.

-}

module Huffman (

    test

  ) where

import Debug.Trace

import GHC.Exts
import Data.Word
import Data.List
import Data.Monoid
import Data.Maybe
import Data.Array.Unboxed
import Control.Monad
import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.ByteString as BS

import BinTree
import BitString


type HuffTree = BinTree (Word8,Int) -- Stores the frequency of each byte
type CodeMap  = M.Map Word8 BS.ByteString -- Maps a byte value to a string of \0 or \1 



-- Counts the occurance of each byte in a ByteString.
countBytes :: BS.ByteString -> [(Word8,Int)]
countBytes =
    dropWhile ((== 0) . snd) . sortBy sorter . assocs . BS.foldl' step zero
  where
    step :: UArray Word8 Int -> Word8 -> UArray Word8 Int
    step arr byte = arr // [(byte, 1+(arr ! byte))]
    zero = array (0,255) [] 
    sorter (_,a) (_,b) = compare a b



constructTree :: [(Word8,Int)] -> HuffTree
constructTree = go . map Leaf
  where
    go ls
     | (null . tail) ls = head ls
     | otherwise = go . sortTrees . pairUp $ ls
  
    pairUp (x:y:xs) = Node x y : pairRest xs
      where
        pairRest (a:b:cs)
         | treeSumSnd a == treeSumSnd b || null cs = Node a b : pairRest cs
         | otherwise = a:b:cs
        pairRest done = done

    sortTrees = sortBy sf
      where sf a b = compare (treeSumSnd a) (treeSumSnd b)

    treeSumSnd t  = F.foldl' (+) 0 (fmap snd t)



getCodeMap :: HuffTree -> CodeMap
getCodeMap = M.fromList . treeFlatten . go BS.empty
  where
    go code (Node l r) = (Node (go codeL l) (go codeR r))
      where
        codeL = code `BS.append` (BS.singleton 0)
        codeR = code `BS.append` (BS.singleton 1)
    go code (Leaf (v,c)) = Leaf (v,code)

    treeFlatten t = reverse $ F.foldl' (flip (:)) [] t



constructHeader :: Int -> CodeMap -> BS.ByteString
constructHeader ovr code = 
    snd . bitsPack . mconcat $
      [ bitsFromInteger 8 (toInteger ovr),
        codePointCounts code,
        bitsFromInteger 8 0xFF,
        codePointKeys code,
        codePointValues code ]

codePointCounts code =
 let groupedLengs = group . sort . map BS.length $ M.elems code
     lenCounts    = map (\(x:xs) -> (x,1+length xs)) $ groupedLengs
     allLenCounts = map (fromMaybe 0 . flip lookup lenCounts) [1..(fst $ last lenCounts)]
  in mconcat $ map (bitsFromInteger 8 . toInteger) allLenCounts  

codePointKeys code =  -- TODO: the sorting in here and codePointValues is the same
 let sortedKeys = concat . map sort . group $ sortWith BS.length (M.elems code)
     keyBits    = mconcat $ map bitsFromBytes sortedKeys
     overflows  = (BS.length . bytesFromBits $ keyBits) `mod` 8
     padBits    = bitsFromBytes . BS.pack . take (8-overflows) $ repeat 1
  in keyBits `mappend` padBits

codePointValues code = 
 let keyLenMatch a b = (BS.length . snd) a == (BS.length . snd) b
     sortedByKey = concat . map (sortWith snd) . groupBy keyLenMatch . sortWith (BS.length . snd) $ (M.assocs code)
  in bitsFromBytes . BS.pack . map fst $ sortedByKey


    
huffEncode :: BS.ByteString -> BS.ByteString
huffEncode inp = header `BS.append` content
  where
    codeMap = getCodeMap . constructTree . countBytes $ inp 
    (overflow,content) = bitsPack . bitsFromBytes $ BS.concatMap ((M.!) codeMap) inp
    header = constructHeader overflow codeMap


    
    
testFile :: IO BS.ByteString
testFile = BS.readFile "test.tar"

test :: IO ()
test =
 do file <- testFile
    let huffed = huffEncode file
    let realLen = fromIntegral . BS.length
    putStrLn . show $ (realLen huffed) / (realLen file)




