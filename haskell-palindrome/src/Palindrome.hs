{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Palindrome
  ( smallest
  , largest
  , isPalindrome
  , hasEvenDigits
  , collectPositiveFactorPairs
  , sumUArray
  , Result(..)
  , runServer
  ) where

import Data.Array.Unboxed (UArray, listArray, (!), bounds)
import Data.Array.ST (STUArray, newArray, readArray, writeArray, freeze)
import Control.Monad.ST (ST, runST)
import Control.Monad (forM_)
import Data.Word (Word32, Word8, Word64)
import Data.Maybe (Maybe(..))
import Data.List (sort)
import System.IO (hFlush, stdout, stdin)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LBS
import GHC.Word (Word32(..))
import GHC.Clock (getMonotonicTimeNSec)


-- | Result type matching Rust's Option<(u32, ArrayVec<u32, 4>)>
data Result = Result
  { product :: !Word32
  , pairs   :: !(UArray Int Word32)
  } deriving (Show)

-- | Sum elements of an unboxed array without allocating an intermediate list.
{-# INLINE sumUArray #-}
sumUArray :: UArray Int Word32 -> Word32
sumUArray arr =
  let (lo, hi) = bounds arr
      go !i !acc
        | i > hi = acc
        | otherwise = let !v = arr ! i
                       in go (i + 1) (acc + v)
  in go lo 0

-- | Fast integer square root using Newton's method
{-# INLINE fastIsqrt #-}
fastIsqrt :: Word32 -> Word32
fastIsqrt n
  | n < 2 = n
  | otherwise = newton n (n `quot` 2)
  where
    newton !x !guess
      | newGuess >= guess = guess
      | otherwise = newton x newGuess
      where
        newGuess = (guess + x `quot` guess) `quot` 2

-- | Check if a number has an even number of digits
{-# INLINE hasEvenDigits #-}
hasEvenDigits :: Word32 -> Bool
hasEvenDigits n
  | n < 100 = True
  | n < 1000 = False
  | n < 10000 = True
  | n < 100000 = False
  | n < 1000000 = True
  | n < 10000000 = False
  | n < 100000000 = True
  | n < 1000000000 = False
  | otherwise = True  -- 10 digits (max for Word32)

-- | Check if a number is a palindrome using half-reverse method
{-# INLINE isPalindrome #-}
isPalindrome :: Word32 -> Bool
isPalindrome n
  | n < 10 = True
  | hasEvenDigits n && n `rem` 11 /= 0 = False  -- even digits must be divisible by 11
  | n `rem` 10 == 0 = False  -- trailing zero
  | otherwise =
      -- Half-reverse method
      let loop :: Word32 -> Word32 -> Bool
          loop m rev =
            if m <= rev
              then m == rev || m == (rev `quot` 10)
              else
                let (q, digit) = m `quotRem` 10
                    newRev = rev * 10 + digit
                in loop q newRev
      in loop n 0


-- | Collect positive factor pairs using tight divisor window
{-# INLINE collectPositiveFactorPairs #-}
collectPositiveFactorPairs :: Word32 -> Word32 -> Word32 -> UArray Int Word32
collectPositiveFactorPairs product minVal maxVal = runST $ do
  let sqrtp = fastIsqrt product
      low = max minVal ((product + maxVal - 1) `quot` maxVal)  -- ceil(product/max)
      high = min maxVal sqrtp
      maxPairs = 4  -- 2 pairs * 2 elements per pair
  arr <- newArray (0, maxPairs - 1) 0 :: ST s (STUArray s Int Word32)
  let go !count !x
        | x > high || count >= maxPairs = return count
        | let (q, r) = product `quotRem` x
        , r == 0 = do
            let y = q
            writeArray arr count x
            writeArray arr (count + 1) y
            go (count + 2) (x + 1)
        | otherwise = go count (x + 1)
  finalCount <- go 0 low
  -- Create a new array with only the elements we actually wrote
  if finalCount == 0
    then return $ listArray (0, -1) []  -- empty array
    else do
      result <- newArray (0, finalCount - 1) 0 :: ST s (STUArray s Int Word32)
      let copyLoop !i
            | i >= finalCount = return ()
            | otherwise = do
                v <- readArray arr i
                writeArray result i v
                copyLoop (i + 1)
      copyLoop 0
      freeze result


{-# INLINE smallest #-}
smallest :: Word32 -> Word32 -> Maybe Result
smallest minVal maxVal = case searchSmallest minVal maxVal of
  Nothing            -> Nothing
  Just (prod, pairs) -> Just (Result prod pairs)
  where
    noHit :: Word32
    noHit = maxBound

    -- Return the product and the factor-pair array (UArray)
    searchSmallest :: Word32 -> Word32 -> Maybe (Word32, UArray Int Word32)
    searchSmallest !min' !max' = searchRows min' noHit
      where
        -- x ascends; break when x*x >= best
        searchRows :: Word32 -> Word32 -> Maybe (Word32, UArray Int Word32)
        searchRows !x !best
          | x > max'        = done best
          | x * x >= best   = done best
          | otherwise       =
              case searchRow x best of
                rowBest | rowBest == noHit -> searchRows (x + 1) best
                        | otherwise        -> searchRows (x + 1) rowBest
          where
            done b
              | b == noHit  = Nothing
              | otherwise   = Just (b, collectPositiveFactorPairs b min' max')

        -- One row: hoist initial prod = x'*x' and add x' each step
        searchRow :: Word32 -> Word32 -> Word32
        searchRow !x' !currentBest
          | yUpper < x'   = noHit
          | otherwise     = go x' (x' * x')
          where
            !yUpper = min max' ((currentBest - 1) `quot` x')
            go !y !prod
              | y > yUpper     = noHit
              | isPalindrome prod = prod
              | otherwise      = go (y + 1) (prod + x')


{-# INLINE largest #-}
largest :: Word32 -> Word32 -> Maybe Result
largest minVal maxVal = case searchLargest minVal maxVal of
  Nothing            -> Nothing
  Just (prod, pairs) -> Just (Result prod pairs)
  where
    noHit :: Word32
    noHit = 0

    -- Return the product and the factor-pair array (UArray)
    searchLargest :: Word32 -> Word32 -> Maybe (Word32, UArray Int Word32)
    searchLargest !min' !max' = searchRows max' noHit
      where
        -- x descends; break when x*max' <= best
        searchRows :: Word32 -> Word32 -> Maybe (Word32, UArray Int Word32)
        searchRows !x !best
          | x < min'            = done best
          | x * max' <= best    = done best
          | otherwise           =
              let !rowBest = searchRow x best
              in if rowBest == noHit
                   then searchRows (x - 1) best
                   else searchRows (x - 1) rowBest
          where
            done b
              | b == noHit  = Nothing
              | otherwise   = Just (b, collectPositiveFactorPairs b min' max')

        -- One row: hoist prod = x'*max' and subtract x' each step
        searchRow :: Word32 -> Word32 -> Word32
        searchRow !x' !bestSoFar
          | yLower > max' = noHit
          | otherwise     = go max' (x' * max')
          where
            !q      = bestSoFar `quot` x'
            !yLower = max x' (q + 1)
            go !y !prod
              | y < yLower       = noHit
              | isPalindrome prod = prod
              | otherwise        = go (y - 1) (prod - x')


-- | Server protocol implementation
runServer :: (Word32 -> Word32 -> Word32 -> (Word32, Word64)) -> IO ()
runServer doIters = do
  let loop !minV !maxV = do
        line <- C8.hGetLine stdin
        case C8.head line of
          'I' -> do
            -- INIT <min> <max> (single spaces)
            let rest   = C8.drop 5 line
                (aBS, rest2) = C8.break (== ' ') rest
                bBS    = C8.drop 1 rest2
                a      = parseW32 aBS
                b      = parseW32 bBS
            C8.hPutStrLn stdout "OK"
            hFlush stdout
            loop a b
          'W' -> do
            -- WARMUP <iters>
            let iters = parseW32 (C8.drop 7 line)
                !_ = doIters minV maxV iters
            C8.hPutStrLn stdout "OK"
            hFlush stdout
            loop minV maxV
          'R' -> do
            -- RUN <iters>
            let iters = parseW32 (C8.drop 4 line)
            start <- getMonotonicTimeNSec
            let (!prod, !cnt) = doIters minV maxV iters
            end <- getMonotonicTimeNSec
            let nanos :: Word64
                nanos = end - start
                bld = BB.byteString "OK "
                      <> BB.word32Dec prod
                      <> BB.char8 ' '
                      <> BB.word64Dec cnt
                      <> BB.char8 ' '
                      <> BB.word64Dec nanos
                      <> BB.char8 '\n'
            LBS.hPut stdout (BB.toLazyByteString bld)
            hFlush stdout
            loop minV maxV
          'Q' -> return ()
          _   -> loop minV maxV
  loop 0 0

-- Parse unsigned decimal Word32 from ASCII bytes
{-# INLINE parseW32 #-}
parseW32 :: BS.ByteString -> Word32
parseW32 = BS.foldl' step 0
  where
    step :: Word32 -> Word8 -> Word32
    step acc w = acc * 10 + fromIntegral (w - 48)
