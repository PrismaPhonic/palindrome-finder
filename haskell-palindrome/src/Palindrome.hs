{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}

module Palindrome
  ( smallest
  , largest
  , isPalindrome
  , hasEvenDigits
  , collectPositiveFactorPairs
  , sumUArray
  , Result(..)
  , smallestBoxed
  , largestBoxed
  , getResult
  , caseOption
  , runServer
  ) where

import Data.Array.Unboxed (UArray, listArray, (!), bounds)
import Data.Array.ST (STUArray, newArray, readArray, writeArray, freeze)
import Control.Monad.ST (ST, runST)
import Control.Monad (forM_)
import Data.Word (Word64)
import Data.Maybe (Maybe(..))
import Data.List (sort)
import Data.Char (toUpper)
import System.IO (hFlush, stdout)
import GHC.Word (Word64(..))

-- | Unboxed Option type - zero-cost abstraction  
-- Represents Maybe Word64 as (Word64, Bool) where Bool indicates presence
type UnboxedOption = (Word64, Bool)

-- | Constructor for None (unboxed)
{-# INLINE nothingUnboxed #-}
nothingUnboxed :: UnboxedOption
nothingUnboxed = (0, False)

-- | Constructor for Some (unboxed)
{-# INLINE justUnboxed #-}
justUnboxed :: Word64 -> UnboxedOption
justUnboxed x = (x, True)

-- | Pattern matching for unboxed Option
{-# INLINE caseOption #-}
caseOption :: UnboxedOption -> (Word64 -> r) -> r -> r
caseOption (val, present) justCase nothingCase =
  if present then justCase val else nothingCase

-- | Sum elements of an unboxed array without allocating an intermediate list.
{-# INLINE sumUArray #-}
sumUArray :: UArray Int Word64 -> Word64
sumUArray arr =
  let (lo, hi) = bounds arr
      go !i !acc
        | i > hi = acc
        | otherwise = let !v = arr ! i
                       in go (i + 1) (acc + v)
  in go lo 0

-- | Fast integer square root using Newton's method
{-# INLINE fastIsqrt #-}
fastIsqrt :: Word64 -> Word64
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
hasEvenDigits :: Word64 -> Bool
hasEvenDigits n
  | n < 100 = True
  | n < 1000 = False
  | n < 10000 = True
  | n < 100000 = False
  | n < 1000000 = True
  | n < 10000000 = False
  | n < 100000000 = True
  | n < 1000000000 = False
  | n < 10000000000 = True
  | n < 100000000000 = False
  | n < 1000000000000 = True
  | n < 10000000000000 = False
  | n < 100000000000000 = True
  | n < 1000000000000000 = False
  | n < 10000000000000000 = True
  | n < 100000000000000000 = False
  | n < 1000000000000000000 = True
  | otherwise = False

-- | Check if a number is a palindrome using half-reverse method
{-# INLINE isPalindrome #-}
isPalindrome :: Word64 -> Bool
isPalindrome n
  | n < 10 = True
  | hasEvenDigits n && n `rem` 11 /= 0 = False  -- even digits must be divisible by 11
  | n `rem` 10 == 0 = False  -- trailing zero
  | otherwise =
      -- Half-reverse method
      let loop :: Word64 -> Word64 -> Bool
          loop m rev =
            if m <= rev
              then m == rev || m == (rev `quot` 10)
              else
                let digit = m `rem` 10
                    newRev = rev * 10 + digit
                in loop (m `quot` 10) newRev
      in loop n 0


-- | Collect positive factor pairs using tight divisor window
{-# INLINE collectPositiveFactorPairs #-}
collectPositiveFactorPairs :: Word64 -> Word64 -> Word64 -> UArray Int Word64
collectPositiveFactorPairs product minVal maxVal = runST $ do
  let sqrtp = fastIsqrt product
      low = max minVal ((product + maxVal - 1) `quot` maxVal)  -- ceil(product/max)
      high = min maxVal sqrtp
      maxPairs = 12  -- 6 pairs * 2 elements per pair
  arr <- newArray (0, maxPairs - 1) 0 :: ST s (STUArray s Int Word64)
  let go !count !x
        | x > high || count >= maxPairs = return count
        | product `rem` x == 0 = do
            let y = product `quot` x
            if y >= x && y <= maxVal
              then do
                writeArray arr count x
                writeArray arr (count + 1) y
                go (count + 2) (x + 1)
              else go count (x + 1)
        | otherwise = go count (x + 1)
  finalCount <- go 0 low
  -- Create a new array with only the elements we actually wrote
  if finalCount == 0
    then return $ listArray (0, -1) []  -- empty array
    else do
      result <- newArray (0, finalCount - 1) 0 :: ST s (STUArray s Int Word64)
      let copyLoop !i
            | i >= finalCount = return ()
            | otherwise = do
                v <- readArray arr i
                writeArray result i v
                copyLoop (i + 1)
      copyLoop 0
      freeze result

-- | Find smallest palindromic product
{-# INLINE smallest #-}
smallest :: Word64 -> Word64 -> UnboxedOption
smallest minVal maxVal = case searchSmallest minVal maxVal of
    (prod, False) -> nothingUnboxed
    (prod, True) -> justUnboxed prod
  where
    searchSmallest !min' !max' = searchRowsForSmallest min' (maxBound :: Word64)
      where
        -- x ascends; outer prune mirrors Rust: break when x*x >= best
        searchRowsForSmallest !x !best
          | x > max' = if best == maxBound then nothingUnboxed else justUnboxed best
          | x * x >= best = if best == maxBound then nothingUnboxed else justUnboxed best
          | otherwise = case searchRowForSmallest x best of
              (_, False) -> searchRowsForSmallest (x + 1) best
              (newBest, True) -> searchRowsForSmallest (x + 1) newBest
          where
            searchRowForSmallest !x' !currentBest
              | yUpper < x' = nothingUnboxed
              | otherwise = searchColumnForSmallest x' currentBest
              where
                yUpper = min max' ((currentBest - 1) `quot` x')
                searchColumnForSmallest !y !rowBest
                  | y > yUpper = if rowBest == maxBound then nothingUnboxed else justUnboxed rowBest
                  | otherwise = let prod = x' * y
                                in if isPalindrome prod
                                     then justUnboxed prod
                                     else searchColumnForSmallest (y + 1) rowBest

-- | Find largest palindromic product
{-# INLINE largest #-}
largest :: Word64 -> Word64 -> UnboxedOption
largest minVal maxVal = case searchLargest minVal maxVal of
    (prod, False) -> nothingUnboxed
    (prod, True) -> justUnboxed prod
  where
    searchLargest !min' !max' = searchRowsForLargest max' 0
      where
        -- x descends; outer prune mirrors Rust: break when x*max <= best
        searchRowsForLargest !x !best
          | x < min' = if best == 0 then nothingUnboxed else justUnboxed best
          | x * max' <= best = if best == 0 then nothingUnboxed else justUnboxed best
          | x == 0 = nothingUnboxed  -- no valid factors when x == 0; stop
          | otherwise = case searchRowForLargest x of
              (_, False) -> searchRowsForLargest (x - 1) best
              (newBest, True) -> searchRowsForLargest (x - 1) newBest
          where
            searchRowForLargest !x'
              | x' == 0 = nothingUnboxed
              | otherwise =
                  let yLower = max x' ((best `quot` x') + 1)
                      searchColumnForLargest !y !currentBest
                        | y < yLower = if currentBest == 0 then nothingUnboxed else justUnboxed currentBest
                        | otherwise = let prod = x' * y
                                      in if isPalindrome prod
                                           then justUnboxed prod
                                           else searchColumnForLargest (y - 1) currentBest
                  in if yLower > max'
                       then nothingUnboxed
                       else searchColumnForLargest max' 0

-- | Get result with factor pairs (unboxed version)
{-# INLINE getResult #-}
getResult :: Word64 -> Word64 -> Word64 -> Result
getResult minVal maxVal prod = Result prod (collectPositiveFactorPairs prod minVal maxVal)

-- | Convert unboxed option to Maybe for compatibility
{-# INLINE toMaybe #-}
toMaybe :: UnboxedOption -> Maybe Word64
toMaybe (val, present) = if present then Just val else Nothing

-- | Legacy Result type for compatibility
data Result = Result
  { product :: !Word64
  , pairs   :: !(UArray Int Word64)
  } deriving (Show)

-- | Wrapper functions for backward compatibility
smallestBoxed :: Word64 -> Word64 -> Maybe Result
smallestBoxed minVal maxVal = case smallest minVal maxVal of
  (prod, False) -> Nothing
  (prod, True) -> Just $ getResult minVal maxVal prod

largestBoxed :: Word64 -> Word64 -> Maybe Result
largestBoxed minVal maxVal = case largest minVal maxVal of
  (prod, False) -> Nothing
  (prod, True) -> Just $ getResult minVal maxVal prod

-- | Server protocol implementation
runServer :: (Word64 -> Word64 -> Word64 -> (Word64, Word64)) -> IO ()
runServer doIters = do
  let loop minVal maxVal = do
        line <- getLine
        let parts = words line
            cmd = case parts of
                    []    -> ""
                    (p:_) -> map toUpper p
        case cmd of
          "INIT" -> do
            let a = read (parts !! 1) :: Word64
                b = read (parts !! 2) :: Word64
            putStrLn "OK"
            hFlush stdout
            loop (Just a) (Just b)
          "WARMUP" -> do
            let iters = read (parts !! 1) :: Word64
            case (minVal, maxVal) of
              (Just a, Just b) -> do
                let (p,c) = doIters a b iters
                p `seq` c `seq` putStrLn "OK"
                hFlush stdout
                loop minVal maxVal
              _ -> do
                putStrLn "ERR NOTINIT"
                hFlush stdout
                loop minVal maxVal
          "RUN" -> do
            let iters = read (parts !! 1) :: Word64
            case (minVal, maxVal) of
              (Just a, Just b) -> do
                let (prod, cnt) = doIters a b iters
                cnt `seq` putStrLn $ "OK " ++ show prod ++ " " ++ show cnt
                hFlush stdout
                loop minVal maxVal
              _ -> do
                putStrLn "ERR NOTINIT"
                hFlush stdout
                loop minVal maxVal
          "QUIT" -> return ()
          _ -> do
            putStrLn "ERR BADCMD"
            hFlush stdout
            loop minVal maxVal
  loop Nothing Nothing
