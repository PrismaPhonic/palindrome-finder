{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE FlexibleContexts #-}

module Palindrome
  ( smallest
  , largest
  , isPalindrome
  , hasEvenDigits
  , collectFactorPairs
  , Result(..)
  , runServer
  ) where

import Data.Array.Unboxed (UArray, listArray, (!), bounds, elems)
import Data.Array.ST (STArray, newArray, readArray, writeArray, freeze)
import Control.Monad.ST (ST, runST)
import Control.Monad (forM_)
import Data.Word (Word64)
import Data.Maybe (Maybe(..))
import Data.List (sort)
import Data.Char (toUpper)
import System.IO (hFlush, stdout)

-- | Result type matching Rust's Option<(u64, ArrayVec<u64, 24>)>
data Result = Result
  { product :: !Word64
  , pairs   :: !(UArray Int Word64)
  } deriving (Show)

-- | Fast integer square root using Newton's method
fastIsqrt :: Word64 -> Word64
fastIsqrt n
  | n < 2 = n
  | otherwise = newton n (n `div` 2)
  where
    newton !x !guess
      | newGuess >= guess = guess
      | otherwise = newton x newGuess
      where
        newGuess = (guess + x `div` guess) `div` 2

-- | Check if a number has an even number of digits
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
isPalindrome :: Word64 -> Bool
isPalindrome n
  | n < 10 = True
  | n `mod` 10 == 0 = False  -- trailing zero
  | hasEvenDigits n && n `mod` 11 /= 0 = False  -- even digits must be divisible by 11
  | otherwise = halfReverse n 0
  where
    halfReverse !m !rev
      | m <= rev = m == rev || m == rev `div` 10
      | otherwise = halfReverse (m `div` 10) (rev * 10 + m `mod` 10)

-- | Collect factor pairs for a product, returning unboxed array
collectFactorPairs :: Word64 -> Word64 -> Word64 -> UArray Int Word64
collectFactorPairs product minVal maxVal
  | product == 0 = collectZeroFactorPairs minVal maxVal
  | otherwise = collectPositiveFactorPairs product minVal maxVal

-- | Collect zero factor pairs (0, y) for y in [min, max]
collectZeroFactorPairs :: Word64 -> Word64 -> UArray Int Word64
collectZeroFactorPairs minVal maxVal = runST $ do
  let count = fromIntegral $ 2 * (maxVal - minVal + 1)
  arr <- newArray (0, count - 1) 0 :: ST s (STArray s Int Word64)
  let go !i !y
        | y > maxVal = return ()
        | otherwise = do
            writeArray arr i 0
            writeArray arr (i + 1) y
            go (i + 2) (y + 1)
  go 0 minVal
  freeze arr

-- | Collect positive factor pairs using tight divisor window
collectPositiveFactorPairs :: Word64 -> Word64 -> Word64 -> UArray Int Word64
collectPositiveFactorPairs product minVal maxVal = runST $ do
  let sqrtp = fastIsqrt product
      low = max minVal ((product + maxVal - 1) `div` maxVal)  -- ceil(product/max)
      high = min maxVal sqrtp
      maxPairs = 12  -- 6 pairs * 2 elements per pair
  arr <- newArray (0, maxPairs - 1) 0 :: ST s (STArray s Int Word64)
  let go !count !x
        | x > high || count >= maxPairs = return count
        | product `mod` x == 0 = do
            let y = product `div` x
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
      result <- newArray (0, finalCount - 1) 0 :: ST s (STArray s Int Word64)
      forM_ [0..finalCount-1] $ \i -> do
        val <- readArray arr i
        writeArray result i val
      freeze result

-- | Find smallest palindromic product
smallest :: Word64 -> Word64 -> Maybe Result
smallest minVal maxVal
  | minVal > maxVal = Nothing
  | otherwise = case searchSmallest minVal maxVal of
      Nothing -> Nothing
      Just (prod, pairs) -> Just $ Result prod pairs
  where
    searchSmallest !min' !max' = go min' (maxBound :: Word64)
      where
        go !x !best
          | x > max' = if best == maxBound then Nothing else Just (best, collectFactorPairs best min' max')
          | x * x >= best = if best == maxBound then Nothing else Just (best, collectFactorPairs best min' max')
          | otherwise = case searchRow x best of
              Nothing -> go (x + 1) best
              Just newBest -> go (x + 1) newBest
          where
            searchRow !x' !currentBest
              | yUpper < x' = Nothing
              | otherwise = goY x' currentBest
              where
                yUpper = min max' ((currentBest - 1) `div` x')
                goY !y !rowBest
                  | y > yUpper = if rowBest == maxBound then Nothing else Just rowBest
                  | otherwise = let prod = x' * y
                                in if isPalindrome prod
                                   then Just prod  -- first palindrome in row is the minimum, return it
                                   else goY (y + 1) rowBest

-- | Find largest palindromic product
largest :: Word64 -> Word64 -> Maybe Result
largest minVal maxVal
  | minVal > maxVal = Nothing
  | otherwise = case searchLargest minVal maxVal of
      Nothing -> Nothing
      Just (prod, pairs) -> Just $ Result prod pairs
  where
    searchLargest !min' !max' = go max' 0
      where
        go !x !best
          | x < min' = if best == 0 then Nothing else Just (best, collectFactorPairs best min' max')
          | x * max' <= best = if best == 0 then Nothing else Just (best, collectFactorPairs best min' max')
          | x == 0 = Nothing  -- no valid factors when x == 0; stop
          | otherwise = case searchRow x of
              Nothing -> go (x - 1) best
              Just newBest -> go (x - 1) newBest
          where
            searchRow !x'
              | x' == 0 = Nothing
              | otherwise =
                  let yLower = max x' ((best `div` x') + 1)
                      goY !y !currentBest
                        | y < yLower = if currentBest == 0 then Nothing else Just currentBest
                        | otherwise = let prod = x' * y
                                      in if isPalindrome prod
                                         then Just prod
                                         else goY (y - 1) currentBest
                  in if yLower > max'
                       then Nothing
                       else goY max' 0

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
