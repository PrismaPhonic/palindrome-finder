{-# LANGUAGE BangPatterns #-}

module Main where

import qualified Palindrome as P
import Palindrome (largest, runServer, Result(..))
import System.Environment (getArgs)
import Data.Word (Word64)
import Data.Maybe (maybe)
import Data.Array.Unboxed (elems)
import qualified Data.Foldable as F

-- | Server implementation for largest palindrome
doIters :: Word64 -> Word64 -> Word64 -> (Word64, Word64)

doIters minVal maxVal iters = 
  let rangeCount :: Word64
      rangeCount = if maxVal >= minVal then (maxVal - minVal + 1) else 0
      q :: Word64
      q = if rangeCount > 0 then iters `div` rangeCount else 0
      r :: Word64
      r = if rangeCount > 0 then iters `mod` rangeCount else 0
      -- Base iterations: run q times for each range
      goBaseRanges :: Word64 -> Word64 -> Word64 -> Word64
      goBaseRanges idx acc cnt
        | idx >= rangeCount = acc
        | otherwise =
            let currentMax = maxVal - idx
                goRuns j a c
                  | j >= q = a
                  | otherwise =
                      let res = largest minVal currentMax
                          prod = maybe 0 P.product res
                          sPairs = case res of
                                     Nothing -> 0
                                     Just r  -> F.foldl' (+) 0 (elems (P.pairs r))
                          !a' = a + prod + sPairs + c
                      in goRuns (j + 1) a' (c + 1)
            in goBaseRanges (idx + 1) (goRuns 0 acc cnt) cnt
      
      -- Remainder iterations: run 1 additional time for first r ranges
      goRemainderRanges :: Word64 -> Word64 -> Word64 -> Word64
      goRemainderRanges idx acc cnt
        | idx >= r = acc
        | otherwise =
            let currentMax = maxVal - idx
                res = largest minVal currentMax
                prod = maybe 0 P.product res
                sPairs = case res of
                           Nothing -> 0
                           Just r' -> F.foldl' (+) 0 (elems (P.pairs r'))
                !acc' = acc + prod + sPairs + cnt
            in goRemainderRanges (idx + 1) acc' (cnt + 1)
      
      acc0 = if rangeCount == 0 then 0 else goRemainderRanges 0 (goBaseRanges 0 0 0) 0
      !base = largest minVal maxVal
      prod0 = maybe 0 P.product base
  in (prod0, acc0)



main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--server"] -> runServer doIters
    [minStr, maxStr, itersStr] -> do
      let min' = read minStr :: Word64
          max' = read maxStr :: Word64
          iters' = read itersStr :: Word64
      let (prod, _) = doIters min' max' iters'
      print prod
    _ -> do
      putStrLn "Usage: palprod-haskell-largest <min> <max> <iters> | palprod-haskell-largest --server"
      putStrLn "  --server: Run in server mode for benchmarking"
      putStrLn "  <min> <max> <iters>: Run once with given parameters"
