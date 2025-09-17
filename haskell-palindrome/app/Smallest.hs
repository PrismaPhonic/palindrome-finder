{-# LANGUAGE BangPatterns #-}

module Main where

import qualified Palindrome as P
import Palindrome (smallest, runServer, Result(..))
import System.Environment (getArgs)
import Data.Word (Word64)
import Data.Maybe (maybe)
import Data.Bits (xor, (.&.))
import Data.Array.Unboxed (elems)
import qualified Data.Foldable as F

-- | Server implementation for smallest palindrome
doIters :: Word64 -> Word64 -> Word64 -> (Word64, Word64)

doIters minVal maxVal iters = 
  let rangeCount :: Word64
      rangeCount = if maxVal >= minVal then (maxVal - minVal + 1) else 0
      q :: Word64
      q = if rangeCount > 0 then iters `div` rangeCount else 0
      r :: Word64
      r = if rangeCount > 0 then iters `mod` rangeCount else 0
      -- Base iterations: run q times for each range
      goBaseRanges :: Word64 -> Word64 -> Word64 -> (Word64, Word64)
      goBaseRanges idx acc cnt
        | idx >= rangeCount = (acc, cnt)
        | otherwise =
            let currentMin = minVal + idx
                goRuns j a c
                  | j >= q = (a, c)
                  | otherwise =
                      let res = smallest currentMin maxVal
                      in case res of
                           Nothing ->
                             goRuns (j + 1) a c
                           Just r  ->
                             let prod = P.product r
                                 sPairs = F.foldl' (+) 0 (elems (P.pairs r))
                                 !a' = a + prod + sPairs + c
                             in goRuns (j + 1) a' (c + 1)
                (!acc1, !cnt1) = goRuns 0 acc cnt
            in goBaseRanges (idx + 1) acc1 cnt1
      
      -- Remainder iterations: run 1 additional time for first r ranges
      goRemainderRanges :: Word64 -> Word64 -> Word64 -> (Word64, Word64)
      goRemainderRanges idx acc cnt
        | idx >= r = (acc, cnt)
        | otherwise =
            let currentMin = minVal + idx
            in case smallest currentMin maxVal of
                 Nothing ->
                   goRemainderRanges (idx + 1) acc cnt
                 Just r' ->
                   let prod = P.product r'
                       sPairs = F.foldl' (+) 0 (elems (P.pairs r'))
                       !acc' = acc + prod + sPairs + cnt
                   in goRemainderRanges (idx + 1) acc' (cnt + 1)
      
      (!accAfterBase, !cntAfterBase) = if rangeCount == 0 then (0, 0) else goBaseRanges 0 0 0
      (!acc0, !_cntFinal) = if rangeCount == 0 then (0, 0) else goRemainderRanges 0 accAfterBase cntAfterBase
      !base = smallest minVal maxVal
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
      putStrLn "Usage: palprod-haskell-smallest <min> <max> <iters> | palprod-haskell-smallest --server"
      putStrLn "  --server: Run in server mode for benchmarking"
      putStrLn "  <min> <max> <iters>: Run once with given parameters"
