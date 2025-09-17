{-# LANGUAGE BangPatterns #-}

module Main where

import qualified Palindrome as P
import Palindrome (largest, runServer, Result(..), sumUArray)
import System.Environment (getArgs)
import Data.Word (Word64)
import Data.Maybe (maybe)
import Data.Array.Unboxed (elems)
import qualified Data.Foldable as F

-- Prevent GHC from hoisting loop-invariant calls by making the argument
-- syntactically depend on the inner-loop index. The NOINLINE pragma avoids
-- the optimizer from collapsing the dependency away.

-- | Server implementation for largest palindrome
doIters :: Word64 -> Word64 -> Word64 -> (Word64, Word64)

doIters minVal maxVal iters = 
  let rangeCount :: Word64
      rangeCount = if maxVal >= minVal then (maxVal - minVal + 1) else 0

      -- Single loop: iterate currentMax from max..min cycling, for exactly `iters` steps
      go :: Word64 -> Word64 -> Word64 -> Word64 -> (Word64, Word64)
      go !n !currentMax !acc !cnt
        | n >= iters = (acc, cnt)
        | otherwise =
            let res = largest minVal currentMax
                (acc', cnt') = case res of
                                  Nothing -> (acc, cnt)
                                  Just r  ->
                                    let prod = P.product r
                                        sPairs = sumUArray (P.pairs r)
                                        !a' = acc + prod + sPairs + cnt
                                    in (a', cnt + 1)
                nextMax = if currentMax <= minVal then maxVal else currentMax - 1
            in go (n + 1) nextMax acc' cnt'

      (!acc0, !_cntFinal) = if rangeCount == 0 then (0, 0) else go 0 maxVal 0 0
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
