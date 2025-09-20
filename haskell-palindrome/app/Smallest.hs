{-# LANGUAGE BangPatterns #-}

module Main where

import qualified Palindrome as P
import Palindrome (smallest, runServer, Result(..), sumUArray)
import System.Environment (getArgs)
import Data.Word (Word64)
import Data.Maybe (maybe)
import Data.Bits (xor, (.&.))
import Data.Array.Unboxed (elems)
import qualified Data.Foldable as F

-- | Server implementation for smallest palindrome
doIters :: Word64 -> Word64 -> Word64 -> (Word64, Word64)

doIters minVal maxVal iters = 
  let -- Single loop: iterate currentMin from min..max cycling, for exactly `iters` steps
      iterateSmallest :: Word64 -> Word64 -> Word64 -> Word64 -> (Word64, Word64)
      iterateSmallest !n !currentMin !acc !cnt
        | n >= iters = (acc, cnt)
        | otherwise =
            let res = smallest currentMin maxVal
                (acc', cnt') = case res of
                                  Nothing -> (acc, cnt)
                                  Just r  ->
                                    let prod = P.product r
                                        sPairs = sumUArray (P.pairs r)
                                        !a' = acc + prod + sPairs + cnt
                                    in (a', cnt + 1)
                nextMin = if currentMin >= maxVal then minVal else currentMin + 1
            in iterateSmallest (n + 1) nextMin acc' cnt'

      (!acc0, !_cntFinal) = iterateSmallest 0 minVal 0 0
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
