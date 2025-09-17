{-# LANGUAGE BangPatterns #-}

import qualified Palindrome as P
import Palindrome (smallest, largest, isPalindrome, collectFactorPairs, Result(..))
import Data.Word (Word64)
import Data.Array.Unboxed (elems)
import Data.List (sort)

-- Helper function to normalize factor pairs for comparison
norm :: [(Word64, Word64)] -> [(Word64, Word64)]
norm = sort

-- Helper function to convert flat array to pairs
arrayToPairs :: [Word64] -> [(Word64, Word64)]
arrayToPairs [] = []
arrayToPairs [_] = []  -- odd length, ignore last element
arrayToPairs (x:y:rest) = (x, y) : arrayToPairs rest

-- Helper function to assert Some result matches expected
assertSomeEq :: Maybe (Word64, [Word64]) -> Word64 -> [(Word64, Word64)] -> IO ()
assertSomeEq got expectP expectFactors = do
    case got of
        Nothing -> error "expected Some(..), got None"
        Just (p, f) -> do
            if p /= expectP
                then error $ "product mismatch: expected " ++ show expectP ++ " got " ++ show p
                else do
                    let pairs = arrayToPairs f
                    if norm pairs /= norm expectFactors
                        then error $ "factors mismatch: expected " ++ show (norm expectFactors) ++ " got " ++ show (norm pairs)
                        else return ()

-- Test functions matching Rust exactly
testSmallest :: IO ()
testSmallest = do
    let result = smallest 910 999
    case result of
        Nothing -> error "expected Some(..), got None"
        Just r -> do
            if P.product r /= 861168
                then error $ "product mismatch: expected 861168 got " ++ show (P.product r)
                else do
                    let factors = elems (P.pairs r)
                    if length factors < 2
                        then error "expected at least 2 factors"
                        else do
                            if factors !! 0 /= 924
                                then error $ "first factor mismatch: expected 924 got " ++ show (factors !! 0)
                                else do
                                    if factors !! 1 /= 932
                                        then error $ "second factor mismatch: expected 932 got " ++ show (factors !! 1)
                                        else putStrLn "✓ test_smallest passed"

largest910_999 :: IO ()
largest910_999 = do
    let result = largest 910 999
    case result of
        Nothing -> error "expected Some(..), got None"
        Just r -> do
            if P.product r /= 906609
                then error $ "product mismatch: expected 906609 got " ++ show (P.product r)
                else do
                    let factors = elems (P.pairs r)
                    let found = checkFactorPair factors 913 993
                    if not found
                        then error "expected factor pair (913, 993) not found"
                        else putStrLn "✓ largest_910_999 passed"

largest100_999 :: IO ()
largest100_999 = do
    let result = largest 100 999
    case result of
        Nothing -> error "expected Some(..), got None"
        Just r -> do
            if P.product r /= 906609
                then error $ "product mismatch: expected 906609 got " ++ show (P.product r)
                else do
                    let factors = elems (P.pairs r)
                    let found = checkFactorPair factors 913 993
                    if not found
                        then error "expected factor pair (913, 993) not found"
                        else putStrLn "✓ largest_100_999 passed"

-- Helper to check if a factor pair exists in the flat array
checkFactorPair :: [Word64] -> Word64 -> Word64 -> Bool
checkFactorPair [] _ _ = False
checkFactorPair [_] _ _ = False
checkFactorPair (x:y:rest) a b = (x == a && y == b) || checkFactorPair rest a b

singleDigitPal :: IO ()
singleDigitPal = do
    if isPalindrome 9
        then putStrLn "✓ single_digit_pal passed"
        else error "expected isPalindrome 9 to be True"

evenSixPal :: IO ()
evenSixPal = do
    if isPalindrome 906609
        then putStrLn "✓ even_six_pal passed"
        else error "expected isPalindrome 906609 to be True"

trailingZeroPal :: IO ()
trailingZeroPal = do
    if not (isPalindrome 40)
        then putStrLn "✓ trailing_zero_pal passed"
        else error "expected isPalindrome 40 to be False"

evenNotDiv11 :: IO ()
evenNotDiv11 = do
    if not (isPalindrome 123456)
        then putStrLn "✓ even_not_div_11 passed"
        else error "expected isPalindrome 123456 to be False"

oddLengthPal :: IO ()
oddLengthPal = do
    if isPalindrome 10988901
        then putStrLn "✓ odd_length_pal passed"
        else error "expected isPalindrome 10988901 to be True"

findSmallestPalindromeFromSingleDigitFactors :: IO ()
findSmallestPalindromeFromSingleDigitFactors = do
    let result = smallest 1 9
    case result of
        Nothing -> error "expected Some(..), got None"
        Just r -> do
            if P.product r /= 1
                then error $ "product mismatch: expected 1 got " ++ show (P.product r)
                else do
                    let factors = arrayToPairs (elems (P.pairs r))
                    if norm factors /= norm [(1, 1)]
                        then error $ "factors mismatch: expected [(1,1)] got " ++ show (norm factors)
                        else putStrLn "✓ find_the_smallest_palindrome_from_single_digit_factors passed"

findLargestPalindromeFromSingleDigitFactors :: IO ()
findLargestPalindromeFromSingleDigitFactors = do
    let result = largest 1 9
    case result of
        Nothing -> error "expected Some(..), got None"
        Just r -> do
            if P.product r /= 9
                then error $ "product mismatch: expected 9 got " ++ show (P.product r)
                else do
                    let factors = arrayToPairs (elems (P.pairs r))
                    if norm factors /= norm [(1, 9), (3, 3)]
                        then error $ "factors mismatch: expected [(1,9),(3,3)] got " ++ show (norm factors)
                        else putStrLn "✓ find_the_largest_palindrome_from_single_digit_factors passed"

findSmallestPalindromeFromDoubleDigitFactors :: IO ()
findSmallestPalindromeFromDoubleDigitFactors = do
    let result = smallest 10 99
    case result of
        Nothing -> error "expected Some(..), got None"
        Just r -> do
            if P.product r /= 121
                then error $ "product mismatch: expected 121 got " ++ show (P.product r)
                else do
                    let factors = arrayToPairs (elems (P.pairs r))
                    if norm factors /= norm [(11, 11)]
                        then error $ "factors mismatch: expected [(11,11)] got " ++ show (norm factors)
                        else putStrLn "✓ find_the_smallest_palindrome_from_double_digit_factors passed"

findLargestPalindromeFromDoubleDigitFactors :: IO ()
findLargestPalindromeFromDoubleDigitFactors = do
    let result = largest 10 99
    case result of
        Nothing -> error "expected Some(..), got None"
        Just r -> do
            if P.product r /= 9009
                then error $ "product mismatch: expected 9009 got " ++ show (P.product r)
                else do
                    let factors = arrayToPairs (elems (P.pairs r))
                    if norm factors /= norm [(91, 99)]
                        then error $ "factors mismatch: expected [(91,99)] got " ++ show (norm factors)
                        else putStrLn "✓ find_the_largest_palindrome_from_double_digit_factors passed"

findSmallestPalindromeFromTripleDigitFactors :: IO ()
findSmallestPalindromeFromTripleDigitFactors = do
    let result = smallest 100 999
    case result of
        Nothing -> error "expected Some(..), got None"
        Just r -> do
            if P.product r /= 10201
                then error $ "product mismatch: expected 10201 got " ++ show (P.product r)
                else do
                    let factors = arrayToPairs (elems (P.pairs r))
                    if norm factors /= norm [(101, 101)]
                        then error $ "factors mismatch: expected [(101,101)] got " ++ show (norm factors)
                        else putStrLn "✓ find_the_smallest_palindrome_from_triple_digit_factors passed"

findLargestPalindromeFromTripleDigitFactors :: IO ()
findLargestPalindromeFromTripleDigitFactors = do
    let result = largest 100 999
    case result of
        Nothing -> error "expected Some(..), got None"
        Just r -> do
            if P.product r /= 906609
                then error $ "product mismatch: expected 906609 got " ++ show (P.product r)
                else do
                    let factors = arrayToPairs (elems (P.pairs r))
                    if norm factors /= norm [(913, 993)]
                        then error $ "factors mismatch: expected [(913,993)] got " ++ show (norm factors)
                        else putStrLn "✓ find_the_largest_palindrome_from_triple_digit_factors passed"

findSmallestPalindromeFromFourDigitFactors :: IO ()
findSmallestPalindromeFromFourDigitFactors = do
    let result = smallest 1000 9999
    case result of
        Nothing -> error "expected Some(..), got None"
        Just r -> do
            if P.product r /= 1002001
                then error $ "product mismatch: expected 1002001 got " ++ show (P.product r)
                else do
                    let factors = arrayToPairs (elems (P.pairs r))
                    if norm factors /= norm [(1001, 1001)]
                        then error $ "factors mismatch: expected [(1001,1001)] got " ++ show (norm factors)
                        else putStrLn "✓ find_the_smallest_palindrome_from_four_digit_factors passed"

findLargestPalindromeFromFourDigitFactors :: IO ()
findLargestPalindromeFromFourDigitFactors = do
    let result = largest 1000 9999
    case result of
        Nothing -> error "expected Some(..), got None"
        Just r -> do
            if P.product r /= 99000099
                then error $ "product mismatch: expected 99000099 got " ++ show (P.product r)
                else do
                    let factors = arrayToPairs (elems (P.pairs r))
                    if norm factors /= norm [(9901, 9999)]
                        then error $ "factors mismatch: expected [(9901,9999)] got " ++ show (norm factors)
                        else putStrLn "✓ find_the_largest_palindrome_from_four_digit_factors passed"

emptyResultForSmallestIfNoPalindromeInTheRange :: IO ()
emptyResultForSmallestIfNoPalindromeInTheRange = do
    let result = smallest 1002 1003
    case result of
        Nothing -> putStrLn "✓ empty_result_for_smallest_if_no_palindrome_in_the_range passed"
        Just _ -> error "expected None, got Some(..)"

emptyResultForLargestIfNoPalindromeInTheRange :: IO ()
emptyResultForLargestIfNoPalindromeInTheRange = do
    let result = largest 15 15
    case result of
        Nothing -> putStrLn "✓ empty_result_for_largest_if_no_palindrome_in_the_range passed"
        Just _ -> error "expected None, got Some(..)"

errorResultForSmallestIfMinIsMoreThanMax :: IO ()
errorResultForSmallestIfMinIsMoreThanMax = do
    let result = smallest 10000 1
    case result of
        Nothing -> putStrLn "✓ error_result_for_smallest_if_min_is_more_than_max passed"
        Just _ -> error "expected None, got Some(..)"

errorResultForLargestIfMinIsMoreThanMax :: IO ()
errorResultForLargestIfMinIsMoreThanMax = do
    let result = largest 2 1
    case result of
        Nothing -> putStrLn "✓ error_result_for_largest_if_min_is_more_than_max passed"
        Just _ -> error "expected None, got Some(..)"

smallestProductDoesNotUseTheSmallestFactor :: IO ()
smallestProductDoesNotUseTheSmallestFactor = do
    let result = smallest 3215 4000
    case result of
        Nothing -> error "expected Some(..), got None"
        Just r -> do
            if P.product r /= 10988901
                then error $ "product mismatch: expected 10988901 got " ++ show (P.product r)
                else do
                    let factors = arrayToPairs (elems (P.pairs r))
                    if norm factors /= norm [(3297, 3333)]
                        then error $ "factors mismatch: expected [(3297,3333)] got " ++ show (norm factors)
                        else putStrLn "✓ smallest_product_does_not_use_the_smallest_factor passed"

main :: IO ()
main = do
    putStrLn "Running Haskell palindrome tests (matching Rust exactly)..."
    putStrLn ""
    
    -- Run all tests in the same order as Rust
    testSmallest
    largest910_999
    largest100_999
    singleDigitPal
    evenSixPal
    trailingZeroPal
    evenNotDiv11
    oddLengthPal
    findSmallestPalindromeFromSingleDigitFactors
    findLargestPalindromeFromSingleDigitFactors
    findSmallestPalindromeFromDoubleDigitFactors
    findLargestPalindromeFromDoubleDigitFactors
    findSmallestPalindromeFromTripleDigitFactors
    findLargestPalindromeFromTripleDigitFactors
    findSmallestPalindromeFromFourDigitFactors
    findLargestPalindromeFromFourDigitFactors
    emptyResultForSmallestIfNoPalindromeInTheRange
    emptyResultForLargestIfNoPalindromeInTheRange
    errorResultForSmallestIfMinIsMoreThanMax
    errorResultForLargestIfMinIsMoreThanMax
    smallestProductDoesNotUseTheSmallestFactor
    
    putStrLn ""
    putStrLn "All tests passed!"
