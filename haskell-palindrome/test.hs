import Palindrome (smallestBoxed, largestBoxed, isPalindrome)
import Data.Word (Word64)

main :: IO ()
main = do
    putStrLn "Testing Haskell palindrome functions..."
    
    -- Test isPalindrome
    putStrLn "Testing isPalindrome:"
    putStrLn $ "isPalindrome 9: " ++ show (isPalindrome 9)
    putStrLn $ "isPalindrome 906609: " ++ show (isPalindrome 906609)
    putStrLn $ "isPalindrome 40: " ++ show (isPalindrome 40)
    putStrLn $ "isPalindrome 123456: " ++ show (isPalindrome 123456)
    putStrLn $ "isPalindrome 10988901: " ++ show (isPalindrome 10988901)
    
    -- Test smallest
    putStrLn "\nTesting smallest:"
    putStrLn $ "smallestBoxed 910 999: " ++ show (smallestBoxed 910 999)
    putStrLn $ "smallestBoxed 100 999: " ++ show (smallestBoxed 100 999)
    putStrLn $ "smallestBoxed 1 9: " ++ show (smallestBoxed 1 9)
    putStrLn $ "smallestBoxed 10 99: " ++ show (smallestBoxed 10 99)
    putStrLn $ "smallestBoxed 1000 9999: " ++ show (smallestBoxed 1000 9999)
    
    -- Test largest
    putStrLn "\nTesting largest:"
    putStrLn $ "largestBoxed 910 999: " ++ show (largestBoxed 910 999)
    putStrLn $ "largestBoxed 100 999: " ++ show (largestBoxed 100 999)
    putStrLn $ "largestBoxed 1 9: " ++ show (largestBoxed 1 9)
    putStrLn $ "largestBoxed 10 99: " ++ show (largestBoxed 10 99)
    putStrLn $ "largestBoxed 1000 9999: " ++ show (largestBoxed 1000 9999)
    
    putStrLn "\nAll tests completed!"
