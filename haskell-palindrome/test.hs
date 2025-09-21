import Palindrome (smallest, largest, isPalindrome)
import Data.Word (Word32)

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
    putStrLn $ "smallest 910 999: " ++ show (smallest 910 999)
    putStrLn $ "smallest 100 999: " ++ show (smallest 100 999)
    putStrLn $ "smallest 1 9: " ++ show (smallest 1 9)
    putStrLn $ "smallest 10 99: " ++ show (smallest 10 99)
    putStrLn $ "smallest 1000 9999: " ++ show (smallest 1000 9999)
    
    -- Test largest
    putStrLn "\nTesting largest:"
    putStrLn $ "largest 910 999: " ++ show (largest 910 999)
    putStrLn $ "largest 100 999: " ++ show (largest 100 999)
    putStrLn $ "largest 1 9: " ++ show (largest 1 9)
    putStrLn $ "largest 10 99: " ++ show (largest 10 99)
    putStrLn $ "largest 1000 9999: " ++ show (largest 1000 9999)
    
    putStrLn "\nAll tests completed!"
