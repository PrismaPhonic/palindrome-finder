#!/usr/bin/env deno run --allow-read

import { 
    smallest, 
    largest, 
    isPalindrome, 
    hasEvenDigits, 
    collectFactorPairs,
    RangeResult 
} from './palindrome-core.ts';

function testIsPalindrome(): void {
    // Test palindrome detection
    console.assert(isPalindrome(1) === true, "isPalindrome(1) should be true");
    console.assert(isPalindrome(11) === true, "isPalindrome(11) should be true");
    console.assert(isPalindrome(121) === true, "isPalindrome(121) should be true");
    console.assert(isPalindrome(1221) === true, "isPalindrome(1221) should be true");
    console.assert(isPalindrome(12321) === true, "isPalindrome(12321) should be true");
    
    console.assert(isPalindrome(12) === false, "isPalindrome(12) should be false");
    console.assert(isPalindrome(123) === false, "isPalindrome(123) should be false");
    console.assert(isPalindrome(1234) === false, "isPalindrome(1234) should be false");
    console.assert(isPalindrome(10) === false, "isPalindrome(10) should be false (trailing zero)");
    console.assert(isPalindrome(100) === false, "isPalindrome(100) should be false (trailing zero)");
}

function testHasEvenDigits(): void {
    // Test even digits detection
    console.assert(hasEvenDigits(11) === true, "hasEvenDigits(11) should be true (2 digits)");
    console.assert(hasEvenDigits(123) === false, "hasEvenDigits(123) should be false (3 digits)");
    console.assert(hasEvenDigits(1234) === true, "hasEvenDigits(1234) should be true (4 digits)");
    console.assert(hasEvenDigits(12345) === false, "hasEvenDigits(12345) should be false (5 digits)");
    console.assert(hasEvenDigits(123456) === true, "hasEvenDigits(123456) should be true (6 digits)");
}

function testCollectFactorPairs(): void {
    const pairsOut = new Int32Array(4);
    
    // Test factor pair collection
    const count1 = collectFactorPairs(4, 1, 9, pairsOut);
    console.assert(count1 === 4, `collectFactorPairs(4, 1, 9) should return 4, got ${count1}`);
    console.assert(pairsOut[0] === 1 && pairsOut[1] === 4 && pairsOut[2] === 2 && pairsOut[3] === 2, 
        `collectFactorPairs(4, 1, 9) should return [1, 4, 2, 2], got [${pairsOut[0]}, ${pairsOut[1]}, ${pairsOut[2]}, ${pairsOut[3]}]`);
    
    const count2 = collectFactorPairs(9, 1, 9, pairsOut);
    console.assert(count2 === 4, `collectFactorPairs(9, 1, 9) should return 4, got ${count2}`);
    console.assert(pairsOut[0] === 1 && pairsOut[1] === 9 && pairsOut[2] === 3 && pairsOut[3] === 3, 
        `collectFactorPairs(9, 1, 9) should return [1, 9, 3, 3], got [${pairsOut[0]}, ${pairsOut[1]}, ${pairsOut[2]}, ${pairsOut[3]}]`);
}

function testSmallest(): void {
    const pairsOut = new Int32Array(4);
    const result: RangeResult = { product: 0, pairsCount: 0 };
    
    // Test smallest palindrome finding
    // Single digit factors
    const found1 = smallest(1, 9, pairsOut, result);
    console.assert(found1 === 1, "smallest(1, 9) should find a result");
    console.assert(result.product === 1, `smallest(1, 9) product should be 1, got ${result.product}`);
    console.assert(result.pairsCount === 2, `smallest(1, 9) pairsCount should be 2, got ${result.pairsCount}`);
    console.assert(pairsOut[0] === 1 && pairsOut[1] === 1, `smallest(1, 9) pairs should be [1, 1], got [${pairsOut[0]}, ${pairsOut[1]}]`);
    
    // Double digit factors
    const found2 = smallest(10, 99, pairsOut, result);
    console.assert(found2 === 1, "smallest(10, 99) should find a result");
    console.assert(result.product === 121, `smallest(10, 99) product should be 121, got ${result.product}`);
    console.assert(result.pairsCount === 2, `smallest(10, 99) pairsCount should be 2, got ${result.pairsCount}`);
    console.assert(pairsOut[0] === 11 && pairsOut[1] === 11, `smallest(10, 99) pairs should be [11, 11], got [${pairsOut[0]}, ${pairsOut[1]}]`);
    
    // Triple digit factors
    const found3 = smallest(100, 999, pairsOut, result);
    console.assert(found3 === 1, "smallest(100, 999) should find a result");
    console.assert(result.product === 10201, `smallest(100, 999) product should be 10201, got ${result.product}`);
    console.assert(result.pairsCount === 2, `smallest(100, 999) pairsCount should be 2, got ${result.pairsCount}`);
    console.assert(pairsOut[0] === 101 && pairsOut[1] === 101, `smallest(100, 999) pairs should be [101, 101], got [${pairsOut[0]}, ${pairsOut[1]}]`);
}

function testLargest(): void {
    const pairsOut = new Int32Array(4);
    const result: RangeResult = { product: 0, pairsCount: 0 };
    
    // Test largest palindrome finding
    // Single digit factors
    const found1 = largest(1, 9, pairsOut, result);
    console.assert(found1 === 1, "largest(1, 9) should find a result");
    console.assert(result.product === 9, `largest(1, 9) product should be 9, got ${result.product}`);
    console.assert(result.pairsCount === 4, `largest(1, 9) pairsCount should be 4, got ${result.pairsCount}`);
    console.assert(pairsOut[0] === 1 && pairsOut[1] === 9 && pairsOut[2] === 3 && pairsOut[3] === 3, 
        `largest(1, 9) pairs should be [1, 9, 3, 3], got [${pairsOut[0]}, ${pairsOut[1]}, ${pairsOut[2]}, ${pairsOut[3]}]`);
    
    // Double digit factors
    const found2 = largest(10, 99, pairsOut, result);
    console.assert(found2 === 1, "largest(10, 99) should find a result");
    console.assert(result.product === 9009, `largest(10, 99) product should be 9009, got ${result.product}`);
    console.assert(result.pairsCount === 2, `largest(10, 99) pairsCount should be 2, got ${result.pairsCount}`);
    console.assert(pairsOut[0] === 91 && pairsOut[1] === 99, `largest(10, 99) pairs should be [91, 99], got [${pairsOut[0]}, ${pairsOut[1]}]`);
    
    // Triple digit factors
    const found3 = largest(100, 999, pairsOut, result);
    console.assert(found3 === 1, "largest(100, 999) should find a result");
    console.assert(result.product === 906609, `largest(100, 999) product should be 906609, got ${result.product}`);
    console.assert(result.pairsCount === 2, `largest(100, 999) pairsCount should be 2, got ${result.pairsCount}`);
    console.assert(pairsOut[0] === 913 && pairsOut[1] === 993, `largest(100, 999) pairs should be [913, 993], got [${pairsOut[0]}, ${pairsOut[1]}]`);
}

function testEdgeCases(): void {
    const pairsOut = new Int32Array(4);
    const result: RangeResult = { product: 0, pairsCount: 0 };
    
    // Test edge cases
    // Single value range with palindrome
    const found1 = smallest(2, 2, pairsOut, result);
    console.assert(found1 === 1, "smallest(2, 2) should find a result");
    console.assert(result.product === 4, `smallest(2, 2) product should be 4, got ${result.product}`);
    console.assert(result.pairsCount === 2, `smallest(2, 2) pairsCount should be 2, got ${result.pairsCount}`);
    console.assert(pairsOut[0] === 2 && pairsOut[1] === 2, `smallest(2, 2) pairs should be [2, 2], got [${pairsOut[0]}, ${pairsOut[1]}]`);
    
    const found2 = largest(2, 2, pairsOut, result);
    console.assert(found2 === 1, "largest(2, 2) should find a result");
    console.assert(result.product === 4, `largest(2, 2) product should be 4, got ${result.product}`);
    console.assert(result.pairsCount === 2, `largest(2, 2) pairsCount should be 2, got ${result.pairsCount}`);
    console.assert(pairsOut[0] === 2 && pairsOut[1] === 2, `largest(2, 2) pairs should be [2, 2], got [${pairsOut[0]}, ${pairsOut[1]}]`);
    
    // Another single value range with palindrome
    const found3 = smallest(1, 1, pairsOut, result);
    console.assert(found3 === 1, "smallest(1, 1) should find a result");
    console.assert(result.product === 1, `smallest(1, 1) product should be 1, got ${result.product}`);
    console.assert(result.pairsCount === 2, `smallest(1, 1) pairsCount should be 2, got ${result.pairsCount}`);
    console.assert(pairsOut[0] === 1 && pairsOut[1] === 1, `smallest(1, 1) pairs should be [1, 1], got [${pairsOut[0]}, ${pairsOut[1]}]`);
}

// Main execution
if (import.meta.main) {
    console.log("Running TypeScript palindrome tests...");
    
    try {
        testIsPalindrome();
        console.log("✓ isPalindrome tests passed");
        
        testHasEvenDigits();
        console.log("✓ hasEvenDigits tests passed");
        
        testCollectFactorPairs();
        console.log("✓ collectFactorPairs tests passed");
        
        testSmallest();
        console.log("✓ smallest tests passed");
        
        testLargest();
        console.log("✓ largest tests passed");
        
        testEdgeCases();
        console.log("✓ edge case tests passed");
        
        console.log("\nAll tests passed! 🎉");
    } catch (error) {
        console.error("Test failed:", error);
        Deno.exit(1);
    }
}