#!/usr/bin/env python3
"""
Test script for Python palindrome implementation.
"""

from palindrome import smallest, largest, is_palindrome, has_even_digits


def test_is_palindrome():
    """Test palindrome detection."""
    assert is_palindrome(1) == True
    assert is_palindrome(11) == True
    assert is_palindrome(121) == True
    assert is_palindrome(1221) == True
    assert is_palindrome(12321) == True
    
    assert is_palindrome(12) == False
    assert is_palindrome(123) == False
    assert is_palindrome(1234) == False
    assert is_palindrome(10) == False  # trailing zero
    assert is_palindrome(100) == False  # trailing zero


def test_has_even_digits():
    """Test even digits detection."""
    assert has_even_digits(11) == True   # 2 digits
    assert has_even_digits(123) == False # 3 digits
    assert has_even_digits(1234) == True # 4 digits
    assert has_even_digits(12345) == False # 5 digits
    assert has_even_digits(123456) == True # 6 digits


def test_smallest():
    """Test smallest palindrome finding."""
    # Single digit factors
    result = smallest(1, 9)
    assert result is not None
    product, pairs = result
    assert product == 1
    assert pairs == [1, 1]
    
    # Double digit factors
    result = smallest(10, 99)
    assert result is not None
    product, pairs = result
    assert product == 121
    assert pairs == [11, 11]
    
    # Triple digit factors
    result = smallest(100, 999)
    assert result is not None
    product, pairs = result
    assert product == 10201
    assert pairs == [101, 101]


def test_largest():
    """Test largest palindrome finding."""
    # Single digit factors
    result = largest(1, 9)
    assert result is not None
    product, pairs = result
    assert product == 9
    assert pairs == [1, 9, 3, 3]
    
    # Double digit factors
    result = largest(10, 99)
    assert result is not None
    product, pairs = result
    assert product == 9009
    assert pairs == [91, 99]
    
    # Triple digit factors
    result = largest(100, 999)
    assert result is not None
    product, pairs = result
    assert product == 906609
    assert pairs == [913, 993]


def test_edge_cases():
    """Test edge cases."""
    # Single value range with palindrome
    result = smallest(2, 2)
    assert result is not None
    product, pairs = result
    assert product == 4
    assert pairs == [2, 2]
    
    result = largest(2, 2)
    assert result is not None
    product, pairs = result
    assert product == 4
    assert pairs == [2, 2]
    
    # Another single value range with palindrome
    result = smallest(1, 1)
    assert result is not None
    product, pairs = result
    assert product == 1
    assert pairs == [1, 1]


if __name__ == "__main__":
    print("Running Python palindrome tests...")
    
    test_is_palindrome()
    print("✓ is_palindrome tests passed")
    
    test_has_even_digits()
    print("✓ has_even_digits tests passed")
    
    test_smallest()
    print("✓ smallest tests passed")
    
    test_largest()
    print("✓ largest tests passed")
    
    test_edge_cases()
    print("✓ edge case tests passed")
    
    print("\nAll tests passed! 🎉")
