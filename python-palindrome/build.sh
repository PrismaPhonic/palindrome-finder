#!/bin/zsh
set -e

# Build script for Python palindrome solution
echo "Building Python palindrome solution..."

# Check Python version (need 3.8+ for math.isqrt)
python_version=$(python3 -c "import sys; print(f'{sys.version_info.major}.{sys.version_info.minor}')")
required_version="3.8"
if [ "$(printf '%s\n' "$required_version" "$python_version" | sort -V | head -n1)" != "$required_version" ]; then
    echo "Error: Python 3.8+ required, found $python_version"
    exit 1
fi

# Create virtual environment if it doesn't exist
if [ ! -d "venv" ]; then
    echo "Creating virtual environment..."
    python3 -m venv venv
fi

# Activate virtual environment
echo "Activating virtual environment..."
source venv/bin/activate


# Create target directory
mkdir -p ../target-bin

# Create executable scripts
echo "Creating executable scripts..."

# Smallest executable (regular Python)
cat > ../target-bin/palprod-python-smallest << 'EOF'
#!/bin/bash
cd "$(dirname "$0")/../python-palindrome"
source venv/bin/activate
exec python palindrome.py smallest --server
EOF

# Largest executable (regular Python)
cat > ../target-bin/palprod-python-largest << 'EOF'
#!/bin/bash
cd "$(dirname "$0")/../python-palindrome"
source venv/bin/activate
exec python palindrome.py largest --server
EOF

# PyPy executables with optimized JIT configurations
# Optimal settings: vectorization for smallest, ultrafast for largest
cat > ../target-bin/palprod-py-smallest << 'EOF'
#!/bin/bash
cd "$(dirname "$0")/../python-palindrome"
exec pypy3 --jit function_threshold=100,threshold=100,vec=1 palindrome.py smallest --server
EOF

cat > ../target-bin/palprod-py-largest << 'EOF'
#!/bin/bash
cd "$(dirname "$0")/../python-palindrome"
exec pypy3 --jit function_threshold=30,threshold=30 palindrome.py largest --server
EOF


# Make executables
chmod +x ../target-bin/palprod-python-smallest
chmod +x ../target-bin/palprod-python-largest
chmod +x ../target-bin/palprod-py-smallest
chmod +x ../target-bin/palprod-py-largest

echo "Build complete!"
echo "Executables created:"
echo "  - ../target-bin/palprod-python-smallest (regular Python)"
echo "  - ../target-bin/palprod-python-largest (regular Python)"
echo "  - ../target-bin/palprod-py-smallest (PyPy)"
echo "  - ../target-bin/palprod-py-largest (PyPy)"
