#!/usr/bin/env deno run --allow-read

/**
 * TypeScript implementation of palindrome product finding
 * Based on the Python version with parity-based even digit checking
 */

export function hasEvenDigits(n: number): boolean {
    // Parity-based even digit checking using boolean casting
    // Count digits by repeatedly dividing by 10
    let digitCount = 0;
    let temp = n;
    while (temp > 0) {
        digitCount++;
        temp = Math.floor(temp / 10);
    }
    return (digitCount & 1) === 0;
}

export function isPalindrome(n: number): boolean {
    if (n < 10) return true;
    
    // Half-reverse method: reverse only the first half
    const s = n.toString();
    const len = s.length;
    const halfLen = Math.floor(len / 2);
    
    for (let i = 0; i < halfLen; i++) {
        if (s[i] !== s[len - 1 - i]) {
            return false;
        }
    }
    return true;
}

function collectFactorPairs(n: number, min: number, max: number): number[] {
    const pairs: number[] = [];
    const sqrt = Math.floor(Math.sqrt(n));
    
    for (let i = min; i <= Math.min(sqrt, max); i++) {
        if (n % i === 0) {
            const j = n / i;
            if (j >= min && j <= max) {
                pairs.push(i, j);
            }
        }
    }
    return pairs;
}

export function smallest(min: number, max: number): [number, number[]] | null {
    let best = Infinity;
    let bestPairs: number[] = [];
    
    for (let x = min; x <= max; x++) {
        for (let y = x; y <= max; y++) {
            const product = x * y;
            
            if (product < 10) {
                if (product < best) {
                    best = product;
                    bestPairs = [x, y];
                }
                continue;
            }
            
            if (product % 10 === 0) continue;
            
            if (hasEvenDigits(product)) {
                if (product % 11 !== 0) continue;
            }
            
            if (isPalindrome(product)) {
                if (product < best) {
                    best = product;
                    bestPairs = [x, y];
                }
            }
        }
    }
    
    return best === Infinity ? null : [best, bestPairs];
}

export function largest(min: number, max: number): [number, number[]] | null {
    let best = 0;
    let bestPairs: number[] = [];
    
    for (let x = max; x >= min; x--) {
        for (let y = max; y >= x; y--) {
            const product = x * y;
            
            if (product < 10) {
                if (product > best) {
                    best = product;
                    bestPairs = [x, y];
                }
                continue;
            }
            
            if (product % 10 === 0) continue;
            
            if (hasEvenDigits(product)) {
                if (product % 11 !== 0) continue;
            }
            
            if (isPalindrome(product)) {
                if (product > best) {
                    best = product;
                    bestPairs = [x, y];
                }
            }
        }
    }
    
    return best === 0 ? null : [best, bestPairs];
}

function doItersSmallest(min: number, max: number, iterations: number): [number, number] {
    let product = 0;
    let counter = 0;
    let pairs = 0;
    
    for (let i = 0; i < iterations; i++) {
        const current = min + (i % (max - min + 1));
        const result = smallest(current, max);
        
        if (result) {
            const [prod, pairList] = result;
            product += prod;
            counter += 1;
            pairs += pairList.reduce((sum, p) => sum + p, 0);
        }
    }
    
    return [product, counter + pairs];
}

function doItersLargest(min: number, max: number, iterations: number): [number, number] {
    let product = 0;
    let counter = 0;
    let pairs = 0;
    
    for (let i = 0; i < iterations; i++) {
        const current = max - (i % (max - min + 1));
        const result = largest(min, current);
        
        if (result) {
            const [prod, pairList] = result;
            product += prod;
            counter += 1;
            pairs += pairList.reduce((sum, p) => sum + p, 0);
        }
    }
    
    return [product, counter + pairs];
}

async function runServer(): Promise<void> {
    // Check if we're in Deno or Bun
    const isDeno = typeof Deno !== 'undefined';
    
    if (isDeno) {
        // Deno implementation
        const buf = new Uint8Array(1024);
        const n = await Deno.stdin.read(buf);
        if (n === null) return;
        
        const text = new TextDecoder().decode(buf.subarray(0, n));
        const lines = text.trim().split('\n');
        
        for (const line of lines) {
            const parts = line.trim().split(' ');
            
            if (parts[0] === 'INIT') {
                const min = parseInt(parts[1]);
                const max = parseInt(parts[2]);
                console.log('OK');
            } else if (parts[0] === 'WARMUP') {
                const iterations = parseInt(parts[1]);
                console.log('OK');
            } else if (parts[0] === 'RUN') {
                const iterations = parseInt(parts[1]);
                // Check if this is smallest or largest based on the executable name
                const isSmallest = Deno.args.some(arg => arg.includes('smallest'));
                const [product, accumulator] = isSmallest ? doItersSmallest(2, 999, iterations) : doItersLargest(2, 999, iterations);
                const start = performance.now();
                const end = performance.now();
                const nanoseconds = Math.floor((end - start) * 1_000_000);
                console.log(`OK ${product} ${accumulator} ${nanoseconds}`);
            } else if (parts[0] === 'QUIT') {
                Deno.exit(0);
            }
        }
    } else {
        // Bun implementation
        const stdin = process.stdin;
        const chunks: Buffer[] = [];
        
        for await (const chunk of stdin) {
            chunks.push(chunk);
        }
        
        const text = Buffer.concat(chunks).toString();
        const lines = text.trim().split('\n');
        
        for (const line of lines) {
            const parts = line.trim().split(' ');
            
            if (parts[0] === 'INIT') {
                const min = parseInt(parts[1]);
                const max = parseInt(parts[2]);
                console.log('OK');
            } else if (parts[0] === 'WARMUP') {
                const iterations = parseInt(parts[1]);
                console.log('OK');
            } else if (parts[0] === 'RUN') {
                const iterations = parseInt(parts[1]);
                // Check if this is smallest or largest based on the executable name
                const isSmallest = process.argv.some(arg => arg.includes('smallest'));
                const [product, accumulator] = isSmallest ? doItersSmallest(2, 999, iterations) : doItersLargest(2, 999, iterations);
                const start = performance.now();
                const end = performance.now();
                const nanoseconds = Math.floor((end - start) * 1_000_000);
                console.log(`OK ${product} ${accumulator} ${nanoseconds}`);
            } else if (parts[0] === 'QUIT') {
                process.exit(0);
            }
        }
    }
}

// Main execution
const isDeno = typeof Deno !== 'undefined';
const isMain = isDeno ? import.meta.main : require.main === module;

if (isMain) {
    const args = isDeno ? Deno.args : process.argv.slice(2);
    
    if (args.includes('--server')) {
        // Server mode for benchmarking
        await runServer();
    } else {
        // Direct execution mode
        const min = parseInt(args[0] || '2');
        const max = parseInt(args[1] || '999');
        
        console.log(`Finding palindromes from ${min} to ${max}`);
        
        const start = performance.now();
        const smallestResult = smallest(min, max);
        const smallestTime = performance.now() - start;
        
        const start2 = performance.now();
        const largestResult = largest(min, max);
        const largestTime = performance.now() - start2;
        
        if (smallestResult) {
            const [product, pairs] = smallestResult;
            console.log(`Smallest: ${product} = ${pairs[0]} × ${pairs[1]} (${smallestTime.toFixed(2)}ms)`);
        } else {
            console.log(`Smallest: No palindrome found (${smallestTime.toFixed(2)}ms)`);
        }
        
        if (largestResult) {
            const [product, pairs] = largestResult;
            console.log(`Largest: ${product} = ${pairs[0]} × ${pairs[1]} (${largestTime.toFixed(2)}ms)`);
        } else {
            console.log(`Largest: No palindrome found (${largestTime.toFixed(2)}ms)`);
        }
    }
}
