// Cross-runtime monotonic-ish clock → nanoseconds as BigInt (no floats)
function nowNanosBigInt(): bigint {
    // Bun/Node style
    // @ts-ignore
    if (typeof process !== "undefined" && process.hrtime && process.hrtime.bigint) {
        // @ts-ignore
        return process.hrtime.bigint();
    }
    // Deno: prefer Deno.hrtime if present
    // @ts-ignore
    if (typeof Deno !== "undefined" && typeof Deno.hrtime === "function") {
        // @ts-ignore
        return Deno.hrtime(); // already bigint nanos in modern Deno
    }
    // Fallback: performance.now (ms as float) -> integer nanos then BigInt
    // We avoid exposing floats outside by rounding once here.
    const ns = Math.floor(performance.now() * 1_000_000);
    return BigInt(ns);
}

export function hasEvenDigits(n: number): boolean {
    // Parity-based even digit checking using boolean casting
    const cmp_a = Number(n >= 100);
    const cmp_b = Number(n >= 1000);
    const cmp_c = Number(n >= 10000);
    const cmp_d = Number(n >= 100000);
    const t0 = cmp_a ^ cmp_b;
    const t1 = cmp_c ^ cmp_d;
    const parity = 1 ^ (t0 ^ t1);
    return parity === 1;
}

export function isPalindrome(n: number): boolean {
    if (n < 10) return true;

    // Non-zero numbers ending in 0 cannot be palindromes
    if (n % 10 === 0) return false;

    // Even-length palindromes must be divisible by 11
    if (hasEvenDigits(n) && n % 11 !== 0) return false;

    // Half-reverse method
    let m = n;
    let rev = 0;
    while (m > rev) {
        rev = rev * 10 + (m % 10);
        m = (m / 10) | 0;
    }

    return m === rev || m === ((rev / 10) | 0);
}

/**
 * Writes up to two factor pairs of `product` within [minVal, maxVal] into `out`.
 * Layout: [x1, y1, x2, y2]. Returns number of integers written (0, 2, or 4).
 */
export function collectFactorPairs(
    product: number,
    minVal: number,
    maxVal: number,
    out: Int32Array,
): number {
    const low = Math.max(minVal, Math.ceil(product / maxVal));
    const high = Math.min(maxVal, Math.floor(Math.sqrt(product)));

    let w = 0;
    for (let x = low; x <= high; x++) {
        if (product % x === 0) {
            const y = (product / x) | 0;
            out[w++] = x | 0;
            out[w++] = y;
            if (w === 4) break; // limit to two pairs
        }
    }
    return w;
}

export interface RangeResult {
    product: number;   // 0 if not found
    pairsCount: number; // 0, 2, or 4 (number of ints written to pairsOut)
}

export function smallest(
    minVal: number,
    maxVal: number,
    pairsOut: Int32Array,
    result: RangeResult,
): 0 | 1 {
    let best = 0xFFFFFFFF; // 2**32 - 1

    for (let x = minVal; x <= maxVal; x++) {
        if (x * x >= best) break; // outer prune

        const yUpper = Math.min(maxVal, Math.floor((best - 1) / x));
        if (yUpper < x) continue; // no valid y in this row

        let y = x;
        let prod = x * x;
        while (true) {
            if (isPalindrome(prod)) {
                best = prod;
                break;
            }
            if (y === yUpper) break;
            y++;
            prod += x; // increment by x instead of recomputing x*y
        }
    }

    if (best === 0xFFFFFFFF) {
        result.product = 0;
        result.pairsCount = 0;
        return 0;
    } else {
        result.product = best | 0;
        result.pairsCount = collectFactorPairs(best, minVal, maxVal, pairsOut);
        return 1;
    }
}

export function largest(
    minVal: number,
    maxVal: number,
    pairsOut: Int32Array,
    result: RangeResult,
): 0 | 1 {
    let best = 0;

    for (let x = maxVal; x >= minVal; x--) {
        if (x * maxVal <= best) break; // outer prune

        const yLower = Math.max(x, Math.floor(best / x) + 1);
        if (yLower > maxVal) continue;

        let y = maxVal;
        let prod = x * maxVal;
        while (true) {
            if (isPalindrome(prod)) {
                best = prod;
                break;
            }
            if (y === yLower) break;
            y--;
            prod -= x; // decrement by x instead of recomputing x*y
        }
    }

    if (best === 0) {
        result.product = 0;
        result.pairsCount = 0;
        return 0;
    } else {
        result.product = best | 0;
        result.pairsCount = collectFactorPairs(best, minVal, maxVal, pairsOut);
        return 1;
    }
}

export function doItersSmallest(
    minVal: number,
    maxVal: number,
    iters: number,
    pairsOut: Int32Array,
    res: RangeResult,
    out3: BigInt64Array, // [product, acc, nanos]
): void {
    let acc = 0n;
    let counter = 0n;
    let current = minVal;
    const start = nowNanosBigInt();

    for (let i = 0; i < iters; i++) {
        if (smallest(current, maxVal, pairsOut, res)) {
            // sum factor pairs without allocating
            let s = 0;
            for (let j = 0; j < res.pairsCount; j++) s += pairsOut[j];
            acc += BigInt(res.product + s) + counter;
            counter++;
        }
        current = current >= maxVal ? minVal : (current + 1);
    }

    const end = nowNanosBigInt();
    const nanos = end - start;

    // last product (recompute without allocs)
    smallest(minVal, maxVal, pairsOut, res);

    out3[0] = BigInt(res.product);
    out3[1] = acc;
    out3[2] = nanos;
}

export function doItersLargest(
    minVal: number,
    maxVal: number,
    iters: number,
    pairsOut: Int32Array,
    res: RangeResult,
    out3: BigInt64Array,
): void {
    let acc = 0n;
    let counter = 0n;
    let current = maxVal;

    const t0 = nowNanosBigInt();

    for (let i = 0; i < iters; i++) {
        if (largest(minVal, current, pairsOut, res)) {
            let s = 0;
            for (let j = 0; j < res.pairsCount; j++) s += pairsOut[j];
            acc += BigInt(res.product + s) + counter;
            counter += 1n;
        }
        current = current <= minVal ? maxVal : (current - 1);
    }

    largest(minVal, maxVal, pairsOut, res);

    const t1 = nowNanosBigInt();
    const nanos = t1 - t0;

    out3[0] = BigInt(res.product);
    out3[1] = acc;
    out3[2] = nanos;
}
