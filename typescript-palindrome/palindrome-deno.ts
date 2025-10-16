#!/usr/bin/env deno run --allow-read

import {
    doItersSmallest,
    doItersLargest,
    RangeResult,
} from "./palindrome-core.ts";

async function runServer(): Promise<void> {
    const pairsBuf = new Int32Array(4);
    const result: RangeResult = { product: 0, pairsCount: 0 };
    const triple = new BigInt64Array(3); // [product, acc, nanos]

    let min = 2;
    let max = 999;

    const decoder = new TextDecoder();
    const buf = new Uint8Array(8192);

    while (true) {
        const n = await Deno.stdin.read(buf);
        if (n === null) break;

        const text = decoder.decode(buf.subarray(0, n));
        const lines = text.trim().split("\n");

        for (const raw of lines) {
            const line = raw.trim();
            if (!line) continue;

            const [cmd, a, b] = line.split(" ");

            if (cmd === "INIT") {
                min = parseInt(a, 10);
                max = parseInt(b, 10);
                console.log("OK");
            } else if (cmd === "WARMUP") {
                const iterations = parseInt(a, 10);
                const isSmallest = Deno.args.includes("smallest");
                if (isSmallest) {
                    doItersSmallest(min, max, iterations, pairsBuf, result, triple);
                } else {
                    doItersLargest(min, max, iterations, pairsBuf, result, triple);
                }
                console.log("OK");
            } else if (cmd === "RUN") {
                const iterations = parseInt(a, 10);
                const isSmallest = Deno.args.includes("smallest");

                if (isSmallest) {
                    doItersSmallest(min, max, iterations, pairsBuf, result, triple);
                } else {
                    doItersLargest(min, max, iterations, pairsBuf, result, triple);
                }

                console.log(
                    `OK ${triple[0].toString()} ${triple[1].toString()} ${triple[2].toString()}`
                );
            } else if (cmd === "QUIT") {
                Deno.exit(0);
            }
        }
    }
}

if (import.meta.main) {
    const args = Deno.args;
    if (args.includes("--server")) {
        await runServer();
    } else {
        console.log("Deno palindrome server - use --server flag");
    }
}
