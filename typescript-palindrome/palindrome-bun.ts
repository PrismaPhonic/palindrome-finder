import {
    doItersSmallest,
    doItersLargest,
    RangeResult,
} from "./palindrome-core.ts";

async function runServer(): Promise<void> {
    // one-time reusable buffers (no per-iter allocs)
    const pairsBuf = new Int32Array(4);
    const result: RangeResult = { product: 0, pairsCount: 0 };
    const triple = new BigInt64Array(3); // [product, acc, nanos]

    let min = 2;
    let max = 999;

    const stdin = process.stdin;
    stdin.setEncoding("utf8");

    for await (const chunk of stdin) {
        const lines = chunk.toString().trim().split("\n");

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
                const isSmallest = process.argv.includes("smallest");
                if (isSmallest) {
                    doItersSmallest(min, max, iterations, pairsBuf, result, triple);
                } else {
                    doItersLargest(min, max, iterations, pairsBuf, result, triple);
                }
                console.log("OK");
            } else if (cmd === "RUN") {
                const iterations = parseInt(a, 10);
                const isSmallest = process.argv.includes("smallest");

                if (isSmallest) {
                    doItersSmallest(min, max, iterations, pairsBuf, result, triple);
                } else {
                    doItersLargest(min, max, iterations, pairsBuf, result, triple);
                }

                // triple: [product, accumulator, nanoseconds] — all integers
                console.log(
                    `OK ${triple[0].toString()} ${triple[1].toString()} ${triple[2].toString()}`
                );
            } else if (cmd === "QUIT") {
                process.exit(0);
            }
        }
    }
}

if (require.main === module) {
    const args = process.argv.slice(2);
    if (args.includes("--server")) {
        await runServer();
    } else {
        console.log("Bun palindrome server - use --server flag");
    }
}
