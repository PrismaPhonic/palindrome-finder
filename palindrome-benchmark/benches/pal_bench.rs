use criterion::{criterion_group, criterion_main, Criterion};
use std::io::{BufRead, Write};
use std::process::{Command, Stdio};
use std::time::Duration;

const TARGET_CYCLE_MULTIPLIER: u64 = 20_000;
const FIXED_THRESHOLD_MULTIPLIER: u64 = 1_000;

struct Runner {
    child: std::process::Child,
    stdin: std::io::BufWriter<std::process::ChildStdin>,
    stdout: std::io::BufReader<std::process::ChildStdout>,
    cycle_len: u64,
    fixed_iters: u64,
    fixed_threshold: u64,
    force_fixed: bool,
}

impl Runner {
    fn spawn(bin: &str) -> Self {
        let mut child = Command::new(bin)
            .arg("--server")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .expect("spawn failed");
        let stdin = std::io::BufWriter::new(child.stdin.take().unwrap());
        let stdout = std::io::BufReader::new(child.stdout.take().unwrap());
        Self {
            child,
            stdin,
            stdout,
            cycle_len: 0,
            fixed_iters: 0,
            fixed_threshold: 0,
            force_fixed: false,
        }
    }
    fn send(&mut self, line: &str) {
        writeln!(self.stdin, "{line}").unwrap();
        self.stdin.flush().unwrap();
    }
    fn read_line(&mut self) -> String {
        let mut s = String::new();
        self.stdout.read_line(&mut s).unwrap();
        s
    }
    fn init(&mut self, min: i32, max: i32) {
        self.send(&format!("INIT {min} {max}"));
        assert!(self.read_line().starts_with("OK"));
        let min = min as i64;
        let max = max as i64;
        assert!(max >= min, "invalid range");
        let cycle_len = (max - min + 1) as u64;
        assert!(cycle_len > 0);
        self.cycle_len = cycle_len;
        self.fixed_iters = cycle_len * TARGET_CYCLE_MULTIPLIER;
        self.fixed_threshold = cycle_len * FIXED_THRESHOLD_MULTIPLIER;
        self.force_fixed = false;
    }
    fn warmup(&mut self, iters: u64) {
        let actual = self.round_up_to_cycle(iters.max(1));
        self.send(&format!("WARMUP {actual}"));
        assert!(self.read_line().starts_with("OK"));
    }
    fn run(&mut self, requested_iters: u64) -> (i64, u128) {
        let (actual_iters, scaled_iters) = self.adjust_iters(requested_iters.max(1));
        self.send(&format!("RUN {actual_iters}"));
        let line = self.read_line();
        let parts: Vec<_> = line.split_whitespace().collect();
        assert!(parts.len() >= 4 && parts[0] == "OK");
        let product = parts[1].parse::<i64>().unwrap();
        let nanos = parts[3].parse::<u128>().unwrap();
        let scaled = if actual_iters == scaled_iters {
            nanos
        } else {
            let numerator = nanos * (scaled_iters as u128);
            let denominator = actual_iters as u128;
            (numerator + denominator / 2) / denominator
        };
        (product, scaled)
    }
    fn round_up_to_cycle(&self, value: u64) -> u64 {
        let cycle_len = self.cycle_len;
        assert!(cycle_len > 0, "init must be called before run");
        let value = value.max(cycle_len);
        let rem = value % cycle_len;
        if rem == 0 {
            value
        } else {
            value + (cycle_len - rem)
        }
    }
    fn adjust_iters(&mut self, requested: u64) -> (u64, u64) {
        let cycle_len = self.cycle_len;
        assert!(cycle_len > 0, "init must be called before run");
        if !self.force_fixed && requested >= self.fixed_threshold {
            self.force_fixed = true;
        }
        let actual = if self.force_fixed {
            self.fixed_iters
        } else {
            self.round_up_to_cycle(requested)
        };
        (actual, requested)
    }
}

impl Drop for Runner {
    fn drop(&mut self) {
        self.send("QUIT");
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

fn bench_servered(c: &mut Criterion, name: &str, bin: &str, min: i32, max: i32) {
    let mut r = Runner::spawn(bin);
    r.init(min, max);

    // optional short pre-prime (page cache / JITs); not measured
    r.warmup(500_000);

    c.bench_function(name, |b| {
        b.iter_custom(|iters| {
            let (_, nanos) = r.run(iters);
            let seconds = (nanos / 1_000_000_000) as u64;
            let nanos_remainder = (nanos % 1_000_000_000) as u32;
            Duration::new(seconds, nanos_remainder)
        })
    });
}

pub fn benches(c: &mut Criterion) {
    let sbcl_lg = "../target-bin/palprod-fast-largest-inner";
    let sbcl_sm = "../target-bin/palprod-fast-smallest-inner";
    let rust_lg = "../target-bin/palprod-rust-largest";
    let rust_sm = "../target-bin/palprod-rust-smallest";
    let rust_lg_bolt_opt = "../target-bin/palprod-rust-largest-bolt-optimized";
    let rust_sm_bolt_opt = "../target-bin/palprod-rust-smallest-bolt-optimized";
    let rust_fn_lg = "../target-bin/palprod-rust-functional-largest";
    let rust_fn_sm = "../target-bin/palprod-rust-functional-smallest";
    let rust_fn_lg_bolt_opt = "../target-bin/palprod-rust-functional-largest-bolt-optimized";
    let rust_fn_sm_bolt_opt = "../target-bin/palprod-rust-functional-smallest-bolt-optimized";
    let rust_simd_lg = "../target-bin/palprod-rust-simd-largest";
    let rust_simd_sm = "../target-bin/palprod-rust-simd-smallest";
    let rust_simd_bolt_lg = "../target-bin/palprod-rust-simd-largest-bolt-optimized";
    let rust_simd_bolt_sm = "../target-bin/palprod-rust-simd-smallest-bolt-optimized";
    let go_lg = "../target-bin/palprod-go-largest";
    let go_sm = "../target-bin/palprod-go-smallest";
    let go_lg_pgo = "../target-bin/palprod-go-largest-pgo";
    let go_sm_pgo = "../target-bin/palprod-go-smallest-pgo";
    let haskell_lg = "../target-bin/palprod-haskell-largest";
    let haskell_sm = "../target-bin/palprod-haskell-smallest";
    let coalton_lg = "../target-bin/palprod-coalton-largest";
    let coalton_sm = "../target-bin/palprod-coalton-smallest";
    // let python_lg = "../target-bin/palprod-python-largest";
    // let python_sm = "../target-bin/palprod-python-smallest";
    let pypy_lg = "../target-bin/palprod-py-largest";
    let pypy_sm = "../target-bin/palprod-py-smallest";
    let deno_lg = "../target-bin/palprod-deno-largest";
    let deno_sm = "../target-bin/palprod-deno-smallest";
    let bun_lg = "../target-bin/palprod-bun-largest";
    let bun_sm = "../target-bin/palprod-bun-smallest";

    bench_servered(c, "RUST               smallest 2..999", rust_sm, 2, 999);
    bench_servered(c, "RUST               largest 2..999", rust_lg, 2, 999);

    bench_servered(c, "RUST (functional)  largest 2..999", rust_fn_lg, 2, 999);
    bench_servered(c, "RUST (functional)  smallest 2..999", rust_fn_sm, 2, 999);

    bench_servered(c, "RUST (simd)        largest 2..999", rust_simd_lg, 2, 999);
    bench_servered(
        c,
        "RUST (simd)        smallest 2..999",
        rust_simd_sm,
        2,
        999,
    );

    if std::path::Path::new(rust_simd_bolt_lg).exists() {
        bench_servered(
            c,
            "RUST (simd)+BOLT largest 2..999",
            rust_simd_bolt_lg,
            2,
            999,
        );
    }
    if std::path::Path::new(rust_simd_bolt_sm).exists() {
        bench_servered(
            c,
            "RUST (simd)+BOLT smallest 2..999",
            rust_simd_bolt_sm,
            2,
            999,
        );
    }

    if std::path::Path::new(rust_lg_bolt_opt).exists() {
        bench_servered(c, "RUST+BOLT largest 2..999", rust_lg_bolt_opt, 2, 999);
    }
    if std::path::Path::new(rust_sm_bolt_opt).exists() {
        bench_servered(c, "RUST+BOLT smallest 2..999", rust_sm_bolt_opt, 2, 999);
    }
    if std::path::Path::new(rust_fn_lg_bolt_opt).exists() {
        bench_servered(
            c,
            "RUST (functional)+BOLT largest 2..999",
            rust_fn_lg_bolt_opt,
            2,
            999,
        );
    }
    if std::path::Path::new(rust_fn_sm_bolt_opt).exists() {
        bench_servered(
            c,
            "RUST (functional)+BOLT smallest 2..999",
            rust_fn_sm_bolt_opt,
            2,
            999,
        );
    }


    bench_servered(c, "Common Lisp   largest 2..999", sbcl_lg, 2, 999);
    bench_servered(c, "Common Lisp   smallest 2..999", sbcl_sm, 2, 999);

    bench_servered(c, "Haskell largest 2..999", haskell_lg, 2, 999);
    bench_servered(c, "Haskell smallest 2..999", haskell_sm, 2, 999);

    bench_servered(c, "Coalton largest 2..999", coalton_lg, 2, 999);
    bench_servered(c, "Coalton smallest 2..999", coalton_sm, 2, 999);

    bench_servered(c, "GO     largest 2..999", go_lg, 2, 999);
    bench_servered(c, "GO     smallest 2..999", go_sm, 2, 999);
    if std::path::Path::new(go_lg_pgo).exists() {
        bench_servered(c, "GO+PGO largest 2..999", go_lg_pgo, 2, 999);
    }
    if std::path::Path::new(go_sm_pgo).exists() {
        bench_servered(c, "GO+PGO smallest 2..999", go_sm_pgo, 2, 999);
    }

    bench_servered(c, "Deno largest 2..999", deno_lg, 2, 999);
    bench_servered(c, "Deno smallest 2..999", deno_sm, 2, 999);

    bench_servered(c, "Bun largest 2..999", bun_lg, 2, 999);
    bench_servered(c, "Bun smallest 2..999", bun_sm, 2, 999);

    bench_servered(c, "PyPy largest 2..999", pypy_lg, 2, 999);
    bench_servered(c, "PyPy smallest 2..999", pypy_sm, 2, 999);

    // bench_servered(c, "Python largest 2..999", python_lg, 2, 999);
    // bench_servered(c, "Python smallest 2..999", python_sm, 2, 999);
}

criterion_group! {
    name = benches_group;
    config = Criterion::default()
        .sample_size(1000)
        .warm_up_time(Duration::from_millis(500))
        .measurement_time(Duration::from_secs(30));
    targets = benches
}

criterion_main!(benches_group);
