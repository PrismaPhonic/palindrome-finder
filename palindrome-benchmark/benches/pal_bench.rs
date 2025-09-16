use criterion::{Criterion, criterion_group, criterion_main};
use std::io::{BufRead, Write};
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

struct Runner {
    child: std::process::Child,
    stdin: std::io::BufWriter<std::process::ChildStdin>,
    stdout: std::io::BufReader<std::process::ChildStdout>,
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
    }
    fn warmup(&mut self, iters: u64) {
        self.send(&format!("WARMUP {iters}"));
        assert!(self.read_line().starts_with("OK"));
    }
    fn run(&mut self, iters: u64) -> i64 {
        self.send(&format!("RUN {iters}"));
        let line = self.read_line();
        let parts: Vec<_> = line.split_whitespace().collect();
        assert!(parts.len() >= 2 && parts[0] == "OK");
        parts[1].parse::<i64>().unwrap()
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
            let start = Instant::now();
            let _ = r.run(iters);
            start.elapsed()
        })
    });
}

pub fn benches(c: &mut Criterion) {
    let fast_lg = "../target-bin/palprod-fast-largest-inner";
    let fast_sm = "../target-bin/palprod-fast-smallest-inner";
    let comm_lg = "../target-bin/palprod-comm-largest-inner";
    let comm_sm = "../target-bin/palprod-comm-smallest-inner";
    let rust_lg = "../target-bin/palprod-rust-largest";
    let rust_sm = "../target-bin/palprod-rust-smallest";
    let go_lg = "../target-bin/palprod-go-largest";
    let go_sm = "../target-bin/palprod-go-smallest";

    bench_servered(c, "FAST   largest 100..999", fast_lg, 100, 999);
    bench_servered(c, "FAST   smallest 910..999", fast_sm, 910, 999);

    bench_servered(c, "COMM   largest 100..999", comm_lg, 100, 999);
    bench_servered(c, "COMM   smallest 910..999", comm_sm, 910, 999);

    bench_servered(c, "RUST   largest 100..999", rust_lg, 100, 999);
    bench_servered(c, "RUST   smallest 910..999", rust_sm, 910, 999);

    bench_servered(c, "GO     largest 100..999", go_lg, 100, 999);
    bench_servered(c, "GO     smallest 910..999", go_sm, 910, 999);
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
