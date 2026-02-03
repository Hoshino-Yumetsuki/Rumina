#!/bin/bash
# Simple benchmark to show JIT performance improvements

echo "Building release version..."
cargo build --release --bin rumina-cli 2>&1 | grep -E "Finished|Compiling rumina"

echo ""
echo "=== JIT Performance Benchmark ==="
echo ""

# Create a test program
cat > /tmp/bench_loop.lm << 'EOF'
var sum = 0;
var i = 0;
while (i < 10000) {
    sum = sum + i;
    i = i + 1;
}
print(sum);
EOF

echo "Test program (10000 iteration loop with arithmetic):"
cat /tmp/bench_loop.lm
echo ""

echo "Running benchmark (this may take a moment)..."
echo ""

# Run the program and measure execution time
echo "Execution with JIT optimizations:"
time ./target/release/rumina-cli /tmp/bench_loop.lm

echo ""
echo "Note: JIT compiles hot paths after 100 executions, providing speedup for loops and repeated operations."
echo ""

# Cleanup
rm -f /tmp/bench_loop.lm
