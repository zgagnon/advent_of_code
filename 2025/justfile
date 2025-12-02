# Advent of Code 2025 - Rust build/test commands

# Run a specific day's solution
run day:
    cargo run --bin day{{day}}

# Run tests for a specific day
test day:
    cargo test --bin day{{day}}

# Run all tests
test-all:
    cargo test

# Run tests in watch mode
watch day:
    cargo watch -x "test --bin day{{day}}"

# Watch all tests
watch-all:
    cargo watch -x test

# Build all binaries
build:
    cargo build

# Build in release mode
build-release:
    cargo build --release

# Run a day's solution in release mode
run-release day:
    cargo run --release --bin day{{day}}

# Create a new day's solution template
new day:
    #!/usr/bin/env bash
    mkdir -p src/bin
    cat > src/bin/day{{day}}.rs << 'EOF'
    fn main() {
        let input = include_str!("../../inputs/day{{day}}.txt");
        println!("Part 1: {}", part1(input));
        println!("Part 2: {}", part2(input));
    }

    fn part1(input: &str) -> String {
        todo!("Implement part 1")
    }

    fn part2(input: &str) -> String {
        todo!("Implement part 2")
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn test_part1() {
            let input = "";
            assert_eq!(part1(input), "expected");
        }

        #[test]
        fn test_part2() {
            let input = "";
            assert_eq!(part2(input), "expected");
        }
    }
    EOF
    mkdir -p inputs
    touch inputs/day{{day}}.txt
    touch inputs/day{{day}}_example.txt
    echo "Created day{{day}} template with input files"

# Format all Rust files
fmt:
    cargo fmt --all

# Run clippy lints
lint:
    cargo clippy

# Clean build artifacts
clean:
    cargo clean
