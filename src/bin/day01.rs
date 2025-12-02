fn main() {
    let input = include_str!("../../inputs/day01.txt");
    println!("Part 1: {}", part1(input));
    println!("Part 2: {}", part2(input));
}

fn part1(input: &str) -> String {
    let sum: i32 = input
        .lines()
        .filter_map(|line| line.parse::<i32>().ok())
        .sum();
    sum.to_string()
}

fn part2(input: &str) -> String {
    let product: i32 = input
        .lines()
        .filter_map(|line| line.parse::<i32>().ok())
        .product();
    product.to_string()
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case("1\n2\n3", "6")]
    #[case("10", "10")]
    #[case("", "0")]
    fn test_part1(#[case] input: &str, #[case] expected: &str) {
        assert_eq!(part1(input), expected);
    }

    #[rstest]
    #[case("2\n3\n4", "24")]
    #[case("5", "5")]
    #[case("", "1")]
    fn test_part2(#[case] input: &str, #[case] expected: &str) {
        assert_eq!(part2(input), expected);
    }
}
