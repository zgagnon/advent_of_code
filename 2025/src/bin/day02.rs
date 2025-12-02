fn main() {
    let input = include_str!("../../inputs/day02.txt");
    println!("Part 1: {}", sum_invalid(input, &part1));
    println!("Part 2: {}", sum_invalid(input, &part2));
}

fn part1((string, len): &(String, usize)) -> bool {
    if len % 2 == 0 {
        let mid = len / 2;
        let front = &string[0..mid];
        let back = &string[mid..*len];
        let result = front == back;
        // println!("Final comparison {} : {} = {}", front, back, result);
        result
    } else {
        false
    }
}
fn part2((string, len): &(String, usize)) -> bool {
    let mut possible_sizes = 1..((len / 2) + 1);
    possible_sizes.any(|size| {
        if len % size == 0 {
            let pointless_intermediary_vec_for_ownership = string.chars().collect::<Vec<_>>();
            let mut chunks = pointless_intermediary_vec_for_ownership.chunks(size); //and it was never referenced again
            let first = chunks
                .next()
                .expect(&format!("Chunks definitely shouldn't be empty {}", string));
            let mut matches = true;
            for chunk in chunks {
                matches = matches && chunk == first;
            }
            println!("{} & {} match? {}", string, size, matches);
            matches
        } else {
            println!("{} & {} != 0", string, size);
            false
        }
    })
}

fn sum_invalid<F>(input: &str, filter: F) -> u64
where
    F: Fn(&(String, usize)) -> bool,
{
    println!("Parsing Ranges from {}", input);
    input
        .trim()
        .split(",")
        .map(|range| {
            let mut items = range.split("-");
            match (items.next(), items.next()) {
                (Some(a), Some(b)) => (a, b),
                _ => panic!("Wrong range structure {}", range),
            }
        })
        .map(|(start, end)| {
            (
                start.parse::<u64>().expect(&format!(
                    "Range start should be a number ({}, {})",
                    start, end,
                )),
                end.parse::<u64>().expect(&format!(
                    "Range end should be a number ({}, {})",
                    start, end,
                )),
            )
        })
        .flat_map(|(start, end)| start..=end)
        .map(|x| {
            let string = x.to_string();
            let len = string.len();
            (string, len)
        })
        .filter(filter)
        .map(|(s, _)| {
            s.parse::<u64>()
                .expect(&format!("Expected our own output to be a number {}", s))
        })
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        let input = include_str!("../../inputs/day02_example.txt");
        assert_eq!(sum_invalid(input, part1), 1227775554);
    }

    #[test]
    fn test_part2() {
        let input = include_str!("../../inputs/day02_example.txt");
        assert_eq!(sum_invalid(input, part2), 4174379265);
    }
}
