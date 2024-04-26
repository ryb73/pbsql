#[cfg(test)]
pub fn split_by_line_and_trim_spaces(s: &str) -> Vec<String> {
    let mut lines = s.split('\n').collect::<Vec<_>>();
    if lines.first() == Some(&"") {
        lines.remove(0);
    }
    if lines.last().map(|s| s.trim()) == Some(&"") {
        lines.pop();
    }

    let baseline_preceding_space_count =
        lines
            .iter()
            .filter(|s| !s.trim().is_empty())
            .fold(50, |acc, s| {
                let current_space_count = s.chars().take_while(|c| c.is_whitespace()).count();

                if current_space_count < acc {
                    current_space_count
                } else {
                    acc
                }
            });

    lines
        .iter()
        .map(|s| {
            s.strip_prefix(&' '.to_string().repeat(baseline_preceding_space_count))
                .unwrap_or(s)
                .to_string()
        })
        .collect()
}
