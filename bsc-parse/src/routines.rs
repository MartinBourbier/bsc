pub fn apply_string_escapes(input: &str) -> String {
    let mut chars = input.chars().peekable();
    let mut escaped = String::new();

    while let Some(c) = chars.next() {
        if c == '\\' {
            if let Some(&next) = chars.peek() {
                let replacement = match next {
                    'n' => '\n',
                    't' => '\t',
                    _ => next,
                };
                escaped.push(replacement);
                chars.next();
            }
        } else {
            escaped.push(c);
        }
    }

    escaped
}
