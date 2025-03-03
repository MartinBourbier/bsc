mod routines;

lalrpop_util::lalrpop_mod!(grammar);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_string() {
        assert_eq!(
            grammar::StringLitParser::new().parse(r#""this is a string!""#),
            Ok(String::from("this is a string!"))
        );
    }

    #[test]
    fn intereting_char_string() {
        for (input, expected) in [
            (r#""this is a \t string""#, "this is a \t string"),
            (r#""this is a \n string""#, "this is a \n string"),
            (r#""this is \\ a string!""#, "this is \\ a string!"),
        ] {
            assert_eq!(
                grammar::StringLitParser::new().parse(input),
                Ok(String::from(expected))
            );
        }
    }

    #[test]
    fn primary_expression() {
        assert!(grammar::PrimaryExpressionParser::new().parse("(1)").is_ok());
        assert!(grammar::PrimaryExpressionParser::new().parse("12").is_ok());
        assert!(
            grammar::PrimaryExpressionParser::new()
                .parse(r#""this is a string!""#)
                .is_ok()
        );
        assert!(
            grammar::PrimaryExpressionParser::new()
                .parse(r#"variableName"#)
                .is_ok()
        );
        assert!(
            grammar::PrimaryExpressionParser::new()
                .parse(r#"variable_name"#)
                .is_ok()
        );
    }
}
