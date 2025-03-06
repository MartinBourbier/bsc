lalrpop_util::lalrpop_mod!(grammar);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn grammar_id_simple_test() {
        let sym = grammar::IdParser::new().parse("a");

        assert!(sym.is_ok());
        assert_eq!(sym.unwrap().get_name(), "a");
    }

    #[test]
    fn grammar_id_underscore_prefix_test() {
        let sym = grammar::IdParser::new().parse("_a");

        assert!(sym.is_ok());
        assert_eq!(sym.unwrap().get_name(), "_a");
    }

    #[test]
    fn grammar_id_wrong_prefix_test() {
        let sym = grammar::IdParser::new().parse("1a");

        assert!(sym.is_err());
    }

    #[test]
    fn grammar_id_alphanumeric_test() {
        let sym = grammar::IdParser::new().parse("a1");

        assert!(sym.is_ok());
        assert_eq!(sym.unwrap().get_name(), "a1");
    }

    #[test]
    fn grammar_id_alphanumeric_underscore_test() {
        let sym = grammar::IdParser::new().parse("a_1");

        assert!(sym.is_ok());
        assert_eq!(sym.unwrap().get_name(), "a_1");
    }
}
