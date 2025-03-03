lalrpop_util::lalrpop_mod!(grammar);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn grammar_test() {
        assert!(grammar::TermParser::new().parse("22").is_ok());
        assert!(grammar::TermParser::new().parse("(22)").is_ok());
        assert!(grammar::TermParser::new().parse("((((22))))").is_ok());
        assert!(grammar::TermParser::new().parse("((22)").is_err());
    }
}
