use std::sync::atomic::{AtomicUsize, Ordering};


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Symbol {
    name: String,
}

static COUNTER: AtomicUsize = AtomicUsize::new(0);

#[allow(dead_code)]
impl Symbol {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
        }
    }

    pub fn get_name(&self) -> &str {
        &self.name }

    pub fn fresh() -> Self {
        Self::fresh_prefixed("gensym")
    }

    pub fn fresh_prefixed(prefix: &str) -> Self {
        let id = COUNTER.fetch_add(1, Ordering::SeqCst);
        Self {
            name: format!("{}_{}", prefix, id),
        }
    }

    // Private helper function to reset the counter before and after each test
    fn reset_counter() {
        COUNTER.store(0, Ordering::SeqCst);
    }
}

impl Default for Symbol {
    fn default() -> Self {
        Self::fresh()
    }
}

impl Into<String> for Symbol {
    fn into(self) -> String {
        self.name
    }
}

#[cfg(test)]
// Required to avoid tests being run in parallel which messes up the counter
#[serial_test::serial]
mod tests {
    use super::*;

    #[test]
    fn symbol_construct_test() {
        Symbol::reset_counter();

        let s = Symbol::new("test");
        assert_eq!(s.name, "test");

        Symbol::reset_counter();
    }

    #[test]
    fn symbol_fresh_test() {
        Symbol::reset_counter();

        let s1 = Symbol::fresh();
        let s2 = Symbol::fresh();
        assert_ne!(s1.name, s2.name);

        assert_eq!(s1.name, "gensym_0");
        assert_eq!(s2.name, "gensym_1");

        Symbol::reset_counter();
    }

    #[test]
    fn symbol_fresh_prefixed_test() {
        Symbol::reset_counter();

        let s1 = Symbol::fresh_prefixed("foo");
        let s2 = Symbol::fresh_prefixed("bar");
        assert_ne!(s1.name, s2.name);

        assert_eq!(s1.name, "foo_0");
        assert_eq!(s2.name, "bar_1");

        Symbol::reset_counter();
    }

    #[test]
    fn symbol_get_name_test() {
        Symbol::reset_counter();

        let s = Symbol::new("test_again");
        assert_eq!(s.get_name(), "test_again");

        Symbol::reset_counter();
    }

    #[test]
    fn symbol_eq_test() {
        Symbol::reset_counter();

        let s1 = Symbol::new("test");
        let s2 = Symbol::new("test");
        let s3 = Symbol::new("test_again");

        assert_eq!(s1, s2);
        assert_ne!(s1, s3);

        Symbol::reset_counter();
    }

    #[test]
    fn symbol_default_test() {
        Symbol::reset_counter();

        let s = Symbol::default();
        assert_eq!(s.name, "gensym_0");

        Symbol::reset_counter();
    }

    #[test]
    fn symbol_eq_default_test() {
        Symbol::reset_counter();

        let s1 = Symbol::default();
        let s2 = Symbol::default();
        let s3 = Symbol::new("gensym_0");

        assert_ne!(s1, s2);
        assert_eq!(s1, s3);

        Symbol::reset_counter();
    }

    #[test]
    fn symbol_default_many_test() {
        Symbol::reset_counter();

        let s1 = Symbol::default();
        let s2 = Symbol::default();
        let s3 = Symbol::default();
        let s4 = Symbol::default();

        assert_eq!(s1.name, "gensym_0");
        assert_eq!(s2.name, "gensym_1");
        assert_eq!(s3.name, "gensym_2");
        assert_eq!(s4.name, "gensym_3");

        Symbol::reset_counter();
    }
}
