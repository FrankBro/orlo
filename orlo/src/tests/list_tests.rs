use crate::repl::Repl;

#[test]
fn tests() {
    let cases: &[(&str, &str)] = &[("(list 1 #t \"a\")", "(the (int bool string) (1 #t \"a\"))")];
    let mut repl = Repl::default();
    for (input, expected) in cases {
        match repl.handle_input(input) {
            Ok(the) => assert_eq!(&the.to_string(), expected),
            Err(e) => panic!("Error: {:?} for input {}", e, input),
        }
    }
}
