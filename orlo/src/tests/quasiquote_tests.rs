use crate::repl::Repl;

#[test]
fn tests() {
    let cases: &[(&str, &str)] = &[
        ("`a", "(the symbol a)"),
        ("`(1 2 3)", "(the (int int int) (1 2 3))"),
        ("(let ((x 10)) `(1 ,x 3))", ("(the (int int int) (1 10 3))")),
    ];
    let mut repl = Repl::default();
    for (input, expected) in cases {
        match repl.handle_input(input) {
            Ok(the) => assert_eq!(&the.to_string(), expected),
            Err(e) => panic!("Error: {:?} for input {}", e, input),
        }
    }
}
