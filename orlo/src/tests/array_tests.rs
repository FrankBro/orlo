use crate::repl::Repl;

#[test]
fn tests() {
    let cases: &[(&str, &str)] = &[
        ("[]", "(the [a] [])"),
        ("[1]", "(the [int] [1])"),
        ("[1 2]", "(the [int] [1 2])"),
    ];
    let mut repl = Repl::default();
    for (input, expected) in cases {
        match repl.handle_input(input) {
            Ok(the) => assert_eq!(&the.to_string(), expected),
            Err(e) => panic!("Error: {:?} for input {}", e, input),
        }
    }
}
