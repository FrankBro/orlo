use crate::repl::Repl;

#[test]
fn tests() {
    let cases: &[(&str, &str)] = &[
        ("[]", "(the [~a] [])"),
        ("[1]", "(the [int] [1])"),
        ("[1 2]", "(the [int] [1 2])"),
        // test push
        ("(define arr [])", "(the [~a] [])"),
        ("(push! arr 1)", "(the [int] [1])"),
        ("(. [1] 0)", "(the int 1)"),
        ("(. [1 2 3] -1)", "(the int 3)"),
        ("(. [1 2 3] -2)", "(the int 2)"),
        ("(. [1 2 3] -3)", "(the int 1)"),
    ];
    let mut repl = Repl::default();
    for (input, expected) in cases {
        match repl.handle_input(input) {
            Ok(the) => assert_eq!(&the.to_string(), expected),
            Err(e) => panic!("Error: {:?} for input {}", e, input),
        }
    }
}
