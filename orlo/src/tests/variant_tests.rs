use crate::repl::Repl;

#[test]
fn tests() {
    let cases: &[(&str, &str)] = &[
        ("(.a 1)", "(the (.a int . ra) (.a 1))"),
        ("(match (.a 1) ((.a a) a))", "(the int 1)"),
    ];
    let mut repl = Repl::default();
    for (input, expected) in cases {
        match repl.handle_input(input) {
            Ok(the) => assert_eq!(&the.to_string(), expected, "for input {}", input),
            Err(e) => panic!("Error: {:?} for input {}", e, input),
        }
    }
}
