use crate::repl::Repl;

fn test(repl: &mut Repl, input: &str, expected: &str) {
    match repl.handle_input(input) {
        Ok(the) => assert_eq!(the.to_string(), expected),
        Err(e) => panic!("Error: {:?} for input {}", e, input),
    }
}

#[test]
fn tests() {
    let cases: &[(&str, &str)] = &[
        ("(let ((x 1)) x)", "(the int 1)"),
        ("(let ((x 1) (y 2)) x)", "(the int 1)"),
        ("(let ((x 1) (y 2)) y)", "(the int 2)"),
    ];
    for (input, expected) in cases {
        let mut repl = Repl::default();
        test(&mut repl, input, expected);
    }
}
