use crate::repl::Repl;

#[test]
fn tests() {
    let cases: &[(&str, &str)] = &[
        ("(let ((x 1)) x)", "(the int 1)"),
        ("(let ((x 1) (y 2)) x)", "(the int 1)"),
        ("(let ((x 1) (y 2)) y)", "(the int 2)"),
        (
            "(let loop ((i 0)) (if (> i 5) i (loop (+ i 1))))",
            "(the int 6)",
        ),
    ];
    let mut repl = Repl::default();
    for (input, expected) in cases {
        match repl.handle_input(input) {
            Ok(the) => assert_eq!(&the.to_string(), expected),
            Err(e) => panic!("Error: {:?} for input {}", e, input),
        }
    }
}
