use crate::repl::Repl;

#[test]
fn tests() {
    let cases: &[(&str, &str)] = &[
        ("(define i 0)", "(the int 0)"),
        ("(define arr [])", "(the [~a] [])"),
        (
            "(while (< i 5) (push! arr i) (set! i (+ i 1)))",
            "(the void ())",
        ),
        ("arr", "(the [int] [0 1 2 3 4])"),
    ];
    let mut repl = Repl::default();
    for (input, expected) in cases {
        match repl.handle_input(input) {
            Ok(the) => assert_eq!(&the.to_string(), expected, "for input {}", input),
            Err(e) => panic!("Error: {:?} for input {}", e, input),
        }
    }
}
