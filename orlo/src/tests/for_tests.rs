use crate::repl::Repl;

#[test]
fn tests() {
    let cases: &[(&str, &str)] = &[
        ("(define res 0)", "(the int 0)"),
        ("(for ((x [1 2 3])) (set! res (+ res x)))", "(the void ())"),
        ("res", "(the int 6)"),
        //
        ("(define arr [])", "(the [~a] [])"),
        ("(for ((x [1 2])) (push! arr x))", "(the void ())"),
        ("arr", "(the [int] [1 2])"),
        //
        ("(define arr [])", "(the [~a] [])"),
        (
            "(for ((x [2 1]) (y [1 0])) (push! arr (- x y)))",
            "(the void ())",
        ),
        ("arr", "(the [int] [1 2 0 1])"),
    ];
    let mut repl = Repl::default();
    for (input, expected) in cases {
        match repl.handle_input(input) {
            Ok(the) => assert_eq!(&the.to_string(), expected, "for input {}", input),
            Err(e) => panic!("Error: {:?} for input {}", e, input),
        }
    }
}
