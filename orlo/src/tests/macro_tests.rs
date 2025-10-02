use crate::repl::Repl;

#[test]
fn tests() {
    let cases: &[(&str, &str)] = &[
        // Simple macro definition and usage
        (
            "(define-macro (unless condition then else) `(if ,condition ,else ,then))",
            "(the symbol unless)",
        ),
        ("(unless #f 1 2)", "(the int 1)"),
        ("(unless #t 1 2)", "(the int 2)"),
        // Macro with multiple uses
        (
            "(define-macro (when condition then else) `(if ,condition ,then ,else))",
            "(the symbol when)",
        ),
        ("(when #t 100 200)", "(the int 100)"),
        ("(when #f 100 200)", "(the int 200)"),
        // Macro that generates arithmetic
        (
            "(define-macro (square x) `(* ,x ,x))",
            "(the symbol square)",
        ),
        ("(square 5)", "(the int 25)"),
        ("(square (+ 2 3))", "(the int 25)"),
        // Macro with list manipulation
        (
            "(define-macro (first lst) `(car ,lst))",
            "(the symbol first)",
        ),
        ("(first '(1 2 3))", "(the int 1)"),
        // Nested macro expansion
        ("(square (square 2))", "(the int 16)"),
        // Macro with varargs (dotted list)
        (
            "(define-macro (my-when condition . body) `(if ,condition (begin ,@body) 0))",
            "(the symbol my-when)",
        ),
        ("(my-when #t (+ 1 2))", "(the int 3)"),
        ("(my-when #t (+ 1 2) (* 3 4))", "(the int 12)"),
        ("(my-when #f (+ 1 2))", "(the int 0)"),
    ];

    let mut repl = Repl::default();
    for (input, expected) in cases {
        match repl.handle_input(input) {
            Ok(the) => assert_eq!(&the.to_string(), expected, "Input: {}", input),
            Err(e) => panic!("Error: {:?} for input {}", e, input),
        }
    }
}
