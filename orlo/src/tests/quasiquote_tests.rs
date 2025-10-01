use crate::repl::Repl;

#[test]
fn tests() {
    let cases: &[(&str, &str)] = &[
        // Basic cases
        ("`a", "(the symbol a)"),
        ("`(1 2 3)", "(the (int int int) (1 2 3))"),
        ("(let ((x 10)) `(1 ,x 3))", "(the (int int int) (1 10 3))"),
        // Unquote-splicing edge cases
        ("(let ((lst '(2 3))) `(,@lst))", "(the (int int) (2 3))"), // splice-only
        (
            "(let ((lst '(2 3))) `(,@lst 4))",
            "(the (int int int) (2 3 4))",
        ), // splice at start
        (
            "(let ((lst '(2 3))) `(1 ,@lst))",
            "(the (int int int) (1 2 3))",
        ), // splice at end
        (
            "(let ((lst '(2 3))) `(1 ,@lst 4))",
            "(the (int int int int) (1 2 3 4))",
        ), // splice in middle
        // Multiple splices
        (
            "(let ((a '(1 2)) (b '(3 4))) `(,@a ,@b))",
            "(the (int int int int) (1 2 3 4))",
        ),
        // Nested lists with quasiquote
        (
            "`((1 2) (3 4))",
            "(the ((int int) (int int)) ((1 2) (3 4)))",
        ),
        (
            "(let ((x 5)) `((1 ,x) (3 4)))",
            "(the ((int int) (int int)) ((1 5) (3 4)))",
        ),
        // Quasiquote with empty list
        ("`()", "(the () ())"),
        ("(let ((x '())) `(1 ,@x 2))", "(the (int int) (1 2))"), // splice empty list
        // Mixed unquote and unquote-splicing
        (
            "(let ((x 1) (lst '(2 3))) `(,x ,@lst 4))",
            "(the (int int int int) (1 2 3 4))",
        ),
        // Deeply nested unquotes
        (
            "(let ((x 1)) `(a (b ,x)))",
            "(the (symbol (symbol int)) (a (b 1)))",
        ),
        (
            "(let ((x 1) (y 2)) `(a (b ,x (c ,y))))",
            "(the (symbol (symbol int (symbol int))) (a (b 1 (c 2))))",
        ),
        // Quasiquote with literals
        ("`#t", "(the bool #t)"),
        ("`#f", "(the bool #f)"),
        ("`\"hello\"", "(the string \"hello\")"),
        ("`42", "(the int 42)"),
        // Unquote with expression evaluation
        (
            "(let ((x 5)) `(1 ,(+ x 2) 3))",
            "(the (int int int) (1 7 3))",
        ),
        (
            "(let ((lst '(1 2))) `(a ,(car lst)))",
            "(the (symbol int) (a 1))",
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
