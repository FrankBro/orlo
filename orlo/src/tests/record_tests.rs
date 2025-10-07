use crate::repl::Repl;

#[test]
fn tests() {
    let cases: &[(&str, &str)] = &[
        ("{}", "(the {} {})"),
        ("{.a 1}", "(the {.a int} {.a 1})"),
        (
            "{.a 1 .b \"2\"}",
            "(the {.a int .b string} {.a 1 .b \"2\"})",
        ),
    ];
    let mut repl = Repl::default();
    for (input, expected) in cases {
        match repl.handle_input(input) {
            Ok(the) => assert_eq!(&the.to_string(), expected, "for input {}", input),
            Err(e) => panic!("Error: {:?} for input {}", e, input),
        }
    }
}
