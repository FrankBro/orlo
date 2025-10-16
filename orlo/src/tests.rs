#[test]
fn tests() {
    use crate::repl::{Repl, The};
    // find all the .scm files from the tests subfolder
    // split every 3 lines: input, expected output, empty line
    let files = std::fs::read_dir("src/tests").unwrap();
    for file in files {
        let path = file.unwrap().path();
        if path.extension().and_then(|s| s.to_str()) == Some("scm") {
            let mut repl = Repl::default();
            let mut last_the: Option<The> = None;
            let mut count = 0;
            for the in repl.handle_file(path.to_str().unwrap()).unwrap() {
                if the.ty == "datum" {
                    match last_the.as_ref() {
                        Some(last) => {
                            let the: String = the.value.chars().skip(3).collect();
                            assert_eq!(last.to_string(), the, "Failed on file: {}", path.display());
                            count += 1;
                        }
                        None => unreachable!(),
                    }
                } else {
                    last_the = Some(the);
                }
            }
            println!("{}: {} tests passed", path.display(), count);
        }
    }
}
