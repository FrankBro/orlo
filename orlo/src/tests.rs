#[test]
fn tests() {
    use crate::repl::{self, Repl, The};
    // find all the .scm files from the tests subfolder
    // split every 3 lines: input, expected output, empty line
    let files = std::fs::read_dir("src/tests").unwrap();
    for file in files {
        let path = file.unwrap().path();
        if path.extension().and_then(|s| s.to_str()) == Some("scm") {
            let mut repl = Repl::default();
            let mut last_the: Option<Result<The, repl::Error>> = None;
            let mut count = 0;
            for res in repl.handle_file(path.to_str().unwrap()).unwrap() {
                match res {
                    Ok(the) => {
                        if the.ty == "datum" {
                            match last_the.as_ref() {
                                Some(Ok(last)) => {
                                    let the: String = the.value.chars().skip(3).collect();
                                    assert_eq!(
                                        last.to_string(),
                                        the,
                                        "Failed on file: {}",
                                        path.display()
                                    );
                                    count += 1;
                                }
                                Some(Err(last)) => {
                                    let actual = the.value.chars().skip(3).collect::<String>();
                                    let expected = format!("\"{}\"", last);
                                    assert_eq!(
                                        expected,
                                        actual,
                                        "Failed on file: {}",
                                        path.display()
                                    );
                                    count += 1;
                                }
                                None => unreachable!(),
                            }
                        } else {
                            last_the = Some(Ok(the));
                        }
                    }
                    Err(e) => {
                        last_the = Some(Err(e));
                    }
                }
            }
            println!("{}: {} tests passed", path.display(), count);
        }
    }
}
