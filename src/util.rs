pub fn intersperse<T: ToString>(xs: &[T]) -> String {
    let mut output = String::new();
    let mut iter = xs.iter();
    if let Some(head) = iter.next() {
        output.push_str(&head.to_string());
    }
    for value in iter {
        output.push(' ');
        output.push_str(&value.to_string());
    }
    output
}
