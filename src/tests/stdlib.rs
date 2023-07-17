#[cfg(test)]
mod tests {
    use crate::stdlib::load_stdlib;

    #[test]
    fn test_stdlib_typechecking() {
        let tc = load_stdlib().unwrap();
    }
}
