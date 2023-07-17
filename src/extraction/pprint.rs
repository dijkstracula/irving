use std::fmt::{Result, Write};

pub struct PrettyPrinter<W>
where
    W: Write,
{
    pub out: W,
    indent: usize,
    curr_line_is_indented: bool,
}

impl PrettyPrinter<String> {
    #[allow(dead_code)]
    pub fn new() -> Self {
        Self {
            out: String::new(),
            indent: 0,
            curr_line_is_indented: false,
        }
    }
}

impl<W: Write> Write for PrettyPrinter<W> {
    fn write_str(&mut self, s: &str) -> Result {
        // TODO: can I do this without allocation?
        let lines = s.split("\n").enumerate().collect::<Vec<_>>();

        for (i, line) in &lines {
            self.indent -= line.matches("}").count();
            if line.len() > 0 && !self.curr_line_is_indented {
                let indent = std::iter::repeat(" ")
                    .take(4 * self.indent)
                    .collect::<String>();
                self.out.write_str(&indent)?;
                self.curr_line_is_indented = true;
            }
            self.out.write_str(line)?;

            self.indent += line.matches("{").count();
            if *i < lines.len() - 1 {
                self.out.write_str("\n")?;
                self.curr_line_is_indented = false;
            }
        }
        Ok(())
    }
}
