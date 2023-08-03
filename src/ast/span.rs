use std::rc::Rc;

use crate::parser::ivy::Node;

/// A span derived from a location in a source file, which we can
/// use to generate an annotated snippet from.
#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord)]
pub struct SourceSpan {
    input: Rc<str>,

    start: usize,

    end: usize,
}

#[derive(Debug, Clone, Eq, PartialOrd, Ord)]
pub enum Span {
    /// From an actual program text
    Source(SourceSpan),

    /// Optimized as part of a compiler pass,
    Optimized,

    /// Generated as part of a unit test
    SynthesizedForTesting,
}

impl PartialEq for Span {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Source(l0), Self::Source(r0)) => l0 == r0,

            // Because tests::helpers emits synthesized spans, they will never
            // actually match what the parser actually generates, and it would be
            // tedious to compute those spans manually when writing unit tests.
            // Simplify the majority of the tests by ensuring that a Synthesized
            // matches anything.  Slightly unfortunate in that we won't catch
            // incorrect spans in most tests, but the alternative is far more
            // painful.
            (Self::SynthesizedForTesting, _) => true,
            (_, Self::SynthesizedForTesting) => true,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl Span {
    pub fn from_pest(input: Rc<str>, span: &pest::Span) -> Self {
        Self::Source(SourceSpan {
            input,
            start: span.start(),
            end: span.end(),
        })
    }

    pub fn from_node(input: &Node) -> Self {
        Self::from_pest(Rc::clone(&input.user_data()), &input.as_span())
    }

    pub fn merge(s1: &Span, s2: &Span) -> Self {
        match (s1, s2) {
            (Span::Source(s1), Span::Source(s2)) => {
                if s1.input.len() != s2.input.len() {
                    // Unsound but fast
                    panic!("Trying to merge two different spans: {:?} {:?}", s1, s2);
                }
                let start = s1.start.min(s2.start);
                let end = s1.end.max(s2.end);
                Self::Source(SourceSpan {
                    input: s1.input.clone(),
                    start,
                    end,
                })
            }

            // If one of them was syntheized for testing, we can't say anything about
            // the merged span.
            (Span::SynthesizedForTesting, _) | (_, Span::SynthesizedForTesting) => {
                Span::SynthesizedForTesting
            }
            _ => todo!(),
        }
    }
}
