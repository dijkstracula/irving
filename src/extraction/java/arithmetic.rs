use std::fmt;

use super::types::JavaType;

/// Routines for saturating arithmetic.

pub fn write_bounded_long(f: &mut fmt::Formatter<'_>, i: i64, lo: i64, hi: i64) -> fmt::Result {
    f.write_fmt(format_args!("({i} >= {hi} ? {hi} : "))?;

    f.write_str("(")?;
    f.write_fmt(format_args!("({i} < {lo} ? {lo} : i)"))?;
    f.write_str(")")
}

pub fn write_numeric_as(f: &mut fmt::Formatter<'_>, i: i64, jtype: &JavaType) -> fmt::Result {
    match jtype {
        // TODO: Bool?  Char?  I don't think we care about these, but do we?
        // There's a world where this is a more general purpose serialization
        // mechanism for e.g. the mocked network, but that's something to think
        // about for later...
        JavaType::Long => f.write_fmt(format_args!("{i}")),
        JavaType::BoundedLong(lo, hi) => write_bounded_long(f, i, *lo, *hi),
        _ => panic!("Not a numeric JType"), // TODO: fmt::Result::Error()?
    }
}
