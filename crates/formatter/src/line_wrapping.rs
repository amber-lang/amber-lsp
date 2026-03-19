use crate::{DESIRED_WIDTH, NEWLINE};
use std::num::{NonZero, NonZeroU8};

pub fn handle_wrapping(current_line: String, mut wraps: Vec<WrapPoint>) -> String {
    if DESIRED_WIDTH >= current_line.len() {
        return current_line;
    }

    wraps.sort_by(|a, b| {
        use std::cmp::Ordering;

        if a.wrap.weight > b.wrap.weight {
            return Ordering::Less;
        }
        if b.wrap.weight > a.wrap.weight {
            return Ordering::Greater;
        }

        if a.location > b.location {
            return Ordering::Less;
        }

        if b.location > a.location {
            return Ordering::Greater;
        }

        if matches!(a.wrap.wrap_type, WrapType::WrapWith) {
            return Ordering::Less;
        }

        Ordering::Equal
    });

    let mut wrapped_line = WrappedLine::new(current_line, DESIRED_WIDTH);

    let mut group = false;
    for point in wraps {
        if group {
            group = point.wrap.wrap_type == WrapType::WrapWith;
            wrapped_line.wrap(point);
            continue;
        }

        group = point.wrap.wrap_type == WrapType::WrapWith;

        if wrapped_line.is_wrapped() {
            break;
        }

        wrapped_line.wrap(point);
    }

    wrapped_line.apply_wraps()
}

#[derive(Debug, Default)]
pub struct Wrap {
    weight: Option<NonZeroU8>,
    wrap_type: WrapType,
}

impl Wrap {
    const W_NEVER: u8 = 0;
    const W_LAST: u8 = 10;
    const W_LOW_MIDDLE: u8 = 90;
    const W_MIDDLE: u8 = 120;
    const W_HIGH_MIDDLE: u8 = 150;
    const W_FIRST: u8 = 255;

    pub const NEVER: Wrap = Wrap::new(Self::W_NEVER);
    pub const LAST: Wrap = Wrap::new(Self::W_LAST);
    pub const LOW_MIDDLE: Wrap = Wrap::new(Self::W_LOW_MIDDLE);
    pub const MIDDLE: Wrap = Wrap::new(Self::W_MIDDLE);
    pub const HIGH_MIDDLE: Wrap = Wrap::new(Self::W_HIGH_MIDDLE);
    pub const FIRST: Wrap = Wrap::new(Self::W_FIRST);

    pub const WITH_NEVER: Wrap = Wrap::new_wrap_with(Self::W_NEVER);
    pub const WITH_LAST: Wrap = Wrap::new_wrap_with(Self::W_LAST);
    pub const WITH_LOW_MIDDLE: Wrap = Wrap::new_wrap_with(Self::W_LOW_MIDDLE);
    pub const WITH_MIDDLE: Wrap = Wrap::new_wrap_with(Self::W_MIDDLE);
    pub const WITH_HIGH_MIDDLE: Wrap = Wrap::new_wrap_with(Self::W_HIGH_MIDDLE);
    pub const WITH_FIRST: Wrap = Wrap::new_wrap_with(Self::W_FIRST);

    pub const fn new(weight: u8) -> Self {
        Self {
            weight: NonZero::new(weight),
            wrap_type: WrapType::WrapAlone,
        }
    }

    /// If another "wrap with" is wrapped with the same weight this space will also be wrapped.
    pub const fn new_wrap_with(weight: u8) -> Self {
        Self {
            weight: NonZero::new(weight),
            wrap_type: WrapType::WrapWith,
        }
    }

    pub fn could_wrap(&self) -> bool {
        self.weight.is_some()
    }
}

#[derive(Debug, Default, PartialEq)]
pub enum WrapType {
    WrapWith,
    #[default]
    WrapAlone,
}

pub struct WrapPoint {
    style: WrapStyle,
    wrap: Wrap,
    location: usize,
    indentation: String,
}

impl WrapPoint {
    pub fn new(style: WrapStyle, wrap: Wrap, location: usize, indentation: String) -> Self {
        Self {
            style,
            wrap,
            location,
            indentation,
        }
    }
}

pub enum WrapStyle {
    /// Replace the character with a newline.
    Character,
    /// Insert a newline between this character and the one after.
    Between,
}

struct WrappedLine {
    current_line: String,
    wrap_points: Vec<WrapPoint>,
    longest_line: usize,
    desired_width: usize,
}

impl WrappedLine {
    fn new(current_line: String, desired_width: usize) -> Self {
        let longest_line = current_line.len();
        Self {
            current_line,
            wrap_points: Vec::new(),
            longest_line,
            desired_width,
        }
    }

    fn wrap(&mut self, wrap: WrapPoint) {
        self.wrap_points.push(wrap);
        self.wrap_points.sort_by(|a, b| a.location.cmp(&b.location));
        self.longest_line = self.wrap_points.windows(2).fold(0, |acc, elements| {
            elements
                .get(0)
                .and_then(|first| {
                    elements.get(1).map(|second| {
                        second
                            .location
                            .saturating_sub(first.location + first.indentation.len())
                    })
                })
                .unwrap_or(acc)
        });
    }

    fn is_wrapped(&self) -> bool {
        self.longest_line <= self.desired_width
    }

    fn apply_wraps(mut self) -> String {
        for wrap in self.wrap_points.into_iter().rev() {
            match wrap.style {
                WrapStyle::Character => self.current_line.replace_range(
                    wrap.location - 1..wrap.location,
                    &format!("{NEWLINE}{}", wrap.indentation),
                ),
                WrapStyle::Between => self
                    .current_line
                    .insert_str(wrap.location, &format!("{NEWLINE}{}", wrap.location)),
            }
        }
        self.current_line
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn short_no_wrap() {}
}
