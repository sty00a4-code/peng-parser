use std::{ops::Range, fmt::{Debug, Display}};

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Position {
    pub ln: Range<usize>,
    pub col: Range<usize>,
}
impl Position {
    pub fn new(ln: Range<usize>, col: Range<usize>) -> Self {
        Self { ln, col }
    }
    pub fn extend(&mut self, pos: &Self) {
        self.ln.end = pos.ln.end;
        self.col.end = pos.col.end;
    }
    pub fn ln(self) -> Range<usize> {
        self.ln
    }
    pub fn col(self) -> Range<usize> {
        self.col
    }
    pub fn ln_ref(&self) -> &Range<usize> {
        &self.ln
    }
    pub fn col_ref(&self) -> &Range<usize> {
        &self.col
    }
    pub fn ln_mut(&mut self) -> &mut Range<usize> {
        &mut self.ln
    }
    pub fn col_mut(&mut self) -> &mut Range<usize> {
        &mut self.col
    }
}

pub struct Located<T> {
    pub item: T,
    pub pos: Position
}
impl<T> Located<T> {
    pub fn new(item: T, pos: Position) -> Self {
        Self { item, pos }
    }
    pub fn unwrap(self) -> T {
        self.item
    }
    pub fn unwrap_pos(self) -> Position {
        self.pos
    }
}
impl<T: Debug> Debug for Located<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.item)
    }
}
impl<T: Display> Display for Located<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.item)
    }
}
impl<T: Clone> Clone for Located<T> {
    fn clone(&self) -> Self {
        Self::new(self.item.clone(), self.pos.clone())
    }
}
impl<T: PartialEq> PartialEq for Located<T> {
    fn eq(&self, other: &Self) -> bool {
        self.item == other.item && self.pos == other.pos
    }
}
impl<T: Default> Default for Located<T> {
    fn default() -> Self {
        Self::new(T::default(), Position::default())
    }
}

#[macro_export]
macro_rules! locate {
    ($item:expr, $pos:expr) => {
        self::Located::new($item, $pos)
    };
    ($item:expr, $ln:expr, $col:expr) => {
        self::Located::new($item, self::Position::new($ln, $col))
    };
    ($item:expr, $ln_start:expr, $ln_end:expr, $col_start:expr, $col_end:expr) => {
        self::Located::new($item, self::Position::new($ln_start..$ln_end, $col_start..$col_end))
    };
}