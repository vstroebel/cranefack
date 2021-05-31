/// Cell content inferred during optimization
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Cell {
    /// Cell was accessed but not modified
    Read,

    /// Cell got modified but value is unknown
    Write,

    /// Cell value is known
    Value(u8),
}

impl Cell {
    pub fn is_read(&self) -> bool {
        matches!(self, Cell::Read)
    }

    pub fn is_write(&self) -> bool {
        !self.is_read()
    }
}

/// Hold information about a known modified cell
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct CellAccess {
    /// Offset of the cell relative to the surrounding scope (typically the counter ptr of a local loop)
    pub offset: isize,

    /// The value of th cell
    pub value: Cell,
}

impl CellAccess {
    pub fn new_read(offset: isize) -> CellAccess {
        CellAccess {
            offset,
            value: Cell::Read,
        }
    }

    pub fn new_write(offset: isize) -> CellAccess {
        CellAccess {
            offset,
            value: Cell::Write,
        }
    }

    pub fn new_value(offset: isize, value: u8) -> CellAccess {
        CellAccess {
            offset,
            value: Cell::Value(value),
        }
    }

    pub fn add(cells: &mut Vec<CellAccess>, offset: isize, value: Cell) {
        // Ignore loop counters
        if offset == 0 {
            return;
        }

        for exiting_cell in cells.iter_mut() {
            if exiting_cell.offset == offset {
                if !value.is_read() {
                    exiting_cell.value = value;
                }
                return;
            }
        }

        cells.push(CellAccess {
            offset,
            value,
        });
    }

    pub fn get_value(cells: &[CellAccess], offset: isize) -> Option<Cell> {
        for cell in cells {
            if cell.offset == offset {
                return Some(cell.value);
            }
        }

        None
    }

    pub fn was_written(cells: &[CellAccess], offset: isize) -> bool {
        for cell in cells {
            if cell.offset == offset {
                return cell.value.is_write();
            }
        }

        false
    }

    pub fn was_accessed(cells: &[CellAccess], offset: isize) -> bool {
        for cell in cells {
            if cell.offset == offset {
                return true;
            }
        }

        false
    }
}
