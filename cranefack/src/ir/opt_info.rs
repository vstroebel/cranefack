#[derive(Debug, Clone, Eq, PartialEq)]
pub struct BlockInfo {
    cell_access: Option<Vec<CellAccess>>,
    allways_used: bool,
}

impl BlockInfo {
    pub fn new_empty() -> BlockInfo {
        BlockInfo {
            cell_access: None,
            allways_used: false,
        }
    }

    pub fn new_access(cell_access: Vec<CellAccess>) -> BlockInfo {
        BlockInfo {
            cell_access: Some(cell_access),
            allways_used: false,
        }
    }

    pub fn has_cell_access(&self) -> bool {
        self.cell_access.is_some()
    }

    pub fn cell_access(&self) -> Option<&Vec<CellAccess>> {
        self.cell_access.as_ref()
    }

    pub fn set_cell_access(&mut self, cell_access: Vec<CellAccess>) {
        self.cell_access = Some(cell_access);
    }

    pub fn always_used(&self) -> bool {
        self.allways_used
    }

    pub fn set_always_used(&mut self, always_used: bool) {
        self.allways_used = always_used;
    }

    pub fn get_access_value(&self, offset: isize) -> Option<Cell> {
        if let Some(cell_access) = &self.cell_access {
            for cell in cell_access {
                if cell.offset == offset {
                    return Some(cell.value);
                }
            }
        }
        None
    }

    pub fn was_cell_written(&self, offset: isize) -> bool {
        if let Some(cell_access) = &self.cell_access {
            for cell in cell_access {
                if cell.offset == offset {
                    return cell.value.is_write();
                }
            }
        }

        false
    }

    pub fn was_cell_accessed(&self, offset: isize) -> bool {
        if let Some(cell_access) = &self.cell_access {
            for cell in cell_access {
                if cell.offset == offset {
                    return true;
                }
            }
        }

        false
    }

    pub fn asm(&self, debug: bool) -> String {
        let cell_access = if let Some(cell_access) = self.cell_access() {
            if debug {
                format!("access: {:?}", cell_access)
            } else {
                format!("access: {}", cell_access.len())
            }
        } else {
            "access: None".to_owned()
        };

        if self.allways_used {
            format!("always {}", cell_access)
        } else {
            cell_access
        }
    }
}

/// Cell content inferred during optimization
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Cell {
    /// Cell was accessed but not modified
    Read,

    /// Cell got modified but value is unknown
    Write,

    /// Cell is known to not cotain zero
    NonZero,

    /// Cell is known to be either 0 or 1
    Bool,

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

    pub fn is_constant_write(&self) -> bool {
        matches!(self, Cell::Value(_))
    }
}

/// Hold information about a known modified cell
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct CellAccess {
    /// Cell is only written
    pub write_only: bool,

    /// Offset of the cell relative to the surrounding scope (typically the counter ptr of a local loop)
    pub offset: isize,

    /// The value of th3 cell
    pub value: Cell,
}

impl CellAccess {
    pub fn new_read(offset: isize) -> CellAccess {
        CellAccess {
            write_only: false,
            offset,
            value: Cell::Read,
        }
    }

    pub fn new_write(offset: isize) -> CellAccess {
        CellAccess {
            write_only: false,
            offset,
            value: Cell::Write,
        }
    }

    pub fn new_value(offset: isize, value: u8) -> CellAccess {
        CellAccess {
            write_only: true,
            offset,
            value: Cell::Value(value),
        }
    }

    pub fn add(cells: &mut Vec<CellAccess>, offset: isize, value: Cell) {
        for exiting_cell in cells.iter_mut() {
            if exiting_cell.offset == offset {
                if !value.is_read() {
                    exiting_cell.value = value;
                } else {
                    exiting_cell.write_only = false;
                }
                return;
            }
        }

        cells.push(CellAccess {
            write_only: value.is_constant_write(),
            offset,
            value,
        });
    }

    pub fn add_conditional(cells: &mut Vec<CellAccess>, offset: isize, value: Cell) {
        for exiting_cell in cells.iter_mut() {
            if exiting_cell.offset == offset {
                match (&exiting_cell.value, value) {
                    (Cell::Value(v1), Cell::Value(v2)) => {
                        if *v1 != v2 {
                            if *v1 != 0 && v2 != 0 {
                                exiting_cell.value = Cell::NonZero
                            } else if (*v1 == 0 || *v1 == 1) && (v2 == 0 || v2 == 1) {
                                exiting_cell.value = Cell::Bool
                            } else {
                                exiting_cell.value = Cell::Write
                            }
                        }
                    }
                    (Cell::NonZero, Cell::Value(v)) => {
                        if v != 0 {
                            exiting_cell.value = Cell::NonZero
                        } else {
                            exiting_cell.value = Cell::Write
                        }
                    }
                    (Cell::Value(v), Cell::NonZero)
                    => {
                        if *v != 0 {
                            exiting_cell.value = Cell::NonZero
                        } else {
                            exiting_cell.value = Cell::Write
                        }
                    }
                    (Cell::Bool, Cell::Value(v)) => {
                        if v == 0 || v == 1 {
                            exiting_cell.value = Cell::Bool
                        } else {
                            exiting_cell.value = Cell::Write
                        }
                    }
                    (Cell::Value(v), Cell::Bool)
                    => {
                        if *v == 0 || *v == 1 {
                            exiting_cell.value = Cell::Bool
                        } else {
                            exiting_cell.value = Cell::Write
                        }
                    }
                    (Cell::NonZero, Cell::NonZero) |
                    (_, Cell::Read) => {
                        // Ignore
                    }
                    (Cell::Read, new) => {
                        exiting_cell.value = new
                    }
                    _ => {
                        exiting_cell.value = Cell::Write
                    }
                }

                if !value.is_constant_write() {
                    exiting_cell.write_only = false;
                }
                return;
            }
        }

        cells.push(CellAccess {
            write_only: value.is_constant_write(),
            offset,
            value,
        });
    }

    pub fn add_backward(cells: &mut Vec<CellAccess>, offset: isize, value: Cell) {
        for exiting_cell in cells.iter_mut() {
            if exiting_cell.offset == offset && !matches!(exiting_cell.value, Cell::Read) {
                return;
            }
        }

        cells.push(CellAccess {
            write_only: value.is_constant_write(),
            offset,
            value,
        });
    }

    pub fn get(cells: &[CellAccess], offset: isize) -> Option<Cell> {
        cells.iter().find(|c| c.offset == offset).map(|a| a.value)
    }

    pub fn was_written(cells: &[CellAccess], offset: isize) -> bool {
        for cell in cells {
            if cell.offset == offset {
                return cell.value.is_write();
            }
        }


        false
    }
}
