use std::ops::Range;

use crate::ir::ops::{Op, OpType};
use crate::ir::opt_info::Cell;

#[derive(Debug)]
pub enum Change {
    Ignore,
    Remove,
    RemoveOffset(usize),
    Replace(Vec<OpType>),
    ReplaceOffset(usize, Range<usize>, Vec<OpType>),
    RemoveAndReplace(isize, Vec<OpType>),
}

impl Change {
    pub fn apply(self, ops: &mut Vec<Op>, i: usize, size: usize) -> (bool, usize) {
        match self {
            Change::Remove => {
                for _ in 0..size {
                    ops.remove(i);
                }

                (true, 0)
            }
            Change::RemoveOffset(offset) => {
                ops.remove(i + offset);

                (true, 0)
            }
            Change::Replace(op_types) => {
                let span = ops[i].span.start..ops[i + size - 1].span.end;

                for _ in 0..size {
                    ops.remove(i);
                }

                for op_type in op_types.into_iter().rev() {
                    ops.insert_or_push(i, Op {
                        op_type,
                        span: span.clone(),
                    });
                }

                (true, 0)
            }
            Change::RemoveAndReplace(remove_index, op_types) => {
                let span = ops[i].span.start..ops[i + size - 1].span.end;

                for _ in 0..size {
                    ops.remove(i);
                }

                for op_type in op_types.into_iter().rev() {
                    ops.insert_or_push(i, Op {
                        op_type,
                        span: span.clone(),
                    });
                }

                let remove_index = i as isize + remove_index;

                debug_assert!(remove_index >= 0 && remove_index < ops.len() as isize, "Bad remove index {} Length: {}", remove_index, ops.len());

                ops.remove(remove_index as usize);

                (true, 1)
            }
            Change::ReplaceOffset(offset, span, op_types) => {
                for _ in offset..size {
                    ops.remove(i + offset);
                }

                for op_type in op_types.into_iter().rev() {
                    ops.insert_or_push(i + offset, Op {
                        op_type,
                        span: span.clone(),
                    });
                }

                (true, 0)
            }
            Change::Ignore => {
                (false, 0)
            }
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum CellValue {
    Unknown,
    Value(u8),
}

pub fn count_ops_recursive(ops: &[Op]) -> usize {
    let mut count = ops.len();

    for op in ops {
        if let Some(children) = op.op_type.get_children() {
            count += count_ops_recursive(children);
        }
    }

    count
}

struct AccessIndices {
    zeroed: bool,
    offset: isize,
    indices: Vec<isize>,
}

impl AccessIndices {
    pub fn new(zeroed: bool) -> AccessIndices {
        AccessIndices {
            zeroed,
            offset: 0,
            indices: vec![],
        }
    }

    pub fn clear(&mut self) {
        self.zeroed = false;
        self.indices.clear();
    }

    pub fn zeroed(&self) -> bool {
        self.zeroed
    }

    pub fn set_zeroed(&mut self, zeroed: bool) {
        self.zeroed = zeroed;
    }

    pub fn inc_offset(&mut self, offset: usize) {
        self.offset += offset as isize;
    }

    pub fn dec_offset(&mut self, offset: usize) {
        self.offset -= offset as isize;
    }

    pub fn add(&mut self, offset: isize) {
        let offset = self.offset + offset;

        if !self.indices.contains(&offset) {
            self.indices.push(offset);
        }
    }
}

pub fn run_non_local_pass<F>(ops: &mut Vec<Op>, func: F, zeroed: bool, inputs: &[(isize, CellValue)]) -> bool
    where F: Fn(&mut Vec<Op>, bool, &[(isize, CellValue)]) -> bool + Copy
{
    let mut progress = false;

    progress |= func(ops, zeroed, inputs);

    let mut access = AccessIndices::new(zeroed);

    let mut i = 0;

    while i < ops.len() {
        let op = &mut ops[i];

        let is_local_loop = match &mut op.op_type {
            OpType::Start => {
                access.set_zeroed(true);
                false
            }
            OpType::IncPtr(offset) => {
                access.inc_offset(*offset);
                false
            }
            OpType::DecPtr(offset) => {
                access.dec_offset(*offset);
                false
            }
            OpType::Set(offset, _) |
            OpType::Inc(offset, _) |
            OpType::Dec(offset, _) |
            OpType::GetChar(offset)
            => {
                access.add(*offset);
                false
            }
            OpType::Add(src_offset, dest_offset, _) |
            OpType::CAdd(src_offset, dest_offset, _) |
            OpType::Sub(src_offset, dest_offset, _) |
            OpType::CSub(src_offset, dest_offset, _) |
            OpType::Mul(src_offset, dest_offset, _) |
            OpType::Move(src_offset, dest_offset)
            => {
                access.add(*src_offset);
                access.add(*dest_offset);
                false
            }
            OpType::NzAdd(_, offset, _) |
            OpType::NzCAdd(_, offset, _) |
            OpType::NzSub(_, offset, _) |
            OpType::NzCSub(_, offset, _) |
            OpType::NzMul(_, offset, _) |
            OpType::Copy(_, offset)
            => {
                access.add(*offset);
                false
            }
            OpType::SearchZero(_)
            => {
                access.clear();
                false
            }
            OpType::DLoop(children) => {
                access.clear();
                progress |= run_non_local_pass(children, func, false, &[]);
                false
            }
            OpType::LLoop(..) |
            OpType::ILoop(..) |
            OpType::CLoop(..) |
            OpType::TNz(..)
            => {
                true
            }
            OpType::PutChar(_) |
            OpType::PutString(..)
            => {
                // Ignore
                false
            }
        };

        if is_local_loop {
            access.add(0);

            let mut loop_inputs = access.indices.iter().map(|offset| {
                if *offset == access.offset {
                    (0, CellValue::Unknown)
                } else {
                    (offset - access.offset, find_heap_value(ops, offset - access.offset, i as isize - 1, access.zeroed(), &inputs))
                }
            }).collect::<Vec<_>>();

            if let Some(info) = ops[i].op_type.get_block_info() {
                for cell in info.cell_access() {
                    match &cell.value {
                        Cell::Read => {
                            //ignore
                        }
                        Cell::Write => {
                            let mut found = false;

                            for (offset, value) in loop_inputs.iter_mut() {
                                if *offset == cell.offset {
                                    *value = CellValue::Unknown;
                                    found = true;
                                }
                            }

                            if !found {
                                loop_inputs.push((cell.offset, CellValue::Unknown))
                            }
                        }
                        Cell::Value(v) => {
                            let mut found = false;

                            for (offset, value) in loop_inputs.iter_mut() {
                                if *offset == cell.offset {
                                    match value {
                                        CellValue::Unknown => {
                                            // Keep as is
                                        }
                                        CellValue::Value(v2) => {
                                            if *v != *v2 {
                                                *value = CellValue::Unknown;
                                            }
                                        }
                                    }
                                    found = true;
                                    break;
                                }
                            }

                            if !found {
                                if *v == 0 && access.zeroed() {
                                    loop_inputs.push((cell.offset, CellValue::Value(*v)))
                                } else {
                                    loop_inputs.push((cell.offset, CellValue::Unknown))
                                }
                            }
                        }
                    }
                }
            } else {
                unreachable!("Local loops must a block info");
            }

            if let Some(children) = ops[i].op_type.get_children_mut() {
                progress |= run_non_local_pass(children, func, access.zeroed(), &loop_inputs);
            } else {
                unreachable!("Local loops must have children");
            }

            if let Some(info) = ops[i].op_type.get_block_info() {
                for cell in info.cell_access() {
                    access.add(cell.offset);
                }
            } else {
                unreachable!("Local loops must a block info");
            }
        }

        i += 1
    }

    progress
}

pub fn find_heap_value(ops: &[Op], start_cell_offset: isize, start_index: isize, zeroed: bool, inputs: &[(isize, CellValue)]) -> CellValue {
    let mut cell_offset = start_cell_offset;
    let mut i = start_index;

    while i >= 0 {
        let op = &ops[i as usize];

        if op.op_type.is_zeroing(cell_offset) {
            return CellValue::Value(0);
        }

        match &op.op_type {
            OpType::IncPtr(offset) => cell_offset += *offset as isize,
            OpType::DecPtr(offset) => cell_offset -= *offset as isize,
            OpType::Set(offset, v) => {
                if *offset == cell_offset {
                    return CellValue::Value(*v);
                }
            }
            OpType::Inc(offset, _) |
            OpType::Dec(offset, _) |
            OpType::Add(_, offset, _) |
            OpType::NzAdd(_, offset, _) |
            OpType::CAdd(_, offset, _) |
            OpType::NzCAdd(_, offset, _) |
            OpType::Sub(_, offset, _) |
            OpType::NzSub(_, offset, _) |
            OpType::CSub(_, offset, _) |
            OpType::NzCSub(_, offset, _) |
            OpType::Mul(_, offset, _) |
            OpType::NzMul(_, offset, _) |
            OpType::Move(_, offset) |
            OpType::Copy(_, offset)
            => {
                if *offset == cell_offset {
                    return CellValue::Unknown;
                }
            }
            OpType::PutChar(..) => {
                // Ignore
            }
            OpType::LLoop(_, info) |
            OpType::ILoop(_, _, _, info) |
            OpType::TNz(_, info)
            => {
                if let Some(value) = info.get_access_value(cell_offset) {
                    match value {
                        Cell::Write => return CellValue::Unknown,
                        Cell::Value(v) => {
                            if i > 0 {
                                let loop_inputs = inputs.iter().map(|(offset, cell)| {
                                    (offset + cell_offset - start_cell_offset, *cell)
                                }).collect::<Vec<_>>();

                                if let CellValue::Value(v2) = find_heap_value(ops, cell_offset, i - 1, zeroed, &loop_inputs) {
                                    if v == v2 {
                                        return CellValue::Value(v);
                                    }
                                }
                            }
                            return CellValue::Unknown;
                        }
                        Cell::Read => {
                            // ignore
                        }
                    }
                }
            }
            OpType::CLoop(_, count, _, info)
            => {
                if *count > 0 {
                    if let Some(value) = info.get_access_value(cell_offset) {
                        match value {
                            Cell::Write => return CellValue::Unknown,
                            Cell::Value(v) => return CellValue::Value(v),
                            Cell::Read => {
                                // ignore
                            }
                        }
                    }
                }
            }
            OpType::PutString(_) => {
                // Ignore
            }
            _ => return CellValue::Unknown
        }

        i -= 1;
    }

    for (offset, cell) in inputs {
        if *offset == cell_offset {
            return *cell;
        }
    }

    if zeroed {
        CellValue::Value(0)
    } else {
        CellValue::Unknown
    }
}

pub fn find_last_accessing_inc_dec(ops: &[Op], start_offset: isize, start_index: isize) -> Option<(isize, i16)> {
    let mut ptr_offset = start_offset;

    let mut i = start_index;

    while i >= 0 {
        let op = &ops[i as usize];

        match &op.op_type {
            OpType::IncPtr(offset) => ptr_offset += *offset as isize,
            OpType::DecPtr(offset) => ptr_offset -= *offset as isize,
            OpType::Inc(offset, v) => {
                if *offset == ptr_offset {
                    return Some((i, *v as i16));
                }
            }
            OpType::Dec(offset, v) => {
                if *offset == ptr_offset {
                    return Some((i, -(*v as i16)));
                }
            }
            OpType::Add(src_offset, dest_offset, _) |
            OpType::CAdd(src_offset, dest_offset, _) |
            OpType::Sub(src_offset, dest_offset, _) |
            OpType::CSub(src_offset, dest_offset, _) |
            OpType::Mul(src_offset, dest_offset, _) |
            OpType::Move(src_offset, dest_offset)
            => {
                if *src_offset == ptr_offset || *dest_offset == ptr_offset {
                    return None;
                }
            }
            OpType::Set(offset, _) |
            OpType::NzAdd(_, offset, _) |
            OpType::NzCAdd(_, offset, _) |
            OpType::NzSub(_, offset, _) |
            OpType::NzCSub(_, offset, _) |
            OpType::NzMul(_, offset, _) |
            OpType::Copy(_, offset) |
            OpType::GetChar(offset) |
            OpType::PutChar(offset)
            => {
                if *offset == ptr_offset {
                    return None;
                }
            }
            OpType::Start |
            OpType::SearchZero(_) |
            OpType::DLoop(_) => {
                return None;
            }
            OpType::LLoop(.., info) |
            OpType::ILoop(.., info) |
            OpType::CLoop(.., info) |
            OpType::TNz(.., info)
            => {
                if info.get_access_value(ptr_offset).is_some() {
                    return None;
                }
            }
            OpType::PutString(_) => {
                // Ignore
            }
        }

        i -= 1;
    }

    None
}

pub trait OpCodes {
    fn insert_or_push(&mut self, index: usize, op: Op);
}

impl OpCodes for Vec<Op> {
    fn insert_or_push(&mut self, index: usize, op: Op) {
        if index >= self.len() {
            self.push(op);
        } else {
            self.insert(index, op);
        }
    }
}