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

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum CellValue {
    Unknown,
    NonZero,
    Bool,
    Value(u8),
    Range(u8, u8),
}

impl CellValue {
    pub fn is_not_zero(&self) -> bool {
        match self {
            CellValue::NonZero => true,
            CellValue::Value(v) => *v != 0,
            CellValue::Range(start, _) => *start > 0,
            CellValue::Bool => false,
            CellValue::Unknown => false,
        }
    }
    pub fn is_zero(&self) -> bool {
        matches!(self, CellValue::Value(0))
    }
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

pub fn run_non_local_pass<F>(ops: &mut Vec<Op>, func: F, zeroed: bool, inputs: &[(isize, CellValue)], wrapping_is_ub: bool) -> bool
    where F: Fn(&mut Vec<Op>, bool, &[(isize, CellValue)], bool) -> bool + Copy
{
    let mut progress = false;

    progress |= func(ops, zeroed, inputs, wrapping_is_ub);

    let mut access = AccessIndices::new(zeroed);

    for (offset, _) in inputs {
        access.add(*offset)
    }

    let mut i = 0;

    while i < ops.len() {
        let op = &mut ops[i];

        let is_loop = match &mut op.op_type {
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
            OpType::SearchZero(step, always)
            => {
                access.clear();
                access.add(0);

                if *always {
                    access.add(-*step);
                }

                false
            }
            OpType::DLoop(children, info)
            => {
                if info.has_cell_access() {
                    true
                } else {
                    access.clear();
                    progress |= run_non_local_pass(children, func, false, &[], wrapping_is_ub);
                    false
                }
            }
            OpType::LLoop(..) |
            OpType::ILoop(..) |
            OpType::CLoop(..) |
            OpType::TNz(..) |
            OpType::DTNz(..)
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

        if is_loop {
            let loop_inputs = get_loop_inputs(ops, &inputs, wrapping_is_ub, &mut access, i);

            if let Some(children) = ops[i].op_type.get_children_mut() {
                progress |= run_non_local_pass(children, func, access.zeroed(), &loop_inputs, wrapping_is_ub);
            } else {
                unreachable!("Local loops must have children");
            }

            if let Some(info) = ops[i].op_type.get_block_info() {
                if let Some(cell_access) = info.cell_access() {
                    for cell in cell_access {
                        access.add(cell.offset);
                    }
                }
            } else {
                unreachable!("Local loops must a block info");
            }
        }

        i += 1
    }

    progress
}

fn get_loop_inputs(ops: &mut Vec<Op>, inputs: &&[(isize, CellValue)], wrapping_is_ub: bool, access: &mut AccessIndices, i: usize) -> Vec<(isize, CellValue)> {
    let is_d_loop = matches!(ops[i].op_type, OpType::DLoop(..));

    let loop_inputs = if is_d_loop {
        if let Some(info) = ops[i].op_type.get_block_info() {
            let mut loop_inputs = vec![];
            if let Some(cell_access) = info.cell_access() {
                for cell in cell_access.clone() {
                    match (cell.value, find_heap_value(
                        ops,
                        cell.offset,
                        i as isize - 1,
                        access.zeroed,
                        &inputs,
                        wrapping_is_ub,
                        true)) {
                        (Cell::Value(v1), CellValue::Value(v2)) => {
                            if v1 == v2 {
                                loop_inputs.push((cell.offset, CellValue::Value(v1)));
                            } else if v1 != 0 && v2 != 0 {
                                loop_inputs.push((cell.offset, CellValue::NonZero));
                            } else if (v1 == 0 || v1 == 1) && (v2 == 0 || v2 == 1) {
                                loop_inputs.push((cell.offset, CellValue::Bool));
                            } else {
                                let min = v1.min(v2);
                                let max = v1.max(v2);
                                if min != 0 && max != 255 {
                                    loop_inputs.push((cell.offset, CellValue::Range(min, max)));
                                }
                            }
                        }
                        (Cell::NonZero, CellValue::NonZero) => {
                            loop_inputs.push((cell.offset, CellValue::NonZero));
                        }
                        (Cell::NonZero, CellValue::Value(v)) |
                        (Cell::Value(v), CellValue::NonZero) => {
                            if v > 0 {
                                loop_inputs.push((cell.offset, CellValue::NonZero));
                            }
                        }
                        (Cell::Bool, CellValue::Bool) => {
                            loop_inputs.push((cell.offset, CellValue::Bool));
                        }
                        (Cell::Bool, CellValue::Value(v)) |
                        (Cell::Value(v), CellValue::Bool) => {
                            if v == 0 || v == 1 {
                                loop_inputs.push((cell.offset, CellValue::Bool));
                            }
                        }
                        (Cell::Range(s1, e1), CellValue::Range(s2, e2)) => {
                            loop_inputs.push((cell.offset, CellValue::Range(s1.min(s2), e1.max(e2))));
                        }
                        (Cell::Range(s1, e1), CellValue::Value(v)) |
                        (Cell::Value(v), CellValue::Range(s1, e1)) => {
                            loop_inputs.push((cell.offset, CellValue::Range(s1.min(v), e1.max(v))));
                        }
                        (Cell::Range(s1, e1), CellValue::Bool) |
                        (Cell::Bool, CellValue::Range(s1, e1)) => {
                            loop_inputs.push((cell.offset, CellValue::Range(s1.min(0), e1.max(1))));
                        }
                        (Cell::Range(s1, _), CellValue::NonZero) |
                        (Cell::NonZero, CellValue::Range(s1, _)) => {
                            if s1 > 0 {
                                loop_inputs.push((cell.offset, CellValue::NonZero));
                            }
                        }
                        _ => {
                            // Ignore
                        }
                    }
                }
            }

            let mut has_loop_offset = false;

            for (offset, value) in &mut loop_inputs {
                if *offset == 0 {
                    match value {
                        CellValue::Unknown => {
                            *value = CellValue::NonZero
                        }
                        CellValue::Bool => {
                            *value = CellValue::Value(1)
                        }
                        CellValue::Range(s, e) => {
                            *value = CellValue::Range((*s).max(1), *e);
                        }
                        _ => {
                            // Ignore
                        }
                    }
                    has_loop_offset = true;
                    break;
                }
            }

            if !has_loop_offset {
                loop_inputs.push((0, CellValue::NonZero));
            }

            access.clear();

            loop_inputs
        } else {
            unreachable!("Loops must have block info");
        }
    } else {
        access.add(0);

        let mut loop_inputs = access.indices.iter().map(|offset| {
            if *offset == access.offset {
                if matches!(ops[i].op_type, OpType::TNz(..) | OpType::DTNz(..)) {
                    if find_heap_value(ops,
                                       offset - access.offset,
                                       i as isize - 1,
                                       access.zeroed(),
                                       &inputs,
                                       wrapping_is_ub,
                                       true) == CellValue::Bool {
                        (0, CellValue::Value(1))
                    } else {
                        (0, CellValue::NonZero)
                    }
                } else {
                    (0, CellValue::NonZero)
                }
            } else {
                (offset - access.offset, find_heap_value(ops,
                                                         offset - access.offset,
                                                         i as isize - 1,
                                                         access.zeroed(),
                                                         &inputs,
                                                         wrapping_is_ub,
                                                         true))
            }
        }).collect::<Vec<_>>();

        if !matches!(ops[i].op_type, OpType::TNz(..) | OpType::DTNz(..)) {
            if let Some(info) = ops[i].op_type.get_block_info() {
                if let Some(cell_access) = info.cell_access() {
                    for cell in cell_access {
                        if cell.offset != 0 {
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
                                                        if *v != 0 && *v2 != 0 {
                                                            *value = CellValue::NonZero;
                                                        } else if (*v == 0 || *v == 1) && (*v2 == 0 || *v2 == 1) {
                                                            *value = CellValue::Bool;
                                                        } else {
                                                            let min = (*v).min(*v2);
                                                            let max = (*v).max(*v2);
                                                            if min != 0 && max != 255 {
                                                                *value = CellValue::Range(min, max);
                                                            } else {
                                                                *value = CellValue::Unknown;
                                                            }
                                                        }
                                                    }
                                                }
                                                CellValue::Range(start, end) => {
                                                    let start = (*start).min(*v);
                                                    let end = (*end).max(*v);
                                                    if start == 0 && end == 255 {
                                                        *value = CellValue::Unknown;
                                                    } else {
                                                        *value = CellValue::Range(start, end);
                                                    }
                                                }
                                                CellValue::Bool => {
                                                    if *v != 0 && *v != 1 {
                                                        *value = CellValue::Unknown;
                                                    }
                                                }
                                                CellValue::NonZero => {
                                                    if *v == 0 {
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
                                Cell::Range(s1, e1) => {
                                    let mut found = false;

                                    for (offset, value) in loop_inputs.iter_mut() {
                                        if *offset == cell.offset {
                                            match value {
                                                CellValue::Unknown => {
                                                    // Keep as is
                                                }
                                                CellValue::Value(v) => {
                                                    let start = (*s1).min(*v);
                                                    let end = (*e1).max(*v);
                                                    if start == 0 && end == 255 {
                                                        *value = CellValue::Unknown;
                                                    } else {
                                                        *value = CellValue::Range(start, end);
                                                    }
                                                }
                                                CellValue::Range(start, end) => {
                                                    let start = (*start).min(*s1);
                                                    let end = (*end).max(*e1);
                                                    if start == 0 && end == 255 {
                                                        *value = CellValue::Unknown;
                                                    } else {
                                                        *value = CellValue::Range(start, end);
                                                    }
                                                }
                                                CellValue::Bool => {
                                                    let start = (*s1).min(0);
                                                    let end = (*e1).max(1);
                                                    if start == 0 && end == 255 {
                                                        *value = CellValue::Unknown;
                                                    } else {
                                                        *value = CellValue::Range(start, end);
                                                    }
                                                }
                                                CellValue::NonZero => {
                                                    if *s1 > 0 {
                                                        *value = CellValue::NonZero;
                                                    } else {
                                                        *value = CellValue::Unknown;
                                                    }
                                                }
                                            }
                                            found = true;
                                            break;
                                        }
                                    }
                                    if !found {
                                        if access.zeroed() {
                                            let start = 0.min(*s1);
                                            if start == 0 && *e1 == 255 {
                                                loop_inputs.push((cell.offset, CellValue::Unknown));
                                            } else {
                                                loop_inputs.push((cell.offset, CellValue::Range(start, *e1)));
                                            }
                                        } else {
                                            loop_inputs.push((cell.offset, CellValue::Unknown));
                                        }
                                    }
                                }
                                Cell::NonZero => {
                                    let mut found = false;

                                    for (offset, value) in loop_inputs.iter_mut() {
                                        if *offset == cell.offset {
                                            match value {
                                                CellValue::Unknown => {
                                                    // Keep as is
                                                }
                                                CellValue::Value(v2) => {
                                                    if *v2 != 0 {
                                                        *value = CellValue::NonZero;
                                                    } else {
                                                        *value = CellValue::Unknown;
                                                    }
                                                }
                                                CellValue::Range(start, _) => {
                                                    if *start > 0 {
                                                        *value = CellValue::NonZero;
                                                    } else {
                                                        *value = CellValue::Unknown;
                                                    }
                                                }
                                                CellValue::NonZero => {
                                                    *value = CellValue::NonZero;
                                                }
                                                CellValue::Bool => {
                                                    *value = CellValue::Unknown;
                                                }
                                            }
                                            found = true;
                                            break;
                                        }
                                    }

                                    if !found {
                                        loop_inputs.push((cell.offset, CellValue::Unknown))
                                    }
                                }
                                Cell::Bool => {
                                    let mut found = false;

                                    for (offset, value) in loop_inputs.iter_mut() {
                                        if *offset == cell.offset {
                                            match value {
                                                CellValue::Unknown |
                                                CellValue::Bool => {
                                                    // Keep as is
                                                }
                                                CellValue::Value(v2) => {
                                                    if *v2 == 0 || *v2 == 1 {
                                                        *value = CellValue::Bool;
                                                    } else {
                                                        *value = CellValue::Unknown;
                                                    }
                                                }
                                                CellValue::Range(start, end) => {
                                                    let start = (*start).min(0);
                                                    let end = (*end).max(1);
                                                    if start == 0 && end == 255 {
                                                        *value = CellValue::Unknown;
                                                    } else {
                                                        *value = CellValue::Range(start, end);
                                                    }
                                                }
                                                CellValue::NonZero => {
                                                    *value = CellValue::Unknown;
                                                }
                                            }
                                            found = true;
                                            break;
                                        }
                                    }

                                    if !found {
                                        loop_inputs.push((cell.offset, CellValue::Unknown))
                                    }
                                }
                            }
                        }
                    }
                }
            } else {
                unreachable!("Loops must have block info");
            }
        }

        if matches!(ops[i].op_type, OpType::DTNz(..)) {
            access.clear();
        }

        let mut has_loop_offset = false;

        for (offset, value) in &mut loop_inputs {
            if *offset == 0 {
                match value {
                    CellValue::Unknown => {
                        *value = CellValue::NonZero
                    }
                    CellValue::Bool => {
                        *value = CellValue::Value(1)
                    }
                    CellValue::Range(s, e) => {
                        *value = CellValue::Range((*s).max(1), *e);
                    }
                    _ => {
                        // Ignore
                    }
                }
                has_loop_offset = true;
                break;
            }
        }

        if !has_loop_offset {
            loop_inputs.push((0, CellValue::NonZero));
        }

        loop_inputs
    };
    loop_inputs
}

pub fn find_heap_value(ops: &[Op],
                       start_cell_offset: isize,
                       start_index: isize,
                       zeroed: bool,
                       inputs: &[(isize, CellValue)],
                       wrapping_is_ub: bool,
                       follow: bool) -> CellValue {
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
            OpType::Inc(offset, v) |
            OpType::CAdd(_, offset, v) |
            OpType::NzCAdd(_, offset, v)
            => {
                if *offset == cell_offset {
                    return if wrapping_is_ub && *v > 0 {
                        CellValue::NonZero
                    } else {
                        match find_heap_value(ops, *offset, i - 1, zeroed, inputs, wrapping_is_ub, true) {
                            CellValue::Bool => {
                                if *v > 0 && *v < 254 {
                                    CellValue::NonZero
                                } else {
                                    CellValue::Unknown
                                }
                            }
                            CellValue::Range(start, end) => {
                                if 255 - end >= *v {
                                    CellValue::Range(start + *v, end + *v)
                                } else {
                                    CellValue::Unknown
                                }
                            }
                            _ => CellValue::Unknown
                        }
                    };
                }
            }
            OpType::Move(src_offset, dest_offset) |
            OpType::Copy(src_offset, dest_offset)
            => {
                if *dest_offset == cell_offset {
                    return if follow {
                        find_heap_value(ops, *src_offset, i - 1, zeroed, inputs, wrapping_is_ub, true)
                    } else {
                        CellValue::Unknown
                    };
                }
            }
            OpType::Sub(src_offset, dest_offset, _) |
            OpType::NzSub(src_offset, dest_offset, _) => {
                if *dest_offset == cell_offset {
                    let src_value = find_heap_value(ops, *src_offset, i - 1, zeroed, inputs, wrapping_is_ub, true);
                    let dest_value = find_heap_value(ops, *dest_offset, i - 1, zeroed, inputs, wrapping_is_ub, true);

                    return match (src_value, dest_value) {
                        (CellValue::Bool, CellValue::Value(1)) => {
                            CellValue::Bool
                        }
                        _ => CellValue::Unknown
                    };
                }
            }
            OpType::Mul(src_offset, dest_offset, _) |
            OpType::NzMul(src_offset, dest_offset, _)
            => {
                if *dest_offset == cell_offset {
                    let src_value = find_heap_value(ops, *src_offset, i - 1, zeroed, inputs, wrapping_is_ub, true);
                    let dest_value = find_heap_value(ops, *dest_offset, i - 1, zeroed, inputs, wrapping_is_ub, true);

                    return match (src_value, dest_value) {
                        (CellValue::Bool, CellValue::Bool) => {
                            CellValue::Bool
                        }
                        _ => CellValue::Unknown
                    };
                }
            }
            OpType::Dec(offset, v) |
            OpType::CSub(_, offset, v) |
            OpType::NzCSub(_, offset, v) => {
                if *offset == cell_offset {
                    match find_heap_value(ops, *offset, i - 1, zeroed, inputs, wrapping_is_ub, true) {
                        CellValue::NonZero => {
                            if *v == 1 {
                                return CellValue::Range(0, 254);
                            } else {}
                        }
                        CellValue::Range(start, end) => {
                            if start >= *v {
                                return CellValue::Range(start - *v, end - *v);
                            }
                        }
                        _ => {
                            // Ignore
                        }
                    }

                    return CellValue::Unknown;
                }
            }
            OpType::Add(_, offset, _) |
            OpType::NzAdd(_, offset, _) |
            OpType::GetChar(offset)
            => {
                if *offset == cell_offset {
                    return CellValue::Unknown;
                }
            }
            OpType::PutChar(..) => {
                // Ignore
            }
            OpType::DTNz(_, Some(offset), info) => {
                if info.always_used() {
                    if let Some(value) = info.get_access_value(cell_offset) {
                        match value {
                            Cell::Write => return CellValue::Unknown,
                            Cell::Value(v) => return CellValue::Value(v),
                            Cell::Range(start, end) => return CellValue::Range(start, end),
                            Cell::NonZero => return CellValue::NonZero,
                            Cell::Bool => return CellValue::Bool,
                            Cell::Read => {
                                // ignore
                            }
                        }
                    }
                    return CellValue::Unknown;
                }

                match info.get_access_value(cell_offset) {
                    Some(Cell::Value(loop_value)) => {
                        let loop_inputs = inputs.iter().map(|(offset, cell)| {
                            (offset + cell_offset - start_cell_offset, *cell)
                        }).collect::<Vec<_>>();

                        match find_heap_value(ops, cell_offset, i - 1, zeroed, &loop_inputs, wrapping_is_ub, follow) {
                            CellValue::Value(input_value) => {
                                if loop_value == input_value {
                                    return CellValue::Value(loop_value);
                                } else if (loop_value == 0 || loop_value == 1) && (input_value == 0 || input_value == 1) {
                                    return CellValue::Bool;
                                } else if loop_value != 0 && input_value != 0 {
                                    return CellValue::NonZero;
                                } else {
                                    let start = (input_value).min(loop_value);
                                    let end = (input_value).max(loop_value);
                                    if start == 0 && end == 255 {
                                        return CellValue::Unknown;
                                    } else {
                                        return CellValue::Range(start, end);
                                    }
                                }
                            }
                            CellValue::Range(start, end) => {
                                let start = (start).min(loop_value);
                                let end = (end).max(loop_value);
                                if start == 0 && end == 255 {
                                    return CellValue::Unknown;
                                } else {
                                    return CellValue::Range(start, end);
                                }
                            }
                            CellValue::NonZero => {
                                if loop_value != 0 {
                                    return CellValue::NonZero;
                                } else {
                                    return CellValue::Unknown;
                                }
                            }
                            CellValue::Bool => {
                                if loop_value == 0 || loop_value == 1 {
                                    return CellValue::Bool;
                                } else {
                                    let start = 0.min(loop_value);
                                    let end = 1.max(loop_value);
                                    if start == 0 && end == 255 {
                                        return CellValue::Unknown;
                                    } else {
                                        return CellValue::Range(start, end);
                                    }
                                }
                            }
                            _ => return CellValue::Unknown
                        }
                    }
                    Some(Cell::NonZero) => {
                        let loop_inputs = inputs.iter().map(|(offset, cell)| {
                            (offset + cell_offset - start_cell_offset, *cell)
                        }).collect::<Vec<_>>();

                        match find_heap_value(ops, cell_offset, i - 1, zeroed, &loop_inputs, wrapping_is_ub, follow) {
                            CellValue::Value(input_value) => {
                                if input_value != 0 {
                                    return CellValue::NonZero;
                                } else {
                                    return CellValue::Unknown;
                                }
                            }
                            CellValue::Range(start, _) => {
                                if start > 0 {
                                    return CellValue::NonZero;
                                } else {
                                    return CellValue::Unknown;
                                }
                            }
                            CellValue::NonZero => {
                                return CellValue::NonZero;
                            }
                            _ => return CellValue::Unknown,
                        }
                    }
                    Some(Cell::Bool) => {
                        let loop_inputs = inputs.iter().map(|(offset, cell)| {
                            (offset + cell_offset - start_cell_offset, *cell)
                        }).collect::<Vec<_>>();

                        match find_heap_value(ops, cell_offset, i - 1, zeroed, &loop_inputs, wrapping_is_ub, follow) {
                            CellValue::Value(input_value) => {
                                if input_value == 0 || input_value == 1 {
                                    return CellValue::Bool;
                                } else {
                                    return CellValue::Unknown;
                                }
                            }
                            CellValue::Range(start, end) => {
                                let start = 0.min(start);
                                let end = 1.max(end);
                                if start == 0 && end == 255 {
                                    return CellValue::Unknown;
                                } else {
                                    return CellValue::Range(start, end);
                                }
                            }
                            CellValue::NonZero => {
                                return CellValue::Unknown;
                            }
                            CellValue::Bool => {
                                return CellValue::Bool;
                            }
                            _ => return CellValue::Unknown
                        }
                    }
                    None => {
                        cell_offset -= offset;
                    }
                    _ => return CellValue::Unknown,
                }
            }
            OpType::DLoop(_, info) |
            OpType::DTNz(_, _, info)
            => {
                if info.always_used() {
                    if let Some(value) = info.get_access_value(cell_offset) {
                        match value {
                            Cell::Write => return CellValue::Unknown,
                            Cell::Value(v) => return CellValue::Value(v),
                            Cell::Range(start, end) => return CellValue::Range(start, end),
                            Cell::NonZero => return CellValue::NonZero,
                            Cell::Bool => return CellValue::Bool,
                            Cell::Read => {
                                // ignore
                            }
                        }
                    }
                    return CellValue::Unknown;
                }

                match info.get_access_value(cell_offset) {
                    Some(Cell::Value(loop_value)) => {
                        let loop_inputs = inputs.iter().map(|(offset, cell)| {
                            (offset + cell_offset - start_cell_offset, *cell)
                        }).collect::<Vec<_>>();

                        match find_heap_value(ops, cell_offset, i - 1, zeroed, &loop_inputs, wrapping_is_ub, follow) {
                            CellValue::Value(input_value) => {
                                if loop_value == input_value {
                                    return CellValue::Value(loop_value);
                                } else if (loop_value == 0 || loop_value == 1) && (input_value == 0 || input_value == 1) {
                                    return CellValue::Bool;
                                } else if loop_value != 0 && input_value != 0 {
                                    return CellValue::NonZero;
                                } else {
                                    let start = loop_value.min(input_value);
                                    let end = loop_value.max(input_value);
                                    if start == 0 && end == 255 {
                                        return CellValue::Unknown;
                                    } else {
                                        return CellValue::Range(start, end);
                                    }
                                }
                            }
                            CellValue::Range(start, end) => {
                                let start = (start).min(loop_value);
                                let end = (end).max(loop_value);
                                if start == 0 && end == 255 {
                                    return CellValue::Unknown;
                                } else {
                                    return CellValue::Range(start, end);
                                }
                            }
                            CellValue::NonZero => {
                                if loop_value != 0 {
                                    return CellValue::NonZero;
                                } else {
                                    return CellValue::Unknown;
                                }
                            }
                            CellValue::Bool => {
                                if loop_value == 0 || loop_value == 1 {
                                    return CellValue::Bool;
                                } else {
                                    return CellValue::Unknown;
                                }
                            }
                            _ => return CellValue::Unknown
                        }
                    }
                    Some(Cell::NonZero) => {
                        let loop_inputs = inputs.iter().map(|(offset, cell)| {
                            (offset + cell_offset - start_cell_offset, *cell)
                        }).collect::<Vec<_>>();

                        match find_heap_value(ops, cell_offset, i - 1, zeroed, &loop_inputs, wrapping_is_ub, follow) {
                            CellValue::Value(input_value) => {
                                if input_value != 0 {
                                    return CellValue::NonZero;
                                } else {
                                    return CellValue::Unknown;
                                }
                            }
                            CellValue::NonZero => {
                                return CellValue::NonZero;
                            }
                            _ => return CellValue::Unknown
                        }
                    }
                    Some(Cell::Bool) => {
                        let loop_inputs = inputs.iter().map(|(offset, cell)| {
                            (offset + cell_offset - start_cell_offset, *cell)
                        }).collect::<Vec<_>>();

                        match find_heap_value(ops, cell_offset, i - 1, zeroed, &loop_inputs, wrapping_is_ub, follow) {
                            CellValue::Value(input_value) => {
                                if input_value == 0 || input_value == 1 {
                                    return CellValue::Bool;
                                } else {
                                    return CellValue::Unknown;
                                }
                            }
                            CellValue::Range(start, end) => {
                                let start = 0.min(start);
                                let end = 1.max(end);
                                if start == 0 && end == 255 {
                                    return CellValue::Unknown;
                                } else {
                                    return CellValue::Range(start, end);
                                }
                            }
                            CellValue::NonZero => {
                                return CellValue::Unknown;
                            }
                            CellValue::Bool => {
                                return CellValue::Bool;
                            }
                            _ => return CellValue::Unknown
                        }
                    }
                    _ => return CellValue::Unknown,
                }
            }
            OpType::LLoop(_, info) |
            OpType::ILoop(_, _, _, info) |
            OpType::TNz(_, info)
            => {
                if !info.has_cell_access() {
                    return CellValue::Unknown;
                }

                if info.always_used() {
                    if let Some(value) = info.get_access_value(cell_offset) {
                        match value {
                            Cell::Write => return CellValue::Unknown,
                            Cell::Value(v) => return CellValue::Value(v),
                            Cell::Range(start, end) => return CellValue::Range(start, end),
                            Cell::NonZero => return CellValue::NonZero,
                            Cell::Bool => return CellValue::Bool,
                            Cell::Read => {
                                // ignore
                            }
                        }
                    }
                    return CellValue::Unknown;
                }

                if let Some(value) = info.get_access_value(cell_offset) {
                    match value {
                        Cell::Write => return CellValue::Unknown,
                        Cell::Value(v) => {
                            if i > 0 {
                                let loop_inputs = inputs.iter().map(|(offset, cell)| {
                                    (offset + cell_offset - start_cell_offset, *cell)
                                }).collect::<Vec<_>>();

                                match find_heap_value(ops, cell_offset, i - 1, zeroed, &loop_inputs, wrapping_is_ub, follow) {
                                    CellValue::Value(v2) => {
                                        if v == v2 {
                                            return CellValue::Value(v);
                                        } else if v == 0 && v2 == 1 || v == 1 && v2 == 0 {
                                            return CellValue::Bool;
                                        } else if v != 0 && v2 != 0 {
                                            return CellValue::NonZero;
                                        } else {
                                            let start = v.min(v2);
                                            let end = v.max(v2);
                                            if start != 0 && end != 255 {
                                                return CellValue::Range(start, end);
                                            }
                                        }
                                    }
                                    CellValue::Range(start, end) => {
                                        let start = (start).min(v);
                                        let end = (end).max(v);
                                        if start == 0 && end == 255 {
                                            return CellValue::Unknown;
                                        } else {
                                            return CellValue::Range(start, end);
                                        }
                                    }
                                    CellValue::NonZero => {
                                        if v != 0 {
                                            return CellValue::NonZero;
                                        } else {
                                            return CellValue::Unknown;
                                        }
                                    }
                                    CellValue::Bool => {
                                        if v == 0 || v == 1 {
                                            return CellValue::Bool;
                                        } else {
                                            return CellValue::Unknown;
                                        }
                                    }
                                    CellValue::Unknown => {
                                        return CellValue::Unknown;
                                    }
                                }
                            }
                            return CellValue::Unknown;
                        }
                        Cell::Range(start, end) => {
                            if i > 0 {
                                let loop_inputs = inputs.iter().map(|(offset, cell)| {
                                    (offset + cell_offset - start_cell_offset, *cell)
                                }).collect::<Vec<_>>();

                                match find_heap_value(ops, cell_offset, i - 1, zeroed, &loop_inputs, wrapping_is_ub, follow) {
                                    CellValue::Value(v2) => {
                                        let start = (start).min(v2);
                                        let end = (end).max(v2);
                                        if start == 0 && end == 255 {
                                            return CellValue::Unknown;
                                        } else {
                                            return CellValue::Range(start, end);
                                        }
                                    }
                                    CellValue::Range(start2, end2) => {
                                        let start = (start).min(start2);
                                        let end = (end).max(end2);
                                        if start == 0 && end == 255 {
                                            return CellValue::Unknown;
                                        } else {
                                            return CellValue::Range(start, end);
                                        }
                                    }
                                    CellValue::NonZero => {
                                        if start > 0 {
                                            return CellValue::NonZero;
                                        } else {
                                            return CellValue::Unknown;
                                        }
                                    }
                                    CellValue::Bool => {
                                        let start = (start).min(0);
                                        let end = (end).max(1);
                                        if start == 0 && end == 255 {
                                            return CellValue::Unknown;
                                        } else {
                                            return CellValue::Range(start, end);
                                        }
                                    }
                                    CellValue::Unknown => {
                                        return CellValue::Unknown;
                                    }
                                }
                            }
                            return CellValue::Unknown;
                        }
                        Cell::NonZero => {
                            let loop_inputs = inputs.iter().map(|(offset, cell)| {
                                (offset + cell_offset - start_cell_offset, *cell)
                            }).collect::<Vec<_>>();

                            match find_heap_value(ops, cell_offset, i - 1, zeroed, &loop_inputs, wrapping_is_ub, follow) {
                                CellValue::NonZero => return CellValue::NonZero,
                                CellValue::Value(v) |
                                CellValue::Range(v, _) => {
                                    if v != 0 {
                                        return CellValue::NonZero;
                                    } else {
                                        return CellValue::Unknown;
                                    }
                                }
                                _ => return CellValue::Unknown,
                            }
                        }
                        Cell::Bool => {
                            let loop_inputs = inputs.iter().map(|(offset, cell)| {
                                (offset + cell_offset - start_cell_offset, *cell)
                            }).collect::<Vec<_>>();

                            match find_heap_value(ops, cell_offset, i - 1, zeroed, &loop_inputs, wrapping_is_ub, follow) {
                                CellValue::Bool => {
                                    return CellValue::Bool;
                                }
                                CellValue::Value(v) => {
                                    if v == 0 || v == 1 {
                                        return CellValue::Bool;
                                    } else {
                                        return CellValue::Unknown;
                                    }
                                }
                                CellValue::Range(start, end) => {
                                    let start = 0.min(start);
                                    let end = 1.max(end);
                                    if start == 0 && end == 255 {
                                        return CellValue::Unknown;
                                    } else {
                                        return CellValue::Range(start, end);
                                    }
                                }
                                _ => {
                                    return CellValue::Unknown;
                                }
                            }
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
                            Cell::Range(start, end) => return CellValue::Range(start, end),
                            Cell::NonZero => return CellValue::NonZero,
                            Cell::Bool => return CellValue::Bool,
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
            OpType::SearchZero(step, always) => {
                if *always && -*step == cell_offset {
                    return CellValue::NonZero;
                } else {
                    return CellValue::Unknown;
                }
            }
            OpType::Start => return CellValue::Value(0),
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
            OpType::SearchZero(_, _) |
            OpType::DLoop(_, _) |
            OpType::DTNz(_, _, _) => {
                return None;
            }
            OpType::LLoop(.., info) |
            OpType::ILoop(.., info) |
            OpType::CLoop(.., info) |
            OpType::TNz(.., info)
            => {
                if info.has_cell_access() || info.get_access_value(ptr_offset).is_some() {
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

pub fn find_last_put_string(ops: &[Op], start_index: isize) -> Option<(isize, Vec<u8>)> {
    let mut i = start_index;

    while i >= 0 {
        let op = &ops[i as usize];

        match &op.op_type {
            OpType::PutString(string) => {
                return Some((i, string.to_vec()));
            }
            OpType::PutChar(_) |
            OpType::GetChar(_) |
            OpType::SearchZero(_, _) |
            OpType::DLoop(..) |
            OpType::DTNz(..) |
            OpType::LLoop(..) |
            OpType::ILoop(..) |
            OpType::CLoop(..) |
            OpType::TNz(..)
            => {
                return None;
            }
            _ => {
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