use std::ops::Range;
use crate::ir::opt_info::BlockInfo;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Op {
    pub op_type: OpType,
    pub span: Range<usize>,
}

impl Op {
    pub fn start() -> Op {
        Op {
            op_type: OpType::Start,
            span: 0..1,
        }
    }

    pub fn inc_ptr(span: Range<usize>, count: usize) -> Op {
        Op {
            op_type: OpType::IncPtr(count),
            span,
        }
    }

    pub fn dec_ptr(span: Range<usize>, count: usize) -> Op {
        Op {
            op_type: OpType::DecPtr(count),
            span,
        }
    }

    pub fn ptr_offset(span: Range<usize>, offset: isize) -> Op {
        Op {
            op_type: OpType::new_ptr_offset(offset),
            span,
        }
    }

    pub fn inc(span: Range<usize>, count: u8) -> Op {
        Op {
            op_type: OpType::Inc(0, count),
            span,
        }
    }

    pub fn inc_with_offset(span: Range<usize>, offset: isize, count: u8) -> Op {
        Op {
            op_type: OpType::Inc(offset, count),
            span,
        }
    }

    pub fn dec(span: Range<usize>, count: u8) -> Op {
        Op {
            op_type: OpType::Dec(0, count),
            span,
        }
    }

    pub fn dec_with_offset(span: Range<usize>, offset: isize, count: u8) -> Op {
        Op {
            op_type: OpType::Dec(offset, count),
            span,
        }
    }

    pub fn copy(span: Range<usize>, src_offset: isize, dest_offset: isize) -> Op {
        Op {
            op_type: OpType::Copy(src_offset, dest_offset),
            span,
        }
    }

    pub fn _move(span: Range<usize>, src_offset: isize, dest_offset: isize) -> Op {
        Op {
            op_type: OpType::Move(src_offset, dest_offset),
            span,
        }
    }

    pub fn put_char(span: Range<usize>) -> Op {
        Op {
            op_type: OpType::PutChar(0),
            span,
        }
    }

    pub fn put_char_with_offset(span: Range<usize>, offset: isize) -> Op {
        Op {
            op_type: OpType::PutChar(offset),
            span,
        }
    }

    pub fn get_char(span: Range<usize>) -> Op {
        Op {
            op_type: OpType::GetChar(0),
            span,
        }
    }

    pub fn d_loop(span: Range<usize>, ops: Vec<Op>, info: BlockInfo) -> Op {
        Op {
            op_type: OpType::DLoop(ops, info),
            span,
        }
    }

    pub fn l_loop(span: Range<usize>, ops: Vec<Op>, info: BlockInfo) -> Op {
        Op {
            op_type: OpType::LLoop(ops, info),
            span,
        }
    }

    pub fn i_loop(span: Range<usize>, ops: Vec<Op>, step: u8, info: BlockInfo) -> Op {
        Op {
            op_type: OpType::ILoop(ops, step, LoopDecrement::Auto, info),
            span,
        }
    }

    pub fn i_loop_with_decrement(span: Range<usize>, ops: Vec<Op>, step: u8, decrement: LoopDecrement, info: BlockInfo) -> Op {
        Op {
            op_type: OpType::ILoop(ops, step, decrement, info),
            span,
        }
    }

    pub fn c_loop(span: Range<usize>, ops: Vec<Op>, iterations: u8, info: BlockInfo) -> Op {
        Op {
            op_type: OpType::CLoop(ops, iterations, LoopDecrement::Auto, info),
            span,
        }
    }

    pub fn c_loop_with_decrement(span: Range<usize>, ops: Vec<Op>, iterations: u8, decrement: LoopDecrement, info: BlockInfo) -> Op {
        Op {
            op_type: OpType::CLoop(ops, iterations, decrement, info),
            span,
        }
    }

    pub fn set(span: Range<usize>, value: u8) -> Op {
        Op {
            op_type: OpType::Set(0, value),
            span,
        }
    }

    pub fn set_with_offset(span: Range<usize>, offset: isize, value: u8) -> Op {
        Op {
            op_type: OpType::Set(offset, value),
            span,
        }
    }

    pub fn add(span: Range<usize>, dest_offset: isize, multi: u8) -> Op {
        Op {
            op_type: OpType::Add(0, dest_offset, multi),
            span,
        }
    }

    pub fn add_with_offset(span: Range<usize>, src_offset: isize, dest_offset: isize, multi: u8) -> Op {
        Op {
            op_type: OpType::Add(src_offset, dest_offset, multi),
            span,
        }
    }

    pub fn nz_add(span: Range<usize>, dest_offset: isize, multi: u8) -> Op {
        Op {
            op_type: OpType::NzAdd(0, dest_offset, multi),
            span,
        }
    }

    pub fn c_add(span: Range<usize>, dest_offset: isize, value: u8) -> Op {
        Op {
            op_type: OpType::CAdd(0, dest_offset, value),
            span,
        }
    }

    pub fn nz_c_add(span: Range<usize>, dest_offset: isize, value: u8) -> Op {
        Op {
            op_type: OpType::NzCAdd(0, dest_offset, value),
            span,
        }
    }

    pub fn c_add_with_offset(span: Range<usize>, src_offset: isize, dest_offset: isize, value: u8) -> Op {
        Op {
            op_type: OpType::CAdd(src_offset, dest_offset, value),
            span,
        }
    }

    pub fn sub(span: Range<usize>, dest_offset: isize, multi: u8) -> Op {
        Op {
            op_type: OpType::Sub(0, dest_offset, multi),
            span,
        }
    }

    pub fn c_sub(span: Range<usize>, dest_offset: isize, value: u8) -> Op {
        Op {
            op_type: OpType::CSub(0, dest_offset, value),
            span,
        }
    }

    pub fn nz_sub(span: Range<usize>, dest_offset: isize, multi: u8) -> Op {
        Op {
            op_type: OpType::NzSub(0, dest_offset, multi),
            span,
        }
    }

    pub fn nz_sub_with_offset(span: Range<usize>, src_offset: isize, dest_offset: isize, multi: u8) -> Op {
        Op {
            op_type: OpType::NzSub(src_offset, dest_offset, multi),
            span,
        }
    }

    pub fn nz_c_sub(span: Range<usize>, dest_offset: isize, value: u8) -> Op {
        Op {
            op_type: OpType::NzCSub(0, dest_offset, value),
            span,
        }
    }

    pub fn mul(span: Range<usize>, dest_offset: isize, multi: u8) -> Op {
        Op {
            op_type: OpType::Mul(0, dest_offset, multi),
            span,
        }
    }

    pub fn nz_mul(span: Range<usize>, dest_offset: isize, multi: u8) -> Op {
        Op {
            op_type: OpType::NzMul(0, dest_offset, multi),
            span,
        }
    }

    pub fn t_nz(span: Range<usize>, ops: Vec<Op>, info: BlockInfo) -> Op {
        Op {
            op_type: OpType::TNz(ops, info),
            span,
        }
    }

    pub fn search_zero(span: Range<usize>, step: isize) -> Op {
        Op {
            op_type: OpType::SearchZero(step, false),
            span,
        }
    }
}

/// Information about when to decrement a loop counter
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum LoopDecrement {
    /// Decrement before body invocation
    Pre,

    /// Decrement after body invocation
    Post,

    /// Doesn't matter.
    /// The counter isn't accessed during loop invocation
    Auto,
}


#[derive(Clone, Debug, PartialEq, Eq)]
pub enum OpType {
    /// Start of application
    /// This is a nop op but helps optimization passes to detect if a cell is zero
    Start,

    /// Increment heap pointer by offset
    IncPtr(usize),

    /// Decrement heap pointer by offset
    DecPtr(usize),

    /// Increment cell at offset
    Inc(isize, u8),

    /// Decrement cell at offset
    Dec(isize, u8),

    /// Set cell at offset
    Set(isize, u8),

    /// Add current value to value at offset and reset current value to 0
    Add(isize, isize, u8),

    /// Add current value to value at offset without setting current value to 0
    NzAdd(isize, isize, u8),

    /// Add constant value to value at offset and reset current value to 0
    CAdd(isize, isize, u8),

    /// Add constant value to value at offset without setting current value to 0
    NzCAdd(isize, isize, u8),

    /// Subtract current value to value at offset and reset current value to 0
    Sub(isize, isize, u8),

    /// Subtract current value to value at offset without setting current value to 0
    NzSub(isize, isize, u8),

    /// Subtract constant value to value at offset and reset current value to 0
    CSub(isize, isize, u8),

    /// Subtract constant value to value at offset without setting current value to 0
    NzCSub(isize, isize, u8),

    /// Multiply current value to value at offset and reset current value to 0
    Mul(isize, isize, u8),

    /// Multiply current value to value at offset without setting current value to 0
    NzMul(isize, isize, u8),

    /// Move value to value at offset and reset current value to 0
    Move(isize, isize),

    /// Copy value to value at offset without setting current value to 0
    Copy(isize, isize),

    /// Output current cell
    PutChar(isize),

    ///  Outputs a constant string/array
    PutString(Vec<u8>),

    /// Read from stdin into current cell
    GetChar(isize),

    /// Dynamic loop as defined in raw brainfuck source
    DLoop(Vec<Op>, BlockInfo),

    /// Loop using the same iterator cell for each iteration
    LLoop(Vec<Op>, BlockInfo),

    /// Loop with an iterator variable and know steps per iteration
    ILoop(Vec<Op>, u8, LoopDecrement, BlockInfo),

    /// Loop with compile time known iteration count
    CLoop(Vec<Op>, u8, LoopDecrement, BlockInfo),

    /// Test if not zero.
    ///
    /// Executes block if current value is not zero. Similar to `if true { ops }`
    TNz(Vec<Op>, BlockInfo),

    /// Move heap pointer to first cell containing zero based on step
    SearchZero(isize, bool),
}

impl OpType {
    /// Get IncPtr or DecPtr according to offset
    pub fn new_ptr_offset(offset: isize) -> OpType {
        if offset < 0 {
            OpType::DecPtr((-offset) as usize)
        } else {
            OpType::IncPtr(offset as usize)
        }
    }

    pub fn is_ptr_inc_or_dec(&self) -> bool {
        matches!(self, OpType::DecPtr(_) | OpType::IncPtr(_))
    }

    /// Return offset of pointer increments and decrements
    pub fn get_ptr_offset(&self) -> Option<isize> {
        match self {
            OpType::IncPtr(count) => Some(*count as isize),
            OpType::DecPtr(count) => Some(-(*count as isize)),
            _ => None,
        }
    }

    /// Test for arithmetic ops without offsets
    pub fn is_simple_arithmetic(&self) -> bool {
        matches!(self,
            OpType::Set(_, _) |
            OpType::Inc(_, _) |
            OpType::Dec(_, _) |
            OpType::IncPtr(_) |
            OpType::DecPtr(_)
        )
    }

    /// Test for arithmetic ops
    pub fn is_arithmetic(&self) -> bool {
        matches!(self,
            OpType::Set(..) |
            OpType::Inc(..) |
            OpType::Dec(..)
        )
    }

    /// Test if op zeros the src offset
    pub fn is_zeroing(&self, test_offset: isize) -> bool {
        match self {
            OpType::Add(offset, ..) |
            OpType::CAdd(offset, ..) |
            OpType::Sub(offset, ..) |
            OpType::CSub(offset, ..) |
            OpType::Move(offset, ..) |
            OpType::Set(offset, 0) => {
                test_offset == *offset
            }
            OpType::DLoop(..) |
            OpType::LLoop(..) |
            OpType::ILoop(..) |
            OpType::CLoop(..) |
            OpType::TNz(..) |
            OpType::SearchZero(..) => {
                test_offset == 0
            }
            OpType::Start => true,
            _ => false,
        }
    }

    /// Test if op zeros the src offset without reading/using the value
    pub fn is_zeroing_unread(&self, test_offset: isize) -> bool {
        match self {
            OpType::CAdd(offset, ..) |
            OpType::CSub(offset, ..) |
            OpType::Set(offset, 0) => {
                test_offset == *offset
            }
            OpType::CLoop(..) => {
                test_offset == 0
            }
            _ => false,
        }
    }

    /// Returns the destination offset for ops that overwrites a cell
    pub fn get_overwriting_dest_offset(&self) -> Option<isize> {
        match self {
            OpType::Move(_, dest_offset) |
            OpType::Copy(_, dest_offset) |
            OpType::Set(dest_offset, _) |
            OpType::GetChar(dest_offset)
            => {
                Some(*dest_offset)
            }

            _ => None,
        }
    }

    /// Returns the source offset for ops that set it to zero without reading the value
    pub fn get_unread_zeroing_src_offset(&self) -> Option<isize> {
        match self {
            OpType::CAdd(offset, ..) |
            OpType::CSub(offset, ..) |
            OpType::Set(offset, 0) |
            OpType::GetChar(offset) => {
                Some(*offset)
            }
            OpType::CLoop(..) => {
                Some(0)
            }
            _ => None,
        }
    }

    pub fn get_children(&self) -> Option<&Vec<Op>> {
        match self {
            OpType::DLoop(children, ..) |
            OpType::LLoop(children, ..) |
            OpType::ILoop(children, ..) |
            OpType::CLoop(children, ..) |
            OpType::TNz(children, ..) => Some(children),
            _ => None,
        }
    }

    pub fn get_children_mut(&mut self) -> Option<&mut Vec<Op>> {
        match self {
            OpType::DLoop(children, ..) |
            OpType::LLoop(children, ..) |
            OpType::ILoop(children, ..) |
            OpType::CLoop(children, ..) |
            OpType::TNz(children, ..) => Some(children),
            _ => None,
        }
    }

    pub fn get_block_info(&mut self) -> Option<&BlockInfo> {
        match self {
            OpType::DLoop(_, info) |
            OpType::LLoop(_, info) |
            OpType::ILoop(_, _, _, info) |
            OpType::CLoop(_, _, _, info) |
            OpType::TNz(_, info) => Some(info),
            _ => None,
        }
    }

    pub fn get_block_info_mut(&mut self) -> Option<&mut BlockInfo> {
        match self {
            OpType::DLoop(_, info) |
            OpType::LLoop(_, info) |
            OpType::ILoop(_, _, _, info) |
            OpType::CLoop(_, _, _, info) |
            OpType::TNz(_, info) => Some(info),
            _ => None,
        }
    }

    pub fn is_possible_write(&self, test_offset: isize) -> bool {
        match self {
            OpType::Inc(offset, _) |
            OpType::Dec(offset, _) |
            OpType::Set(offset, _) |
            OpType::GetChar(offset) |
            OpType::NzAdd(_, offset, _) |
            OpType::NzCAdd(_, offset, _) |
            OpType::NzSub(_, offset, _) |
            OpType::NzCSub(_, offset, _) |
            OpType::NzMul(_, offset, _) |
            OpType::Copy(_, offset)
            => {
                *offset == test_offset
            }
            OpType::Add(src_offset, dest_offset, _) |
            OpType::CAdd(src_offset, dest_offset, _) |
            OpType::Sub(src_offset, dest_offset, _) |
            OpType::CSub(src_offset, dest_offset, _) |
            OpType::Mul(src_offset, dest_offset, _) |
            OpType::Move(src_offset, dest_offset)
            => {
                *src_offset == test_offset || *dest_offset == test_offset
            }
            OpType::PutString(_) |
            OpType::PutChar(_) |
            OpType::IncPtr(_) |
            OpType::DecPtr(_)
            => false,
            OpType::Start |
            OpType::SearchZero(_, _) |
            OpType::DLoop(..) => true,
            OpType::LLoop(.., info) |
            OpType::ILoop(.., info) |
            OpType::CLoop(.., info) |
            OpType::TNz(.., info)
            => {
                test_offset == 0 || info.was_cell_written(test_offset)
            }
        }
    }
}
