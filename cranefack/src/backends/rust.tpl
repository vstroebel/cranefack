
use std::io::Read;
use std::io::Write;

const MAX_HEAP_SIZE: usize = 16 * 1024 * 1024;

pub struct Runtime {
    heap: Vec<u8>,
    pointer: usize,
}

impl Runtime {
    pub fn new() -> Runtime {
        Runtime {
            heap: vec![0; 1024 * 1024],
            pointer: 0,
        }
    }

    fn heap_value(&mut self) -> &mut u8 {
        if self.pointer >= MAX_HEAP_SIZE {
            panic!("Max heap size reached: {}", self.pointer);
        }

        while self.pointer > self.heap.len() - 1 {
            self.heap.push(0);
        }
        &mut self.heap[self.pointer]
    }

    fn heap_value_at(&mut self, pointer: isize) -> &mut u8 {
        let pointer = pointer.max(0) as usize;

        if pointer >= MAX_HEAP_SIZE {
            panic!("Max heap size reached: {}", self.pointer);
        }

        while pointer > self.heap.len() - 1 {
            self.heap.push(0);
        }
        &mut self.heap[pointer]
    }

    fn heap_value_at_offset(&mut self, ptr_offset: isize) -> &mut u8 {
        let pointer = self.pointer as isize + ptr_offset;

        let pointer = pointer.max(0) as usize;

        if pointer >= MAX_HEAP_SIZE {
            panic!("Max heap size reached: {}", self.pointer);
        }

        while pointer > self.heap.len() - 1 {
            self.heap.push(0);
        }

        &mut self.heap[pointer]
    }

    fn inc_ptr(&mut self, count: usize) {
        self.pointer = self.pointer.wrapping_add(count)
    }

    fn dec_ptr(&mut self, count: usize) {
        self.pointer = self.pointer.wrapping_sub(count)
    }

    fn inc(&mut self, offset: isize, count: u8) {
        let value = self.heap_value_at_offset(offset);
        *value = value.wrapping_add(count);
    }

    fn set(&mut self, offset: isize, value: u8) {
        *self.heap_value_at_offset(offset) = value;
    }

    fn dec(&mut self, offset: isize, count: u8) {
        let value = self.heap_value_at_offset(offset);
        *value = value.wrapping_sub(count);
    }

    fn get_char(&mut self) {
        let mut buf = [0];

        std::io::stdin().read_exact(&mut buf).unwrap();

        *self.heap_value() = buf[0];
    }

    fn add(&mut self, src_offset: isize, dest_offset: isize, multi: u8) {
        let source = *self.heap_value_at_offset(src_offset);
        let target = self.heap_value_at_offset(dest_offset);
        *target = target.wrapping_add(source.wrapping_mul(multi));
        *self.heap_value_at_offset(src_offset) = 0;
    }

    fn nz_add(&mut self, src_offset: isize, dest_offset: isize, multi: u8) {
        let source = *self.heap_value_at_offset(src_offset);
        let target = self.heap_value_at_offset(dest_offset);
        *target = target.wrapping_add(source.wrapping_mul(multi));
    }

    fn c_add(&mut self, src_offset: isize, dest_offset: isize, value: u8) {
        let target = self.heap_value_at_offset(dest_offset);
        *target = target.wrapping_add(value);
        *self.heap_value_at_offset(src_offset) = 0;
    }

    fn nz_c_add(&mut self, src_offset: isize, dest_offset: isize, value: u8) {
        let target = self.heap_value_at_offset(dest_offset);
        *target = target.wrapping_add(value);
    }

    fn sub(&mut self, src_offset: isize, dest_offset: isize, multi: u8) {
        let source = *self.heap_value_at_offset(src_offset);
        let target = self.heap_value_at_offset(dest_offset);
        *target = target.wrapping_sub(source.wrapping_mul(multi));
        *self.heap_value_at_offset(src_offset) = 0;
    }

    fn nz_sub(&mut self, src_offset: isize, dest_offset: isize, multi: u8) {
        let source = *self.heap_value_at_offset(src_offset);
        let target = self.heap_value_at_offset(dest_offset);
        *target = target.wrapping_sub(source.wrapping_mul(multi));
    }

    fn c_sub(&mut self, src_offset: isize, dest_offset: isize, value: u8) {
        let target = self.heap_value_at_offset(dest_offset);
        *target = target.wrapping_sub(value);
        *self.heap_value_at_offset(src_offset) = 0;
    }

    fn nz_c_sub(&mut self, src_offset: isize, dest_offset: isize, value: u8) {
        let target = self.heap_value_at_offset(dest_offset);
        *target = target.wrapping_sub(value);
    }

    fn mul(&mut self, src_offset: isize, dest_offset: isize, multi: u8) {
        let source = *self.heap_value_at_offset(src_offset);
        let target = self.heap_value_at_offset(dest_offset);
        *target = source.wrapping_mul(multi);
        *self.heap_value_at_offset(src_offset) = 0;
    }

    fn nz_mul(&mut self, src_offset: isize, dest_offset: isize, multi: u8) {
        let source = *self.heap_value_at_offset(src_offset);
        let target = self.heap_value_at_offset(dest_offset);
        *target = source.wrapping_mul(multi);
    }

    fn move(&mut self, src_offset: isize, dest_offset: isize) {
        let source = *self.heap_value_at_offset(src_offset);
        let target = self.heap_value_at_offset(dest_offset);
        *target = source;
        *self.heap_value_at_offset(src_offset) = 0;
    }

    fn copy(&mut self, src_offset: isize, dest_offset: isize) {
        let source = *self.heap_value_at_offset(src_offset);
        let target = self.heap_value_at_offset(dest_offset);
        *target = source;
    }

    fn search_zero(&mut self, step: isize) {
        let mut pointer = self.pointer as isize;

        loop {
            let value = self.heap_value_at(pointer);

            if *value == 0 {
                break;
            }

            pointer += step;
        }

        self.pointer = pointer as usize;
    }

    fn put_char(&mut self, offset: isize) {
        let ch = *self.heap_value_at_offset(offset);

        if ch.is_ascii() {
            write!(std::io::stdout(), "{}", ch as char)
        } else {
            write!(std::io::stdout(), "\\0x{:x}", ch)
        }.unwrap()
    }
}

fn main() {
    let mut rt = Runtime::new();

    {{CODE}}
}
