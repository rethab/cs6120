use std::io;
use std::mem;

use bril_rs::*;

#[derive(Debug, Clone)]
struct BasicBlock {
    label: Option<String>,
    instrs: Vec<Instruction>,
}

impl BasicBlock {
    fn new(label: Option<String>) -> Self {
        Self {
            label,
            instrs: Vec::new(),
        }
    }

    fn push(&mut self, i: Instruction) {
        self.instrs.push(i)
    }

    fn is_empty(&self) -> bool {
        let has_label = self.label.as_ref().map(|l| l.starts_with("_l")) == Some(true);
        self.instrs.is_empty() && has_label
    }
}

fn main() -> io::Result<()> {
    let mut program = load_program();

    for func in program.functions.iter_mut() {
        let mut blocks = create_blocks(func.clone());
        eliminate_dead_code(&mut blocks);
        func.instrs = flatten_blocks(blocks);
    }

    output_program(&program);
    Ok(())
}

fn flatten_blocks(blocks: Vec<BasicBlock>) -> Vec<Code> {
    let mut code = Vec::new();
    for block in blocks {
        if let Some(label) = block.label {
            code.push(Code::Label { label });
        }
        for instr in block.instrs {
            code.push(Code::Instruction(instr))
        }
    }
    code
}

fn eliminate_dead_code(blocks: &mut Vec<BasicBlock>) {
    let mut remove = Vec::new();
    let mut used = Vec::new();
    for (block_idx, block) in blocks.iter().enumerate().rev() {
        for (instr_idx, instr) in block.instrs.iter().enumerate().rev() {
            match instr {
                Instruction::Constant { dest, .. } => {
                    if let Some(pos) = index_of(&used, dest) {
                        used.remove(pos); // eliminates prev. declarations of dest
                    } else {
                        remove.push((block_idx, instr_idx)); // never used, safely remove
                    }
                }
                Instruction::Value { dest, args, .. } => {
                    if index_of(&used, dest).is_some() {
                        // dest is found, all args are used as well
                        for arg in args {
                            used.push(arg.clone());
                        }
                    } else {
                        // dest is not used
                        remove.push((block_idx, instr_idx));
                    }
                }
                Instruction::Effect { args, .. } => {
                    for arg in args {
                        used.push(arg.clone());
                    }
                }
            }
        }
    }

    // note that we need to remove elements with the biggest index first
    // this works because we iterate in reverse above
    for (block_idx, instr_idx) in remove {
        blocks[block_idx].instrs.remove(instr_idx);
    }
}

#[inline]
fn index_of<T: PartialEq>(v: &[T], n: &T) -> Option<usize> {
    v.iter().position(|x| x == n)
}

fn create_blocks(f: Function) -> Vec<BasicBlock> {
    let mut blocks = Vec::new();

    let mut current = BasicBlock::new(None);
    for i in f.instrs {
        match i {
            Code::Instruction(instr) if !is_terminator(&instr) => current.push(instr),
            Code::Instruction(instr) => {
                current.push(instr);
                let next_block = BasicBlock::new(None);
                blocks.push(mem::replace(&mut current, next_block))
            }
            Code::Label { label } if current.is_empty() => {
                current.label = Some(label);
            }
            Code::Label { label } => {
                let next_block = BasicBlock::new(Some(label));
                blocks.push(mem::replace(&mut current, next_block));
            }
        }
    }
    if !current.is_empty() {
        blocks.push(current)
    }

    blocks
}

fn is_terminator(i: &Instruction) -> bool {
    use Instruction::*;

    match i {
        Effect { op, .. } => is_terminator_op(op),
        Constant { .. } => false,
        Value { .. } => false,
    }
}

fn is_terminator_op(op: &EffectOps) -> bool {
    use EffectOps::*;
    matches!(op, Jump | Branch | Return)
}
