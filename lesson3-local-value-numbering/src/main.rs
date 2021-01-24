use std::collections::HashMap;
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
        for block in blocks.iter_mut() {
            local_value_numbering(block);
        }
        eliminate_dead_code(&mut blocks);
        func.instrs = flatten_blocks(blocks);
    }

    output_program(&program);
    Ok(())
}

enum LvnValue {
    Const(Literal),
    Op(ValueOps, Vec<usize>),
}

struct LvnCtx {
    values: Vec<(String, usize, LvnValue, String)>,
}

impl LvnCtx {
    /// Err means it already existed, Ok means it is new
    fn upsert(&mut self, i: &mut Instruction) -> Result<String, String> {
        match i {
            Instruction::Constant { dest, value, .. } => {
                let value = LvnValue::Const(value.clone());
                if let Some((_, _, _, c)) = self.find(&value) {
                    Err(c.clone())
                } else {
                    let idx = self.values.len();
                    self.values.push((dest.clone(), idx, value, dest.clone()));
                    Ok(dest.clone())
                }
            }
            Instruction::Value { op, args, dest, .. } => {
                // x: add a b
                let value = self.value_op(op.clone(), args.clone());
                if let Some((_, _, _, c)) = self.find(&value) {
                    Err(c.clone())
                } else {
                    let idx = self.values.len();
                    self.values.push((dest.clone(), idx, value, dest.clone()));
                    Ok(dest.clone())
                }
            }
            Instruction::Effect { .. } => unreachable!(),
        }
    }

    fn value_op(&self, op: ValueOps, args: Vec<String>) -> LvnValue {
        let mut value_args = Vec::new();
        args.iter().for_each(|a| value_args.push(self.resolve(a)));
        LvnValue::Op(op, value_args)
    }

    fn find(&self, val: &LvnValue) -> Option<&(String, usize, LvnValue, String)> {
        self.values.iter().find(|(_, v, _, _)| v == val)
    }

    fn resolve(&self, variable: &str) -> usize {
        *self
            .values
            .iter()
            .find(|(v, _, _, _)| v == variable)
            .map(|(_, n, _, _)| n)
            .expect("Variable not found")
    }
}

impl Default for LvnCtx {
    fn default() -> Self {
        Self { values: Vec::new() }
    }
}

fn local_value_numbering(block: &mut BasicBlock) {}

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
    let mut used = HashMap::new();
    for (block_idx, block) in blocks.iter().enumerate().rev() {
        for (instr_idx, instr) in block.instrs.iter().enumerate().rev() {
            match instr {
                Instruction::Constant { dest, .. } => {
                    if let Some(used_block_idx) = used.get(dest) {
                        // if it is used in another block, we need to retain it
                        if *used_block_idx == block_idx {
                            used.remove(dest); // eliminates prev. declarations of dest
                        }
                    } else {
                        remove.push((block_idx, instr_idx)); // never used, safely remove
                    }
                }
                Instruction::Value { dest, args, .. } => {
                    if used.contains_key(dest) {
                        // dest is found, all args are used as well
                        for arg in args {
                            used.insert(arg.clone(), block_idx);
                        }
                    } else {
                        // dest is not used
                        remove.push((block_idx, instr_idx));
                    }
                }
                Instruction::Effect { args, .. } => {
                    for arg in args {
                        used.insert(arg.clone(), block_idx);
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
