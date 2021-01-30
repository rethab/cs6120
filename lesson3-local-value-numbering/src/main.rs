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

#[derive(Clone, PartialEq, Debug)]
enum LvnValue {
    Const(Literal),
    Op(ValueOps, Vec<usize>),
}

type LvnRow = (Vec<String>, usize, LvnValue, String);
struct LvnCtx {
    values: Vec<LvnRow>,
}

impl LvnCtx {
    fn create_value(&self, instr: &Instruction) -> Option<LvnValue> {
        match instr {
            Instruction::Constant { value, .. } => Some(LvnValue::Const(value.clone())),
            Instruction::Value { op, args, .. } => Some(LvnValue::Op(*op, self.resolve(args))),
            Instruction::Effect { .. } => None,
        }
    }

    fn resolve(&self, args: &[String]) -> Vec<usize> {
        let mut numbers = Vec::with_capacity(args.len());
        for arg in args {
            let number = self
                .values
                .iter()
                .find_map(|(vs, n, _, _)| if vs.contains(arg) { Some(*n) } else { None })
                .unwrap_or_else(|| panic!("Variable {} not found!", arg));
            numbers.push(number);
        }
        numbers
    }

    fn unresolve(&self, numbers: &[usize]) -> Vec<String> {
        numbers
            .iter()
            .map(|n| self.unrelsolve_number(*n).3.clone())
            .collect()
    }

    fn unrelsolve_number(&self, number: usize) -> &LvnRow {
        self.values
            .iter()
            .find(|(_, n, _, _)| *n == number)
            .unwrap_or_else(|| panic!("Number {} not found", number))
    }

    fn add_alias(&mut self, number: usize, instr: &Instruction) {
        if let Some((vs, _, _, _)) = self.values.iter_mut().find(|(_, n, _, _)| *n == number) {
            match instr {
                Instruction::Constant { dest, .. } => vs.push(dest.clone()),
                Instruction::Value { dest, .. } => vs.push(dest.clone()),
                Instruction::Effect { .. } => unreachable!(),
            }
        } else {
            panic!("Number {} not found", number);
        }
    }

    fn insert(&mut self, instr: &Instruction, mb_val: Option<LvnValue>) {
        let dest: String = match instr {
            Instruction::Constant { dest, .. } => dest.clone(),
            Instruction::Value { dest, .. } => dest.clone(),
            Instruction::Effect { .. } => return,
        };
        let val = if let Some(v) = mb_val { v } else { return };
        let idx = self.values.len();
        self.values.push((vec![dest.clone()], idx, val, dest));
    }

    fn reconstruct(&self, mb_val: Option<LvnValue>, instr: &Instruction) -> Instruction {
        if let Some(val) = mb_val {
            match instr {
                Instruction::Constant { .. } => instr.clone(),
                Instruction::Value {
                    op,
                    dest,
                    op_type,
                    funcs,
                    labels,
                    ..
                } => match val {
                    LvnValue::Const(_) => unreachable!(),
                    LvnValue::Op(_, numbers) => {
                        let args = self.unresolve(&numbers);
                        Instruction::Value {
                            op: *op,
                            dest: dest.clone(),
                            op_type: op_type.clone(),
                            args,
                            funcs: funcs.clone(),
                            labels: labels.clone(),
                        }
                    }
                },
                Instruction::Effect { .. } => unreachable!(),
            }
        } else {
            instr.clone()
        }
    }

    fn find(&self, mb_val: &Option<LvnValue>) -> Option<LvnRow> {
        let val = if let Some(val) = mb_val {
            val
        } else {
            return None;
        };
        // x:     int = const 4
        // copy1: int = id x
        // copy2: int = id copy1
        // copy3: int = id copy2
        // print copy3
        if let Some(x) = self.values.iter().find(|(_, _, v, _)| v == val) {
            Some(x.clone())
        } else if let LvnValue::Op(_, ns) = val {
            // copy propagation if an id points to another id
            let mut other_val = self.unrelsolve_number(ns[0]).clone();
            if let LvnValue::Op(ValueOps::Id, original_ns) = &other_val.2 {
                let mut original_args = self.unresolve(original_ns);
                other_val.3 = original_args.remove(0);
                Some(other_val)
            } else {
                None
            }
        } else {
            None
        }
    }
}

impl Default for LvnCtx {
    fn default() -> Self {
        Self { values: Vec::new() }
    }
}

fn local_value_numbering(block: &mut BasicBlock) {
    let mut ctx = LvnCtx::default();
    /* x: int = const 4;
     * copy1: int = id x;
     * copy2: int = id copy1;
     * copy3: int = id copy2;
     * print copy3; */
    for instr in &mut block.instrs {
        let val = ctx.create_value(&instr);
        let new_instr = if let Some((_, number, _, var)) = ctx.find(&val) {
            ctx.add_alias(number, &instr);
            create_id(&instr, var)
        } else {
            ctx.insert(&instr, val.clone());
            ctx.reconstruct(val, &instr)
        };
        *instr = new_instr;
    }
}

fn create_id(instr: &Instruction, variable: String) -> Instruction {
    match instr {
        Instruction::Constant { .. } => unreachable!(),
        Instruction::Value {
            dest,
            op_type,
            funcs,
            labels,
            ..
        } => Instruction::Value {
            op: ValueOps::Id,
            dest: dest.clone(),
            op_type: op_type.clone(),
            args: vec![variable],
            funcs: funcs.clone(),
            labels: labels.clone(),
        },
        Instruction::Effect { .. } => unreachable!(),
    }
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
