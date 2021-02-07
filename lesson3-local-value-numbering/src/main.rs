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
    Value(ValueOps, Vec<usize>),
    Effect(EffectOps, Vec<usize>),
}

impl LvnValue {
    fn canonicalize(self) -> Self {
        match self {
            c @ LvnValue::Const(_) => c,
            e @ LvnValue::Effect(_, _) => e,
            LvnValue::Value(op, mut args) => {
                if LvnValue::is_commutative(&op) {
                    args.sort_unstable();
                }
                LvnValue::Value(op, args)
            }
        }
    }

    fn is_commutative(op: &ValueOps) -> bool {
        use ValueOps::*;
        matches!(op, Add | Mul | And | Eq | Or | Fadd | Fmul | Feq)
    }
}

type LvnRow = (Vec<String>, usize, LvnValue, String);
struct LvnCtx {
    values: Vec<LvnRow>,
}

impl LvnCtx {
    fn create_value(&self, instr: &Instruction) -> LvnValue {
        match instr {
            Instruction::Constant { value, .. } => LvnValue::Const(value.clone()),
            Instruction::Value { op, args, .. } => LvnValue::Value(*op, self.resolve(args)),
            Instruction::Effect { op, args, .. } => LvnValue::Effect(*op, self.resolve(args)),
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
            .map(|n| self.unresolve_number(*n).3.clone())
            .collect()
    }

    fn unresolve_numbers(&self, numbers: &[usize]) -> Vec<&LvnRow> {
        numbers.iter().map(|n| self.unresolve_number(*n)).collect()
    }

    fn unresolve_number(&self, number: usize) -> &LvnRow {
        self.values
            .iter()
            .find(|(_, n, _, _)| *n == number)
            .map(|row| {
                if let Some(orig_row) = self.resolve_id(&row.2) {
                    orig_row
                } else {
                    row
                }
            })
            .unwrap_or_else(|| panic!("Number {} not found", number))
    }

    fn resolve_id(&self, value: &LvnValue) -> Option<&LvnRow> {
        if let LvnValue::Value(ValueOps::Id, ns) = value {
            Some(self.unresolve_number(ns[0]))
        } else {
            None
        }
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

    fn insert(&mut self, instr: &Instruction, val: LvnValue) {
        let dest: String = match instr {
            Instruction::Constant { dest, .. } => dest.clone(),
            Instruction::Value { dest, .. } => dest.clone(),
            Instruction::Effect { .. } => return,
        };
        let idx = self.values.len();
        self.values
            .push((vec![dest.clone()], idx, val.canonicalize(), dest));
    }

    fn reconstruct(&self, val: LvnValue, instr: &Instruction) -> Instruction {
        if let LvnValue::Const(lit) = val {
            return match instr {
                i @ Instruction::Constant { .. } => i.clone(),
                Instruction::Value { dest, op_type, .. } => Instruction::Constant {
                    op: ConstOps::Const,
                    dest: dest.clone(),
                    const_type: op_type.clone(),
                    value: lit,
                },
                Instruction::Effect { .. } => panic!("An effect cannot be constant-folded"),
            };
        }

        match instr {
            Instruction::Constant { .. } => {
                panic!("Cannot reconstruct constant instruction if lvn value was not a constant")
            }
            Instruction::Value {
                op,
                dest,
                op_type,
                funcs,
                labels,
                ..
            } => self.reconstruct_value(val, op, dest, op_type, funcs, labels),
            Instruction::Effect {
                op, funcs, labels, ..
            } => match val {
                LvnValue::Const(_) => unreachable!(),
                LvnValue::Value(_, _) => unreachable!(),
                LvnValue::Effect(_, numbers) => {
                    let args = self.unresolve(&numbers);
                    Instruction::Effect {
                        op: *op,
                        args,
                        funcs: funcs.clone(),
                        labels: labels.clone(),
                    }
                }
            },
        }
    }

    fn fold_constants(&self, val: LvnValue) -> LvnValue {
        use ValueOps::*;
        let (op, numbers) = if let LvnValue::Value(op, numbers) = val.clone() {
            (op, numbers)
        } else {
            return val;
        };

        let rows_of_args = self.unresolve_numbers(&numbers);
        match op {
            Add => {
                if let Some(val) = LvnCtx::binary_int(&rows_of_args, |a, b| a + b) {
                    return val;
                }
            }
            Sub => {
                if let Some(val) = LvnCtx::binary_int(&rows_of_args, |a, b| a - b) {
                    return val;
                }
            }
            Mul => {
                if let Some(val) = LvnCtx::binary_int(&rows_of_args, |a, b| a * b) {
                    return val;
                }
            }
            Div => {
                if let Some(val) = LvnCtx::binary_int(&rows_of_args, |a, b| a / b) {
                    return val;
                }
            }
            Id => {
                if let Some(lit) = LvnCtx::one_constant(&rows_of_args) {
                    return LvnValue::Const(lit);
                }
            }
            other => unimplemented!("not supported: {:?}", other),
        };
        val
    }

    fn reconstruct_value(
        &self,
        val: LvnValue,
        op: &ValueOps,
        dest: &str,
        op_type: &Type,
        funcs: &[String],
        labels: &[String],
    ) -> Instruction {
        let numbers = if let LvnValue::Value(_, numbers) = val {
            numbers
        } else {
            panic!("Cannot reconstruct {:?} based on {:?}", op, val);
        };
        let args = self.unresolve(&numbers);
        Instruction::Value {
            op: *op,
            dest: dest.to_owned(),
            op_type: op_type.clone(),
            args,
            funcs: funcs.to_vec(),
            labels: labels.to_vec(),
        }
    }

    fn binary_int<F>(rows_of_args: &[&LvnRow], fun: F) -> Option<LvnValue>
    where
        F: FnOnce(i64, i64) -> i64,
    {
        LvnCtx::two_constants(&rows_of_args).map(|(a, b)| {
            LvnValue::Const(Literal::Int(fun(
                LvnCtx::force_int(a),
                LvnCtx::force_int(b),
            )))
        })
    }

    fn two_constants(rows_of_args: &[&LvnRow]) -> Option<(Literal, Literal)> {
        if rows_of_args.len() == 2 {
            LvnCtx::one_constant(&rows_of_args[0..1])
                .and_then(|a| LvnCtx::one_constant(&rows_of_args[1..2]).map(|b| (a, b)))
        } else {
            panic!("Expected two values in {:?}", rows_of_args);
        }
    }

    fn one_constant(rows_of_args: &[&LvnRow]) -> Option<Literal> {
        if rows_of_args.len() == 1 {
            if let LvnValue::Const(l) = &rows_of_args[0].2 {
                Some(l.clone())
            } else {
                None
            }
        } else {
            panic!("Expected one value in {:?}", rows_of_args);
        }
    }

    fn force_int(l: Literal) -> i64 {
        if let Literal::Int(i) = l {
            i
        } else {
            panic!("Expected int, but was {:?}", l)
        }
    }

    fn find(&self, val: &LvnValue) -> Option<LvnRow> {
        let canon_val = val.clone().canonicalize();
        self.values
            .iter()
            .find(|(_, _, v, _)| *v == canon_val)
            .cloned()
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
        let mut val = ctx.create_value(&instr);
        let new_instr = if let Some((_, number, _, var)) = ctx.find(&val) {
            ctx.add_alias(number, &instr);
            create_id(&instr, var)
        } else {
            val = ctx.fold_constants(val);
            ctx.insert(&instr, val.clone());
            ctx.reconstruct(val, &instr)
        };
        *instr = new_instr;
    }
}

fn create_id(instr: &Instruction, variable: String) -> Instruction {
    match instr {
        Instruction::Constant { .. } => instr.clone(),
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
