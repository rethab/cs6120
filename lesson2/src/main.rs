use std::io;
use std::mem;

use bril_rs as bril;
use bril_rs::{Code, EffectOps, Instruction};

#[derive(Debug, Clone)]
struct BasicBlock {
    label: String,
    instrs: Vec<bril::Instruction>,
}

impl BasicBlock {
    fn new(label: String) -> Self {
        Self {
            label,
            instrs: Vec::new(),
        }
    }

    fn push(&mut self, i: bril::Instruction) {
        self.instrs.push(i)
    }

    fn is_empty(&self) -> bool {
        self.instrs.is_empty() && self.label.starts_with("_l")
    }
}

fn create_blocks(p: bril::Program) -> Vec<BasicBlock> {
    let mut blocks = Vec::new();

    for f in p.functions {
        let mut current = BasicBlock::new(f.name);
        for i in f.instrs {
            match i {
                Code::Instruction(instr) if !is_terminator(&instr) => current.push(instr),
                Code::Instruction(instr) => {
                    current.push(instr);
                    let new_label = format!("_l{}", blocks.len());
                    let next_block = BasicBlock::new(new_label);
                    blocks.push(mem::replace(&mut current, next_block))
                }
                Code::Label { label } if current.is_empty() => {
                    current.label = label;
                }
                Code::Label { label } => {
                    let next_block = BasicBlock::new(label);
                    blocks.push(mem::replace(&mut current, next_block));
                }
            }
        }
        if !current.is_empty() {
            blocks.push(current)
        }
    }

    blocks
}

fn is_terminator(i: &bril::Instruction) -> bool {
    use Instruction::*;

    match i {
        Effect { op, .. } => is_terminator_op(op),
        Constant { .. } => false,
        Value { .. } => false,
    }
}

fn is_terminator_op(op: &bril::EffectOps) -> bool {
    use EffectOps::*;
    matches!(op, Jump | Branch | Return)
}

type Cfg = Vec<(String, Vec<String>)>;

fn create_cfg(blocks: Vec<BasicBlock>) -> Cfg {
    use bril::EffectOps::*;
    use bril::Instruction::*;
    let mut edges: Cfg = Vec::new();

    let n_blocks = blocks.len();
    for i in 0..n_blocks {
        let BasicBlock { label, instrs } = blocks[i].clone();
        let last = instrs.last().expect("no last: not handled yet");

        let successors = match last {
            Effect {
                op: Jump, labels, ..
            } => labels.clone(),
            Effect {
                op: Branch, labels, ..
            } => labels[1..].to_vec(),
            Effect { op, labels, .. } if is_terminator_op(op) => labels.clone(),
            _ if i < n_blocks - 1 => {
                let next_label = blocks[i + 1].clone().label;
                vec![next_label]
            }
            _ => vec![],
        };

        edges.push((label, successors))
    }
    edges
}

fn main() -> io::Result<()> {
    let program = bril::load_program();
    let cfg = create_cfg(create_blocks(program));
    println!("digraph main {{");
    for (label, _) in cfg.iter() {
        println!("  {};", label);
    }
    for (label, successors) in cfg {
        for successor in successors {
            println!("  {} -> {};", label, successor);
        }
    }
    println!("}}");
    Ok(())
}
