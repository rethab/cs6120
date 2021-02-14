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
        let madeup_label = self.label.as_ref().map(|l| l.starts_with("_l")) == Some(true);
        self.instrs.is_empty() && (self.label.is_none() || madeup_label)
    }
}

fn create_blocks(f: Function) -> Vec<BasicBlock> {
    let mut blocks = Vec::new();

    let mut current = BasicBlock::new(Some(f.name));
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

type Cfg = Vec<(String, Vec<String>)>;

fn create_cfg(blocks: Vec<BasicBlock>) -> Cfg {
    use EffectOps::*;
    use Instruction::*;
    let mut edges: Cfg = Vec::new();

    let n_blocks = blocks.len();
    for i in 0..n_blocks {
        let BasicBlock {
            label: label_tmp,
            instrs,
        } = blocks[i].clone();
        let label = label_tmp.unwrap_or_else(|| panic!("Missing label: {:?}", instrs));
        let last = instrs.last().expect("no last: not handled yet");

        let successors = match last {
            Effect {
                op: Jump, labels, ..
            } => labels.clone(),
            Effect {
                op: Branch, labels, ..
            } => labels.to_vec(),
            Effect { op, labels, .. } if is_terminator_op(op) => labels.clone(),
            _ if i < n_blocks - 1 => {
                let next_label = blocks[i + 1].clone().label;
                vec![next_label.expect("next has no label")]
            }
            _ => vec![],
        };

        edges.push((label, successors))
    }
    edges
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

fn main() -> io::Result<()> {
    let program = load_program();

    for func in program.functions {
        let blocks = create_blocks(func.clone());
        let cfg = create_cfg(blocks);
        println!("Cfg: {:?}", cfg);
    }

    Ok(())
}
