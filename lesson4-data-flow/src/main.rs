use std::collections::HashMap;
use std::fmt;
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

impl fmt::Display for BasicBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, ".{}:", self.label.as_ref().expect("missing label"))?;
        for instr in self.instrs.iter() {
            writeln!(f, "  {}", instr)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
struct Cfg {
    edges: HashMap<String, Vec<String>>,
    blocks: Vec<BasicBlock>,
}

impl Cfg {
    fn push_edge(&mut self, label: String, successors: Vec<String>) {
        self.edges.insert(label, successors);
        ()
    }

    fn set_blocks(&mut self, blocks: Vec<BasicBlock>) {
        self.blocks = blocks;
    }
}

impl Default for Cfg {
    fn default() -> Self {
        Self {
            edges: HashMap::new(),
            blocks: Vec::new(),
        }
    }
}

impl fmt::Display for Cfg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for block in self.blocks.iter() {
            writeln!(f, ".{}:", block.label.as_ref().expect("Missing label"))?;
            for instr in block.instrs.iter() {
                writeln!(f, "  {}", instr)?;
            }
        }
        Ok(())
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

fn create_cfg(blocks: Vec<BasicBlock>) -> Cfg {
    use Instruction::*;
    let mut cfg: Cfg = Cfg::default();

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
                op: EffectOps::Jump,
                labels,
                ..
            } => labels.clone(),
            Effect {
                op: EffectOps::Branch,
                labels,
                ..
            } => labels.to_vec(),
            Effect { op, labels, .. } if is_terminator_op(op) => labels.clone(),
            _ if i < n_blocks - 1 => {
                let next_label = blocks[i + 1].clone().label;
                vec![next_label.expect("next has no label")]
            }
            _ => vec![],
        };

        cfg.push_edge(label, successors)
    }
    cfg.set_blocks(blocks);
    cfg
}

fn main() -> io::Result<()> {
    let program = load_program();

    for func in program.functions {
        let blocks = create_blocks(func.clone());
        let cfg = create_cfg(blocks);
        println!("{}", cfg);
    }

    Ok(())
}
