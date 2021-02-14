use std::collections::{HashMap, HashSet};
use std::fmt;
use std::mem;

use bril_rs::*;

#[derive(Debug, Clone)]
struct BasicBlock {
    label: Option<String>,
    instrs: Vec<Instruction>,

    // in and out for dataflow
    inc: HashSet<String>,
    out: HashSet<String>,
}

impl BasicBlock {
    fn new(label: Option<String>) -> Self {
        Self {
            label,
            instrs: Vec::new(),

            inc: HashSet::new(),
            out: HashSet::new(),
        }
    }

    fn push(&mut self, i: Instruction) {
        self.instrs.push(i)
    }

    fn force_label(&self) -> String {
        self.clone().label.expect("Missing label")
    }

    fn is_empty(&self) -> bool {
        let madeup_label = self.label.as_ref().map(|l| l.starts_with("_l")) == Some(true);
        self.instrs.is_empty() && (self.label.is_none() || madeup_label)
    }

    fn killed_definitions(&self) -> HashSet<String> {
        let mut killed = HashSet::new();
        for new_def in self.new_definitions() {
            if self.inc.contains(&new_def) {
                killed.insert(new_def);
            }
        }
        killed
    }

    fn new_definitions(&self) -> HashSet<String> {
        let mut new_defs = HashSet::new();
        for instr in self.instrs.iter() {
            let new_var = match instr {
                Instruction::Constant { dest, .. } => dest,
                Instruction::Value { dest, .. } => dest,
                Instruction::Effect { .. } => continue,
            };
            new_defs.insert(new_var.clone());
        }
        new_defs
    }
}

impl fmt::Display for BasicBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, ".{}:", self.label.as_ref().expect("missing label"))?;
        write!(f, "IN: ")?;
        for i in self.inc.iter() {
            write!(f, " {}", i)?;
        }
        writeln!(f, "")?;
        for instr in self.instrs.iter() {
            writeln!(f, "  {}", instr)?;
        }
        write!(f, "OUT: ")?;
        for o in self.out.iter() {
            write!(f, " {}", o)?;
        }
        writeln!(f, "")?;
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
    }

    fn set_blocks(&mut self, blocks: Vec<BasicBlock>) {
        self.blocks = blocks;
    }

    fn out_of_all_predecessors(&self, idx: usize) -> Vec<HashSet<String>> {
        let predecessors = self.get_predecessors(idx);
        let mut outs = Vec::new();
        for pred in predecessors {
            let p = &self.blocks[pred];
            outs.push(p.out.iter().cloned().collect());
        }
        outs
    }

    fn get_predecessors(&self, idx: usize) -> Vec<usize> {
        let mut predecessors = Vec::new();
        let b = &self.blocks[idx];
        for (node, successors) in self.edges.iter() {
            if successors.contains(&b.force_label()) {
                predecessors.push(self.index_of(&node));
            }
        }
        predecessors
    }

    fn index_of(&self, label: &str) -> usize {
        for (i, block) in self.blocks.iter().enumerate() {
            if block.force_label() == label {
                return i;
            }
        }
        panic!("Label {} not found in blocks", label);
    }

    fn set_in(&mut self, idx: usize, inc: HashSet<String>) {
        // hack: the 'in' for the function is set to the args
        if idx != 0 {
            self.blocks[idx].inc = inc;
        }
    }

    fn set_out(&mut self, idx: usize, out: HashSet<String>) -> bool {
        if self.blocks[idx].out == out {
            false
        } else {
            self.blocks[idx].out = out;
            true
        }
    }

    fn all_successors(&self, idx: usize) -> Vec<usize> {
        let b = &self.blocks[idx];
        let mut ids = Vec::new();
        let successors = self
            .edges
            .get(&b.force_label())
            .unwrap_or_else(|| panic!("Missing edges for {}", b.force_label()));
        'outer: for successor in successors {
            for (i, block) in self.blocks.iter().enumerate() {
                if block.force_label() == *successor {
                    ids.push(i);
                    continue 'outer;
                }
            }
            panic!("Successor {} not found", successor);
        }
        ids
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
            writeln!(f, ".{}:", block.force_label())?;
            write!(f, "  IN:")?;
            for i in block.inc.iter() {
                write!(f, " {}", i)?;
            }
            writeln!(f, "")?;
            for instr in block.instrs.iter() {
                writeln!(f, "  {}", instr)?;
            }
            write!(f, "  OUT:")?;
            for o in block.out.iter() {
                write!(f, " {}", o)?;
            }
            writeln!(f, "")?;
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
            ..
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

fn worklist(cfg: &mut Cfg, args: HashSet<String>) {
    cfg.blocks[0].inc = args;

    let mut worklist: Vec<usize> = (0..cfg.blocks.len()).collect();
    while !worklist.is_empty() {
        let idx = worklist.pop().unwrap_or_else(|| unreachable!());
        cfg.set_in(idx, merge(cfg.out_of_all_predecessors(idx)));
        let b = &cfg.blocks[idx];
        let new_out = transfer(&b, &b.inc);
        if cfg.set_out(idx, new_out) {
            worklist.append(&mut cfg.all_successors(idx));
        }
    }
}

fn merge(outs: Vec<HashSet<String>>) -> HashSet<String> {
    let mut res = HashSet::new();
    for out in outs.iter() {
        res = res.union(out).cloned().collect();
    }
    res
}

fn transfer(b: &BasicBlock, ins: &HashSet<String>) -> HashSet<String> {
    let survivors = ins.difference(&b.killed_definitions()).cloned().collect();
    b.new_definitions().union(&survivors).cloned().collect()
}

fn main() {
    let program = load_program();

    for func in program.functions {
        let blocks = create_blocks(func.clone());
        let mut cfg = create_cfg(blocks);
        let args = func.args.iter().map(|a| a.name.clone()).collect();
        worklist(&mut cfg, args);
        println!("{}", cfg);
    }
}
