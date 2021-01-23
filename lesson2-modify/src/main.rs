use std::io;

use bril_rs::*;

fn main() -> io::Result<()> {
    let mut program = load_program();
    print_for_conditions(&mut program);
    output_program(&program);
    Ok(())
}

/// Adds print statements to conditions in for loops. Since there are
/// no for loops in bril, it tries to detect them by checking if the label
/// is named something ike 'for.cond'.
fn print_for_conditions(p: &mut Program) {
    for fun in p.functions.iter_mut() {
        let mut prints = Vec::new();
        collect_locations(&fun.instrs, &mut prints);
        for (idx, (orig_print_idx, code)) in prints.into_iter().enumerate() {
            let instr_idx = idx + orig_print_idx;
            fun.instrs.insert(instr_idx, code);
        }
    }
}

fn collect_locations(instrs: &[Code], prints: &mut Vec<(usize, Code)>) {
    let mut in_cond = false;
    for (idx, instr) in instrs.iter().enumerate() {
        match instr {
            Code::Label { label } if label.contains("for.cond") => {
                in_cond = true;
            }
            Code::Label { .. } => in_cond = false,
            Code::Instruction(Instruction::Effect {
                op: EffectOps::Branch,
                args,
                ..
            }) if in_cond => {
                let variable = args.first().cloned().unwrap();
                prints.push((idx, create_print(variable)));
            }
            Code::Instruction(_) => {}
        }
    }
}

fn create_print(variable: String) -> Code {
    Code::Instruction(Instruction::Effect {
        op: EffectOps::Print,
        args: vec![variable],
        funcs: vec![],
        labels: vec![],
    })
}
