use fs::Fs;
use icmc_cc::gen_asm::gen_asm;
use icmc_cc::gen_ir::gen_ir;
use icmc_cc::parse::parse;
use icmc_cc::preprocess::Preprocessor;
use icmc_cc::regalloc::alloc_regs;
use icmc_cc::sema::sema;
use icmc_cc::token::tokenize;
use wasm_bindgen::prelude::*;

fn fsread(_: &str) -> Option<String> {
    Some("test".to_string())
}

#[wasm_bindgen]
pub struct Compiler {}

#[wasm_bindgen]
impl Compiler {
    pub fn compile(fs: &mut Fs, filenames: &str) -> Result<String, String> {
        let filenames = filenames.split(':').collect::<Vec<_>>();

        let tokens = tokenize(
            fs.read_file(filenames[0]).unwrap(),
            filenames[0].to_string(),
            &mut Preprocessor::new(Box::new(fsread)),
        );

        let nodes = parse(&tokens);
        let (nodes, globals) = sema(nodes);
        let mut fns = gen_ir(nodes);

        alloc_regs(&mut fns);

        let mut output = Vec::new();
        gen_asm(&mut output, globals, fns);

        Ok(String::from_utf8(output).unwrap())
    }
}
