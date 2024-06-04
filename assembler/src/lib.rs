#![feature(iter_array_chunks)]

use fs::Fs;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct Assembly {
    result: customasm::asm::AssemblyResult,
}

#[wasm_bindgen]
pub struct Assembler {}

#[wasm_bindgen]
impl Assembler {
    pub fn assemble(fs: &mut Fs, filenames: &str) -> Result<Assembly, String> {
        let opts = customasm::asm::AssemblyOptions::new();
        let filenames = filenames.split(':').collect::<Vec<_>>();
        let mut report = customasm::diagn::Report::new();

        let result = customasm::asm::assemble(&mut report, &opts, &mut fs.fileserver, &filenames);

        if result.error {
            let mut vec = Vec::new();
            report.print_all(&mut vec, &fs.fileserver, true);
            return Err(String::from_utf8(vec).unwrap());
        }

        Ok(Assembly { result })
    }
}

#[wasm_bindgen]
impl Assembly {
    pub fn symbols(&self) -> String {
        let decls = self.result.decls.as_ref().unwrap();
        let defs = self.result.defs.as_ref().unwrap();

        decls
            .symbols
            .format(decls, defs, &mut |result, symbol_decl, name, bigint| {
                if let customasm::util::SymbolKind::Label = symbol_decl.kind {
                    result.push_str(name);
                    result.push_str(&format!(" = 0x{:x}\n", bigint));
                }
            })
    }

    pub fn binary(&self) -> Vec<u16> {
        self.result
            .output
            .as_ref()
            .unwrap()
            .format_binary()
            .into_iter()
            .array_chunks()
            .map(|bytes| u16::from_be_bytes(bytes))
            .collect()
    }

    pub fn mif(&self) -> String {
        self.result.output.as_ref().unwrap().format_mif()
    }
}
