use wasm_bindgen::prelude::*;

#[wasm_bindgen]
struct Code {
    result: customasm::asm::AssemblyResult,
}

#[wasm_bindgen]
struct Assembler {
    report: customasm::diagn::Report,
    fileserver: customasm::util::FileServerMock,
}

#[wasm_bindgen]
impl Assembler {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self {
            report: customasm::diagn::Report::new(),
            fileserver: customasm::util::FileServerMock::new(),
        }
    }

    pub fn add_std_file(&mut self, filename: &str, contents: &str) {
        self.fileserver.add_std_files(&[(filename, contents)]);
    }

    pub fn add_file(&mut self, filename: &str, contents: &str) {
        self.fileserver.add(filename, contents);
    }

    pub fn assemble(&mut self, filenames: &str) -> Result<Code, String> {
        let opts = customasm::asm::AssemblyOptions::new();
        let filenames = filenames.split(':').collect::<Vec<_>>();

        let result =
            customasm::asm::assemble(&mut self.report, &opts, &mut self.fileserver, &filenames);

        if result.error {
            let mut vec = Vec::new();
            self.report.print_all(&mut vec, &self.fileserver, true);
            return Err(String::from_utf8(vec).unwrap());
        }

        Ok(Code { result })
    }
}

#[wasm_bindgen]
impl Code {
    pub fn symbols(&self) -> String {
        let decls = self.result.decls.as_ref().unwrap();
        let defs = self.result.defs.as_ref().unwrap();

        decls.symbols.format_default(decls, defs)
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
