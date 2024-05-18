use wasm_bindgen::prelude::*;

#[wasm_bindgen]
struct Code {
    report: customasm::diagn::Report,
    fileserver: customasm::util::FileServerMock,
    opts: customasm::asm::AssemblyOptions,
    result: customasm::asm::AssemblyResult,
}

#[wasm_bindgen]
impl Code {
    #[wasm_bindgen(constructor)]
    pub fn new(src: &str) -> Self {
        let virtual_filename = "program.asm";

        let mut report = customasm::diagn::Report::new();
        let mut fileserver = customasm::util::FileServerMock::new();
        fileserver.add("icmc.asm", include_str!("./icmc.asm"));
        fileserver.add("giroto.asm", include_str!("./giroto.asm"));
        fileserver.add(virtual_filename, src);

        let opts = customasm::asm::AssemblyOptions::new();

        let result = customasm::asm::assemble(
            &mut report,
            &opts,
            &mut fileserver,
            &["icmc.asm", virtual_filename],
        );

        Code {
            report,
            fileserver,
            opts,
            result,
        }
    }

    pub fn symbols(&self) -> Result<String, String> {
        if self.result.error {
            let mut vec = Vec::new();
            self.report.print_all(&mut vec, &self.fileserver, true);
            return Err(String::from_utf8(vec).unwrap());
        }

        let decls = self.result.decls.as_ref().unwrap();
        let defs = self.result.defs.as_ref().unwrap();

        Ok(decls.symbols.format_default(decls, defs))
    }

    pub fn binary(&self) -> Result<Vec<u16>, String> {
        if self.result.error {
            let mut vec = Vec::new();
            self.report.print_all(&mut vec, &self.fileserver, true);
            return Err(String::from_utf8(vec).unwrap());
        }

        let Some(ref output) = self.result.output else {
            unreachable!()
        };

        Ok(output
            .format_binary()
            .into_iter()
            .array_chunks()
            .map(|bytes| u16::from_be_bytes(bytes))
            .collect())
    }

    pub fn mif(&self) -> Result<String, String> {
        if self.result.error {
            let mut vec = Vec::new();
            self.report.print_all(&mut vec, &self.fileserver, true);
            return Err(String::from_utf8(vec).unwrap());
        }

        let decls = self.result.decls.as_ref().unwrap();
        let defs = self.result.defs.as_ref().unwrap();

        let Some(ref output) = self.result.output else {
            unreachable!()
        };

        Ok(output.format_mif())
    }
}
