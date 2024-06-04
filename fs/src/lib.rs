use customasm::util::FileServer;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct Fs {
    #[wasm_bindgen(skip)]
    pub fileserver: customasm::util::FileServerMock,
}

#[wasm_bindgen]
impl Fs {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self {
            fileserver: customasm::util::FileServerMock::new(),
        }
    }

    #[wasm_bindgen(js_name = "writeFile")]
    pub fn write_file(&mut self, filename: &str, contents: &str) {
        self.fileserver.add(filename, contents);
    }

    #[wasm_bindgen(js_name = "readFile")]
    pub fn read_file(&mut self, filename: &str) -> Option<String> {
        let mut report = customasm::diagn::Report::new();
        let file_handle = self
            .fileserver
            .get_handle(&mut report, None, filename)
            .ok()?;
        self.fileserver.get_str(&mut report, None, file_handle).ok()
    }
}
