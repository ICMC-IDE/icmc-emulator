fn main() {
    let (input, _) = mif::mif("% multiple-line comment\nmultiple-line comment %\n-- single-line comment\nDEPTH = 32; -- The size of memory in words\nWIDTH = 8; -- The size of data in bits\nADDRESS_RADIX = HEX; -- The radix for address values\nDATA_RADIX = BIN; -- The radix for data values\nCONTENT -- start of (address : data pairs)\nBEGIN\n00 : 00000000; -- memory address : data\n01 : 00000001; 02 : 00000010; 03 : 00000011; 04 : 00000100; 05 : 00000101; 06 : 00000110; 07 : 00000111; 08 : 00001000; 09 : 00001001; 0A : 00001010; 0B : 00001011; 0C : 00001100;\nEND;").unwrap();
    dbg!(input);
}
