use nom::{
    branch::alt,
    bytes::complete::{is_a, is_not, tag, take_until, take_while},
    character::complete::{char, space1},
    combinator::map,
    multi::{many0, many0_count, separated_list1},
    sequence::delimited,
    IResult,
};
use wasm_bindgen::prelude::*;

#[derive(Debug)]
struct Mif {
    address_radix: Radix,
    data_radix: Radix,
    depth: usize,
    width: usize,
    chunks: Vec<usize>,
}

#[derive(Default, Debug)]
struct MifBuilder {
    address_radix: Option<Radix>,
    data_radix: Option<Radix>,
    depth: Option<usize>,
    width: Option<usize>,
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
struct Address {
    from: usize,
    to: usize,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Radix {
    Uns,
    Bin,
    Oct,
    Hex,
}

impl Radix {
    pub fn radix(&self) -> u32 {
        match self {
            Self::Uns => 10,
            Self::Bin => 2,
            Self::Oct => 8,
            Self::Hex => 16,
        }
    }

    pub fn digits(&self) -> &'static str {
        &"0123456789abcdefABCDEF"[0..match self {
            Self::Uns => 10,
            Self::Bin => 2,
            Self::Oct => 8,
            Self::Hex => 22,
        }]
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Element {
    DataRadix(Radix),
    AddressRadix(Radix),
    Width(usize),
    Depth(usize),
    Comment,
    Data(Address, Vec<usize>),
}

fn multiline_comment(input: &str) -> IResult<&str, Element> {
    let (input, _) = take_while(char::is_whitespace)(input)?;
    let (input, _) = delimited(char('%'), is_not("%"), char('%'))(input)?;
    let (input, _) = take_while(char::is_whitespace)(input)?;

    Ok((input, Element::Comment))
}

fn singleline_comment(input: &str) -> IResult<&str, Element> {
    let (input, _) = take_while(char::is_whitespace)(input)?;
    let (input, _) = delimited(tag("--"), is_not("\n"), char('\n'))(input)?;

    Ok((input, Element::Comment))
}

fn radix(input: &str) -> IResult<&str, Radix> {
    map(
        alt((tag("UNS"), tag("BIN"), tag("HEX"), tag("OCT"))),
        |s: &str| match s {
            "UNS" => Radix::Uns,
            "BIN" => Radix::Bin,
            "OCT" => Radix::Oct,
            "HEX" => Radix::Hex,
            _ => unreachable!(),
        },
    )(input)
}

fn number<'a>(input: &'a str, radix: Radix) -> IResult<&'a str, usize> {
    let (input, number) = is_a(radix.digits())(input)?;
    let number = usize::from_str_radix(number, radix.radix()).unwrap();

    Ok((input, number))
}

fn numeric_attribute<'a>(input: &'a str, name: &str) -> IResult<&'a str, usize> {
    let (input, _) = take_while(char::is_whitespace)(input)?;
    let (input, _) = tag(name)(input)?;
    let (input, _) = take_while(char::is_whitespace)(input)?;
    let (input, _) = tag("=")(input)?;
    let (input, _) = take_while(char::is_whitespace)(input)?;
    let (input, number) = take_while(|c: char| c.is_ascii_digit())(input)?;
    let (input, _) = take_while(char::is_whitespace)(input)?;
    let (input, _) = tag(";")(input)?;

    Ok((input, number.parse::<usize>().unwrap()))
}

fn radix_attribute<'a>(input: &'a str, name: &str) -> IResult<&'a str, Radix> {
    let (input, _) = take_while(char::is_whitespace)(input)?;
    let (input, _) = tag(name)(input)?;
    let (input, _) = take_while(char::is_whitespace)(input)?;
    let (input, _) = tag("=")(input)?;
    let (input, _) = take_while(char::is_whitespace)(input)?;
    let (input, radix) = radix(input)?;
    let (input, _) = take_while(char::is_whitespace)(input)?;
    let (input, _) = tag(";")(input)?;

    Ok((input, radix))
}

fn width(input: &str) -> IResult<&str, Element> {
    let (input, value) = numeric_attribute(input, "WIDTH")?;
    Ok((input, Element::Width(value)))
}

fn depth(input: &str) -> IResult<&str, Element> {
    let (input, value) = numeric_attribute(input, "DEPTH")?;
    Ok((input, Element::Depth(value)))
}

fn address_radix(input: &str) -> IResult<&str, Element> {
    let (input, value) = radix_attribute(input, "ADDRESS_RADIX")?;
    Ok((input, Element::AddressRadix(value)))
}

fn data_radix(input: &str) -> IResult<&str, Element> {
    let (input, value) = radix_attribute(input, "DATA_RADIX")?;
    Ok((input, Element::DataRadix(value)))
}

fn address_range<'a>(input: &'a str, radix: Radix) -> IResult<&'a str, Address> {
    let (input, _) = take_while(char::is_whitespace)(input)?;
    let (input, address) = delimited(
        char('['),
        |input: &'a str| {
            let (input, _) = take_while(char::is_whitespace)(input)?;
            let (input, from) = number(input, radix)?;
            let (input, _) = take_while(char::is_whitespace)(input)?;
            let (input, _) = tag("..")(input)?;
            let (input, _) = take_while(char::is_whitespace)(input)?;
            let (input, to) = number(input, radix)?;
            let (input, _) = take_while(char::is_whitespace)(input)?;

            Ok((input, Address { from, to }))
        },
        char(']'),
    )(input)?;

    Ok((input, address))
}

fn address_number<'a>(input: &'a str, radix: Radix) -> IResult<&'a str, Address> {
    let (input, _) = take_while(char::is_whitespace)(input)?;
    let (input, num) = number(input, radix)?;

    Ok((input, Address { from: num, to: num }))
}

fn data<'a>(input: &'a str, mif: &mut Mif) -> IResult<&'a str, Element> {
    let (input, _) = take_while(char::is_whitespace)(input)?;
    let (input, address) = alt((
        |input| address_range(input, mif.address_radix),
        |input| address_number(input, mif.address_radix),
    ))(input)?;
    let (input, _) = take_while(char::is_whitespace)(input)?;
    let (input, _) = tag(":")(input)?;
    let (input, _) = take_while(char::is_whitespace)(input)?;
    let (input, values) = separated_list1(space1, |input| number(input, mif.data_radix))(input)?;
    let (input, _) = tag(";")(input)?;
    let (input, _) = take_while(char::is_whitespace)(input)?;

    let mask = (1usize << mif.width as u32).wrapping_sub(1);
    if address.from == address.to {
        for (i, mut value) in (address.from..).zip(values.into_iter()) {
            let offset = i * mif.width;
            let index = offset / usize::BITS as usize;
            let shift = offset % usize::BITS as usize;

            value &= mask;

            let (lower_chunk, overflow) = value.overflowing_shl(shift as u32);
            mif.chunks[index] |= lower_chunk;

            if overflow {
                mif.chunks[index + 1] |= value >> (usize::BITS as usize - mask);
            }
        }
    } else {
        for (i, mut value) in (address.from..=address.to).zip(values.into_iter().cycle()) {
            let offset = i * mif.width;
            let index = offset / usize::BITS as usize;
            let shift = offset % usize::BITS as usize;

            value &= mask;

            let (lower_chunk, overflow) = value.overflowing_shl(shift as u32);
            mif.chunks[index] |= lower_chunk;

            if overflow {
                mif.chunks[index + 1] |= value >> (usize::BITS as usize - mask);
            }
        }
    }

    Ok((input, Element::Comment))
}

fn content<'a>(input: &'a str, mif: &mut Mif) -> IResult<&'a str, ()> {
    let foo = |input: &'a str| -> IResult<&'a str, Element> { data(input, mif) };

    let (input, _) = take_while(char::is_whitespace)(input)?;
    let (input, _) = tag("CONTENT")(input)?;
    let (input, _) = many0_count(alt((singleline_comment, multiline_comment)))(input)?;
    let (input, _) = take_while(char::is_whitespace)(input)?;
    let (input, _) = delimited(
        tag("BEGIN"),
        many0(alt((foo, singleline_comment, multiline_comment))),
        tag("END"),
    )(input)?;
    let (input, _) = take_while(char::is_whitespace)(input)?;
    let (input, _) = tag(";")(input)?;

    Ok((input, ()))
}

#[wasm_bindgen(js_name = "parseMif")]
pub fn parse_mif(input: &str) -> Option<Vec<u8>> {
    let (input, elements) = many0(alt((
        width,
        depth,
        address_radix,
        data_radix,
        multiline_comment,
        singleline_comment,
    )))(input)
    .ok()?;

    let result = elements
        .into_iter()
        .fold(MifBuilder::default(), |mut acc, element| {
            match element {
                Element::DataRadix(radix) => acc.data_radix = Some(radix),
                Element::AddressRadix(radix) => acc.address_radix = Some(radix),
                Element::Width(width) => acc.width = Some(width),
                Element::Depth(depth) => acc.depth = Some(depth),
                _ => (),
            }

            acc
        });

    let address_radix = result.address_radix.expect("Missing ADDRESS_RADIX");
    let data_radix = result.data_radix.expect("Missing DATA_RADIX");
    let width = result.width.expect("Missing WIDTH");
    let depth = result.depth.expect("Missing DEPTH");
    let size = width * depth / usize::BITS as usize;

    let mut mif = Mif {
        address_radix,
        data_radix,
        width,
        depth,
        chunks: Vec::with_capacity(size),
    };

    mif.chunks.resize(size, 0);

    let (input, content) = content(input, &mut mif).ok()?;

    Some(
        mif.chunks
            .iter()
            .flat_map(|byte| byte.to_ne_bytes())
            .collect(),
    )
}

mod test {
    use super::*;

    #[test]
    pub fn widths() {
        assert_eq!(width("WIDTH=299;"), Ok(("", Element::Width(299))));
        assert_eq!(width("WIDTH =    100;"), Ok(("", Element::Width(100))));
        assert_eq!(width("WIDTH   =\t92;"), Ok(("", Element::Width(92))));
        assert_eq!(width("WIDTH=9\t;"), Ok(("", Element::Width(9))));
        assert_eq!(width("WIDTH=00299;"), Ok(("", Element::Width(299))));
    }

    #[test]
    pub fn depths() {
        assert_eq!(depth("DEPTH=299;"), Ok(("", Element::Depth(299))));
        assert_eq!(depth("DEPTH =    100;"), Ok(("", Element::Depth(100))));
        assert_eq!(depth("DEPTH   =\t92;"), Ok(("", Element::Depth(92))));
        assert_eq!(depth("DEPTH=9\t;"), Ok(("", Element::Depth(9))));
        assert_eq!(depth("DEPTH=00299;"), Ok(("", Element::Depth(299))));
    }

    #[test]
    pub fn data_radixes() {
        assert_eq!(
            data_radix("DATA_RADIX=UNS;"),
            Ok(("", Element::DataRadix(Radix::Uns)))
        );
        assert_eq!(
            data_radix("DATA_RADIX =    OCT;"),
            Ok(("", Element::DataRadix(Radix::Oct)))
        );
        assert_eq!(
            data_radix("DATA_RADIX   =\tBIN;"),
            Ok(("", Element::DataRadix(Radix::Bin)))
        );
        assert_eq!(
            data_radix("\tDATA_RADIX=HEX\t;"),
            Ok(("", Element::DataRadix(Radix::Hex)))
        );
        assert_eq!(
            data_radix("\nDATA_RADIX=OCT;"),
            Ok(("", Element::DataRadix(Radix::Oct)))
        );
    }

    #[test]
    pub fn address_radixes() {
        assert_eq!(
            address_radix("ADDRESS_RADIX=UNS;"),
            Ok(("", Element::AddressRadix(Radix::Uns)))
        );
        assert_eq!(
            address_radix("ADDRESS_RADIX =    OCT;"),
            Ok(("", Element::AddressRadix(Radix::Oct)))
        );
        assert_eq!(
            address_radix("ADDRESS_RADIX   =\tBIN;"),
            Ok(("", Element::AddressRadix(Radix::Bin)))
        );
        assert_eq!(
            address_radix("\tADDRESS_RADIX=HEX\t;"),
            Ok(("", Element::AddressRadix(Radix::Hex)))
        );
        assert_eq!(
            address_radix("\nADDRESS_RADIX=OCT;"),
            Ok(("", Element::AddressRadix(Radix::Oct)))
        );
    }

    #[test]
    pub fn singleline_comments() {
        assert_eq!(
            singleline_comment("--foo afjsdklfj\nX"),
            Ok(("X", Element::Comment))
        );
        assert_eq!(
            singleline_comment("  --foo afjsdklfj\nX"),
            Ok(("X", Element::Comment))
        );
    }

    #[test]
    pub fn multiline_comments() {
        assert_eq!(
            multiline_comment("%foo afjsdklfj\njkasdlfjkla\nasdjfl\tkasd%X"),
            Ok(("X", Element::Comment))
        );
        assert_eq!(
            multiline_comment("  %foo afjsdklfj\njkasdlfjkla\nasdjfl\tkasd%X"),
            Ok(("X", Element::Comment))
        );
    }

    #[test]
    pub fn numbers() {
        assert_eq!(number("1000", Radix::Bin), Ok(("", 0b1000)));
        assert_eq!(number("1000", Radix::Oct), Ok(("", 0o1000)));
        assert_eq!(number("1000", Radix::Uns), Ok(("", 1000)));
        assert_eq!(number("1000", Radix::Hex), Ok(("", 0x1000)));
        assert_eq!(number("fFdD", Radix::Hex), Ok(("", 0xFFDD)));
    }

    #[test]
    pub fn mifs() {}
}
