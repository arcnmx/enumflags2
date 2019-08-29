use enumflags2::BitFlags;

include!("../common.rs");

#[test]
fn debug_format() {
    use enumflags2::BitFlagExtConst;

    // Assert that our Debug output format meets expectations

    assert_eq!(
        format!("{:?}", Test::ALL),
        "BitFlags<Test>(0b1111, A | B | C | D)"
    );

    assert_eq!(
        format!("{:?}", Test::EMPTY),
        "BitFlags<Test>(0b0)"
    );

    assert_eq!(
        format!("{:04x?}", Test::ALL),
        "BitFlags<Test>(0x0f, A | B | C | D)"
    );

    assert_eq!(
        format!("{:04X?}", Test::ALL),
        "BitFlags<Test>(0x0F, A | B | C | D)"
    );
}

#[test]
fn debug_format_alternate() {
    /// Handle the slight difference in alternate debug output on rustc 1.34.2.
    fn compare(mut actual: String, expected: &str) {
        if actual.ends_with("\n}") && !actual.ends_with(",\n}") {
            actual.replace_range(actual.len()-2.., ",\n}");
        }

        assert_eq!(actual, expected);
    }

    compare(
        format!("{:#010?}", Test::ALL),
"BitFlags<Test> {
    bits: 0b00001111,
    flags: A | B | C | D,
}"
    );

    compare(
        format!("{:#?}", Test::EMPTY),
"BitFlags<Test> {
    bits: 0b0,
}"
    );
}

#[test]
fn format() {
    use enumflags2::BitFlagExtConst;

    // Assert BitFlags<T> impls fmt::{Binary, Octal, LowerHex, UpperHex}

    assert_eq!(
        format!("{:b}", Test::ALL),
        "1111"
    );

    assert_eq!(
        format!("{:o}", Test::ALL),
        "17"
    );

    assert_eq!(
        format!("{:x}", Test::ALL),
        "f"
    );

    assert_eq!(
        format!("{:#04X}", Test::ALL),
        "0x0F"
    );
}

#[test]
fn debug_generic() {
    use enumflags2::{BitFlags, RawBitFlags};

    #[derive(Debug)]
    struct Debug<T: RawBitFlags>(BitFlags<T>);

    let _ = format!("{:?}", Debug(BitFlags::<Test>::all()));
}

#[test]
fn works_in_hashmap() {
    // Assert that BitFlags<T> implements Hash.

    use std::collections::HashMap;
    let _map: HashMap<BitFlags<Test>, u8> = HashMap::new();
}
