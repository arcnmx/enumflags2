#[derive(BitFlags, Copy, Clone, Debug, PartialEq, Eq, Hash)]
#[repr(u8)]
enum Test {
    A = 1 << 0,
    B = 1 << 1,
    C = 1 << 2,
    D = 1 << 3,
}

#[derive(BitFlags, Copy, Clone, Debug)]
#[repr(u64)]
enum Test1 {
    A = 1 << 0,
    B = 1 << 1,
    C = 1 << 2,
    D = 1 << 3,
    E = 1 << 34,
}

#[test]
fn test_foo() {
    use enumflags2::*;
    assert_eq!(
        Test::ALL,
        Test::A | Test::B | Test::C | Test::D
    );
    assert_eq!(Test::ALL & Test::A, Test::A);
    assert_eq!(!Test::A, Test::B | Test::C | Test::D);
    assert_eq!((Test::A | Test::C) ^ (Test::C | Test::B), Test::A | Test::B);
    assert_eq!(Test::from_bits_truncate(4), Test::C);
    assert_eq!(Test::from_bits_truncate(5), Test::A | Test::C);
    assert_eq!(
        Test::from_bits_truncate(16),
        Test::EMPTY
    );
    assert_eq!(Test::from_bits_truncate(17), Test::A);
    assert!(Test::from_bits(17).is_err());
    assert_eq!(
        Test::from_bits(15).unwrap(),
        Test::ALL
    );
    {
        let mut b = Test::A | Test::B;
        b.insert(Test::C);
        assert_eq!(b, Test::A | Test::B | Test::C);
    }
    assert!((Test::A | Test::B).intersects(Test::B));
    assert!(!(Test::A | Test::B).intersects(Test::C));
    assert!((Test::A | Test::B | Test::C).contains(Test::A | Test::B));
    assert!(!(Test::A | Test::B | Test::C).contains(Test::A | Test::D));
    assert_eq!(!(Test::A | Test::B), Test::C | Test::D);
    assert_eq!((Test::A | Test::B).bits(), 3);
    assert_eq!((!(Test::A | Test::B)).bits(), 12);
    assert_eq!(Test::ALL.bits(), 15);
    {
        let mut b = Test::A | Test::B | Test::C;
        b.remove(Test::B);
        assert_eq!(b, Test::A | Test::C);
    }
    assert_eq!((Test::A ^ Test::B), Test::A | Test::B);
}

#[test]
fn iterator() {
    use enumflags2::*;

    for &(bitflag, expected) in &[
        (Test::EMPTY, &[][..]),
        (Test::A.into(), &[Test::A]),
        (Test::A | Test::B, &[Test::A, Test::B]),
    ] {
        assert!(bitflag.iter().zip(expected.iter().cloned()).all(|(a, b)| a == b));
    }
}

#[test]
fn assign_ops() {
    let mut x = Test::A | Test::B;
    x |= Test::C;
    assert_eq!(x, Test::A | Test::B | Test::C);

    let mut x = Test::A | Test::B;
    x &= Test::B | Test::C;
    assert_eq!(x, Test::B);

    let mut x = Test::A | Test::B;
    x ^= Test::B | Test::C;
    assert_eq!(x, Test::A | Test::C);
}

#[test]
fn fn_derive() {
    #[derive(BitFlags, Copy, Clone, Debug)]
    #[repr(u8)]
    enum TestFn {
        A = 1 << 0,
    }
}
