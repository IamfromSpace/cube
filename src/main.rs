#![feature(i128_type)]

struct CoordCube {
    corners: i128,
    edges: i128,
}

fn permute(a: i128, b: i128) -> i128 {
    let mut r = 0;
    let mut mask = 31;
    for i in 0..24 {
        let offset = i * 5;
        let to = ((b & mask) >> offset) * 5;
        r = r | (((a & mask) >> offset) << to);
        mask = mask << 5;
    }
    r
}

fn permute_inv(a: i128, b: i128) -> i128 {
    let base_mask = 31;
    let mut mask_1 = base_mask;
    let mut r = 0;
    for i in 0..24 {
        let offset = i * 5;
        let from = ((b & mask_1) >> offset) * 5;
        let mask_2 = base_mask << from;
        r = r | (((a & mask_2) >> from) << offset);
        mask_1 = mask_1 << 5;
    }
    r
}

fn main() {
    let clean =    0b00000000_10111_10110_10101_10100_10011_10010_10001_10000_01111_01110_01101_01100_01011_01010_01001_01000_00111_00110_00101_00100_00011_00010_00001_00000;
    let one_zero = 0b00000000_10111_10110_10101_10100_10011_10010_10001_10000_01111_01110_01101_01100_01011_01010_01001_01000_00111_00110_00101_00100_00011_00010_00000_00001;
    let lots =     0b00000000_10100_10101_10111_10000_01100_10001_10011_00110_00101_01111_01101_01000_01011_01110_00000_01001_00001_00010_01010_10010_00011_00111_00100_10110;
    assert_eq!(clean, permute(clean, clean));
    assert_eq!(clean, permute_inv(clean, clean));
    assert_eq!(one_zero, permute(clean, one_zero));
    assert_eq!(clean, permute(one_zero, one_zero));
    assert_eq!(clean, permute_inv(permute(clean, one_zero), one_zero));
    assert_eq!(clean, permute(permute_inv(clean, one_zero), one_zero));
    assert_eq!(clean, permute_inv(permute(clean, lots), lots));
    assert_eq!(clean, permute(permute_inv(clean, lots), lots));
    let mut cleanc = clean;
    for _ in 0..(10_000_000 as u64) {
        cleanc = permute(cleanc, lots);
    }
    println!("success! {:0<120b}", cleanc);
}
