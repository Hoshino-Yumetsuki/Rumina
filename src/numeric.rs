use malachite_base::num::arithmetic::traits::{Pow, Sign};
use malachite_base::num::conversion::traits::FromStringBase;
use malachite_nz::integer::Integer;
use malachite_q::Rational;
use std::cmp::Ordering;

pub type BigInt = Integer;
pub type BigRational = Rational;

pub trait BigIntExt {
    fn pow_u32(&self, exponent: u32) -> BigInt;
    fn to_i64_checked(&self) -> Option<i64>;
    fn to_f64_checked(&self) -> Option<f64>;
}

impl BigIntExt for BigInt {
    fn pow_u32(&self, exponent: u32) -> BigInt {
        self.pow(u64::from(exponent))
    }

    fn to_i64_checked(&self) -> Option<i64> {
        self.to_string().parse::<i64>().ok()
    }

    fn to_f64_checked(&self) -> Option<f64> {
        self.to_string().parse::<f64>().ok()
    }
}

pub trait BigRationalExt {
    fn numer(&self) -> BigInt;
    fn denom(&self) -> BigInt;
    fn to_f64_checked(&self) -> Option<f64>;
}

impl BigRationalExt for BigRational {
    fn numer(&self) -> BigInt {
        let numerator = BigInt::from(self.to_numerator());
        if self.sign() == Ordering::Less {
            -numerator
        } else {
            numerator
        }
    }

    fn denom(&self) -> BigInt {
        BigInt::from(self.to_denominator())
    }

    fn to_f64_checked(&self) -> Option<f64> {
        let numerator = self.numer().to_f64_checked()?;
        let denominator = self.denom().to_f64_checked()?;
        Some(numerator / denominator)
    }
}

pub fn bigint_parse_bytes(bytes: &[u8], radix: u8) -> Option<BigInt> {
    std::str::from_utf8(bytes)
        .ok()
        .and_then(|digits| BigInt::from_string_base(radix, digits))
}

pub fn rational_new(numerator: BigInt, denominator: BigInt) -> BigRational {
    BigRational::from_integers(numerator, denominator)
}

pub fn rational_from_integer(integer: BigInt) -> BigRational {
    BigRational::from(integer)
}

pub fn rational_from_f64(value: f64) -> Option<BigRational> {
    BigRational::try_from(value).ok()
}
