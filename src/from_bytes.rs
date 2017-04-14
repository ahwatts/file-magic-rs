use num::BigInt;

pub trait FromBigInt {
    fn from_bigint(bigint: BigInt) -> Self;
}

macro_rules! from_big_int_impl {
    ($uty:ident, $uconverter_mtd:ident) => {
        impl FromBigInt for $uty {
            fn from_bigint(bigint: BigInt) -> $uty {
                use num::ToPrimitive;
                bigint.$uconverter_mtd().unwrap_or_else(|| {
                    panic!("Cannot convert {} to a {} using converter method {}",
                           bigint, stringify!($uty), stringify!($uconverter_mtd));
                })
            }
        }
    };

    ($sty:ident, $uty:ident, $sconverter_mtd:ident, $uconverter_mtd:ident) => {
        impl FromBigInt for $sty {
            fn from_bigint(bigint: BigInt) -> $sty {
                use num::ToPrimitive;
                use num::bigint::Sign::*;

                if let Some(sval) = bigint.$sconverter_mtd() {
                    println!("(bigint) {:?}.{} = {:?} ({}?)",
                             bigint, stringify!($sconverter_mtd),
                             sval, stringify!($sty));
                    return sval
                }

                if let Some(uval) = bigint.$uconverter_mtd() {
                    let uptr: *const $uty = &uval;
                    let sval = unsafe { *(uptr as *const $sty) };
                    println!("bigint = {:?} uval = {:?} sval = {:?}", bigint, uval, sval);
                    return match bigint.sign() {
                        Minus => -sval,
                        _ => sval
                    }
                }

                panic!("Cannot convert {} to a {} or via an unsafe cast through a {}",
                       bigint, stringify!($sty), stringify!($uty));
            }
        }
    };
}

from_big_int_impl!(u8,  to_u8);
from_big_int_impl!(u16, to_u16);
from_big_int_impl!(u32, to_u32);
from_big_int_impl!(u64, to_u64);
from_big_int_impl!(i8,  u8,  to_i8,  to_u8);
from_big_int_impl!(i16, u16, to_i16, to_u16);
from_big_int_impl!(i32, u32, to_i32, to_u32);
from_big_int_impl!(i64, u64, to_i64, to_u64);
