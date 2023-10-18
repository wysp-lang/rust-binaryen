use binaryen_sys::{
    BinaryenModuleCreate, BinaryenModuleDispose, BinaryenModuleRef, BinaryenPackedType,
    BinaryenPackedTypeInt16, BinaryenPackedTypeInt8, BinaryenPackedTypeNotPacked,
    BinaryenStringFree, BinaryenType, BinaryenTypeAnyref, BinaryenTypeArity, BinaryenTypeArrayref,
    BinaryenTypeAuto, BinaryenTypeCreate, BinaryenTypeEqref, BinaryenTypeExpand,
    BinaryenTypeExternref, BinaryenTypeFloat32, BinaryenTypeFloat64, BinaryenTypeFuncref,
    BinaryenTypeI31ref, BinaryenTypeInt32, BinaryenTypeInt64, BinaryenTypeNone,
    BinaryenTypeNullExternref, BinaryenTypeNullFuncref, BinaryenTypeNullref, BinaryenTypeStringref,
    BinaryenTypeStringviewIter, BinaryenTypeStringviewWTF16, BinaryenTypeStringviewWTF8,
    BinaryenTypeStructref, BinaryenTypeToString, BinaryenTypeUnreachable, BinaryenTypeVec128,
};
use std::{ffi::CStr, fmt::Debug, mem::transmute, ptr::null_mut};

#[repr(transparent)]
struct Module {
    r: BinaryenModuleRef,
}

impl Module {
    fn new() -> Self {
        Self {
            r: unsafe { BinaryenModuleCreate() },
        }
    }
}

impl Drop for Module {
    fn drop(&mut self) {
        unsafe { BinaryenModuleDispose(self.r) };
        self.r = null_mut();
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
struct Type {
    r: BinaryenType,
}

impl Type {
    fn none() -> Self {
        Self {
            r: unsafe { BinaryenTypeNone() },
        }
    }
    fn int32() -> Self {
        Self {
            r: unsafe { BinaryenTypeInt32() },
        }
    }
    fn int64() -> Self {
        Self {
            r: unsafe { BinaryenTypeInt64() },
        }
    }
    fn float32() -> Self {
        Self {
            r: unsafe { BinaryenTypeFloat32() },
        }
    }
    fn float64() -> Self {
        Self {
            r: unsafe { BinaryenTypeFloat64() },
        }
    }
    fn vec128() -> Self {
        Self {
            r: unsafe { BinaryenTypeVec128() },
        }
    }
    fn funcref() -> Self {
        Self {
            r: unsafe { BinaryenTypeFuncref() },
        }
    }
    fn externref() -> Self {
        Self {
            r: unsafe { BinaryenTypeExternref() },
        }
    }
    fn anyref() -> Self {
        Self {
            r: unsafe { BinaryenTypeAnyref() },
        }
    }
    fn eqref() -> Self {
        Self {
            r: unsafe { BinaryenTypeEqref() },
        }
    }
    fn i31ref() -> Self {
        Self {
            r: unsafe { BinaryenTypeI31ref() },
        }
    }
    fn structref() -> Self {
        Self {
            r: unsafe { BinaryenTypeStructref() },
        }
    }
    fn arrayref() -> Self {
        Self {
            r: unsafe { BinaryenTypeArrayref() },
        }
    }
    fn stringref() -> Self {
        Self {
            r: unsafe { BinaryenTypeStringref() },
        }
    }
    fn stringviewWTF8() -> Self {
        Self {
            r: unsafe { BinaryenTypeStringviewWTF8() },
        }
    }
    fn stringviewWTF16() -> Self {
        Self {
            r: unsafe { BinaryenTypeStringviewWTF16() },
        }
    }
    fn stringview_iter() -> Self {
        Self {
            r: unsafe { BinaryenTypeStringviewIter() },
        }
    }
    fn nullref() -> Self {
        Self {
            r: unsafe { BinaryenTypeNullref() },
        }
    }
    fn null_externref() -> Self {
        Self {
            r: unsafe { BinaryenTypeNullExternref() },
        }
    }
    fn null_funcref() -> Self {
        Self {
            r: unsafe { BinaryenTypeNullFuncref() },
        }
    }
    fn unreachable() -> Self {
        Self {
            r: unsafe { BinaryenTypeUnreachable() },
        }
    }

    /// Not a real type. Used as the last parameter to BinaryenBlock to let
    /// the API figure out the type instead of providing one.
    fn auto() -> Self {
        Self {
            r: unsafe { BinaryenTypeAuto() },
        }
    }

    fn tuple(types: &[Type]) -> Self {
        Self {
            r: unsafe {
                BinaryenTypeCreate(transmute(types.as_ptr()), types.len().try_into().unwrap())
            },
        }
    }

    fn arity(&self) -> u32 {
        unsafe { BinaryenTypeArity(self.r) }
    }

    fn iter(&self) -> impl Iterator<Item = Type> {
        let size = self.arity() as usize;
        let mut slice = (&[Type { r: 0 }]).repeat(size);
        unsafe { BinaryenTypeExpand(self.r, slice.as_mut_ptr() as *mut usize) };
        slice.into_iter()
    }
}

impl Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ptr = unsafe { BinaryenTypeToString(self.r) };
        let c_str = unsafe { CStr::from_ptr(ptr) };
        write!(f, "{}", c_str.to_str().map_err(|_| std::fmt::Error)?)?;
        unsafe { BinaryenStringFree(ptr) };
        Ok(())
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[repr(transparent)]
struct PackedType {
    r: BinaryenPackedType,
}

impl PackedType {
    fn not_packed() -> Self {
        Self {
            r: unsafe { BinaryenPackedTypeNotPacked() },
        }
    }
    fn int8() -> Self {
        Self {
            r: unsafe { BinaryenPackedTypeInt8() },
        }
    }
    fn int16() -> Self {
        Self {
            r: unsafe { BinaryenPackedTypeInt16() },
        }
    }
}

#[test]
fn test_types() {
    let none = Type::none();
    assert_eq!(none.arity(), 0);

    let unreachable = Type::unreachable();
    assert_eq!(unreachable.arity(), 1);

    let int32 = Type::int32();
    assert_eq!(format!("{:?}", int32), "i32");

    let i32_pair = Type::tuple(&[Type::int32(), Type::int32()]);
    assert_eq!(i32_pair.arity(), 2);
    assert_eq!(format!("{:?}", i32_pair), "(i32 i32)");
    //     pair[0] = pair[1] = none;

    let pair_vec = i32_pair.iter().collect::<Vec<_>>();
    assert_eq!(pair_vec, vec![Type::int32(), Type::int32()]);

    let duplicate_pair = Type::tuple(&pair_vec);
    assert_eq!(duplicate_pair, i32_pair);

    let float_pair = Type::tuple(&[Type::float32(), Type::float32()]);
    assert_ne!(float_pair, i32_pair);

    let not_packed = PackedType::not_packed();
    //     printf("BinaryenPackedTypeNotPacked: %d\n", notPacked);
    //     BinaryenPackedType i8 = BinaryenPackedTypeInt8();
    //     printf("BinaryenPackedTypeInt8: %d\n", i8);
    //     BinaryenPackedType i16 = BinaryenPackedTypeInt16();
    //     printf("BinaryenPackedTypeInt16: %d\n", i16);

    //     printf("BinaryenHeapTypeExt: %zd\n", BinaryenHeapTypeExt());
    //     printf("BinaryenHeapTypeFunc: %zd\n", BinaryenHeapTypeFunc());
    //     printf("BinaryenHeapTypeAny: %zd\n", BinaryenHeapTypeAny());
    //     printf("BinaryenHeapTypeEq: %zd\n", BinaryenHeapTypeEq());
    //     printf("BinaryenHeapTypeI31: %zd\n", BinaryenHeapTypeI31());
    //     printf("BinaryenHeapTypeStruct: %zd\n", BinaryenHeapTypeStruct());
    //     printf("BinaryenHeapTypeArray: %zd\n", BinaryenHeapTypeArray());
    //     printf("BinaryenHeapTypeString: %zd\n", BinaryenHeapTypeString());
    //     printf("BinaryenHeapTypeStringviewWTF8: %zd\n",
    //            BinaryenHeapTypeStringviewWTF8());
    //     printf("BinaryenHeapTypeStringviewWTF16: %zd\n",
    //            BinaryenHeapTypeStringviewWTF16());
    //     printf("BinaryenHeapTypeStringviewIter: %zd\n",
    //            BinaryenHeapTypeStringviewIter());
    //     printf("BinaryenHeapTypeNone: %zd\n", BinaryenHeapTypeNone());
    //     printf("BinaryenHeapTypeNoext: %zd\n", BinaryenHeapTypeNoext());
    //     printf("BinaryenHeapTypeNofunc: %zd\n", BinaryenHeapTypeNofunc());

    //     assert(!BinaryenHeapTypeIsBottom(BinaryenHeapTypeExt()));
    //     assert(BinaryenHeapTypeIsBottom(BinaryenHeapTypeNoext()));
    //     assert(BinaryenHeapTypeGetBottom(BinaryenHeapTypeExt()) ==
    //            BinaryenHeapTypeNoext());

    //     BinaryenHeapType eq = BinaryenTypeGetHeapType(eqref);
    //     assert(eq == BinaryenHeapTypeEq());
    //     BinaryenType ref_null_eq = BinaryenTypeFromHeapType(eq, true);
    //     assert(BinaryenTypeGetHeapType(ref_null_eq) == eq);
    //     assert(BinaryenTypeIsNullable(ref_null_eq));
    //     BinaryenType ref_eq = BinaryenTypeFromHeapType(eq, false);
    //     assert(ref_eq != ref_null_eq);
    //     assert(BinaryenTypeGetHeapType(ref_eq) == eq);
    //     assert(!BinaryenTypeIsNullable(ref_eq));
}
