use binaryen_sys::{
    BinaryenModuleCreate, BinaryenModuleDispose, BinaryenModuleRef, BinaryenType,
    BinaryenTypeAnyref, BinaryenTypeArity, BinaryenTypeArrayref, BinaryenTypeCreate,
    BinaryenTypeEqref, BinaryenTypeExternref, BinaryenTypeFloat32, BinaryenTypeFloat64,
    BinaryenTypeFuncref, BinaryenTypeI31ref, BinaryenTypeInt32, BinaryenTypeInt64,
    BinaryenTypeNone, BinaryenTypeNullExternref, BinaryenTypeNullFuncref, BinaryenTypeNullref,
    BinaryenTypeStringref, BinaryenTypeStringviewIter, BinaryenTypeStringviewWTF16,
    BinaryenTypeStringviewWTF8, BinaryenTypeStructref, BinaryenTypeUnreachable, BinaryenTypeVec128,
};
use std::{mem::transmute, ptr::null_mut};

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

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
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
}

#[test]
fn test_types() {
    let none = Type::none();
    assert_eq!(none.arity(), 0);

    let unreachable = Type::unreachable();
    assert_eq!(unreachable.arity(), 1);
    //     (unreachable, &valueType);
    //     assert(valueType == unreachable);

    //     BinaryenType i32 = BinaryenTypeInt32();
    //     printf("BinaryenTypeInt32: %zd\n", i32);
    //     assert(BinaryenTypeArity(i32) == 1);
    //     BinaryenTypeExpand(i32, &valueType);
    //     assert(valueType == i32);

    //     BinaryenType i64 = BinaryenTypeInt64();
    //     printf("BinaryenTypeInt64: %zd\n", i64);
    //     assert(BinaryenTypeArity(i64) == 1);
    //     BinaryenTypeExpand(i64, &valueType);
    //     assert(valueType == i64);

    //     BinaryenType f32 = BinaryenTypeFloat32();
    //     printf("BinaryenTypeFloat32: %zd\n", f32);
    //     assert(BinaryenTypeArity(f32) == 1);
    //     BinaryenTypeExpand(f32, &valueType);
    //     assert(valueType == f32);

    //     BinaryenType f64 = BinaryenTypeFloat64();
    //     printf("BinaryenTypeFloat64: %zd\n", f64);
    //     assert(BinaryenTypeArity(f64) == 1);
    //     BinaryenTypeExpand(f64, &valueType);
    //     assert(valueType == f64);

    //     BinaryenType v128 = BinaryenTypeVec128();
    //     printf("BinaryenTypeVec128: %zd\n", v128);
    //     assert(BinaryenTypeArity(v128) == 1);
    //     BinaryenTypeExpand(v128, &valueType);
    //     assert(valueType == v128);

    //     BinaryenType funcref = BinaryenTypeFuncref();
    //     printf("BinaryenTypeFuncref: (ptr)\n");
    //     assert(funcref == BinaryenTypeFuncref());
    //     assert(BinaryenTypeArity(funcref) == 1);
    //     BinaryenTypeExpand(funcref, &valueType);
    //     assert(valueType == funcref);

    //     BinaryenType externref = BinaryenTypeExternref();
    //     printf("BinaryenTypeExternref: (ptr)\n");
    //     assert(externref == BinaryenTypeExternref());
    //     assert(BinaryenTypeArity(externref) == 1);
    //     BinaryenTypeExpand(externref, &valueType);
    //     assert(valueType == externref);

    //     BinaryenType anyref = BinaryenTypeAnyref();
    //     printf("BinaryenTypeAnyref: (ptr)\n");
    //     assert(anyref == BinaryenTypeAnyref());
    //     assert(BinaryenTypeArity(anyref) == 1);
    //     BinaryenTypeExpand(anyref, &valueType);
    //     assert(valueType == anyref);

    //     BinaryenType eqref = BinaryenTypeEqref();
    //     printf("BinaryenTypeEqref: (ptr)\n");
    //     assert(eqref == BinaryenTypeEqref());
    //     assert(BinaryenTypeArity(eqref) == 1);
    //     BinaryenTypeExpand(eqref, &valueType);
    //     assert(valueType == eqref);

    //     BinaryenType i31ref = BinaryenTypeI31ref();
    //     printf("BinaryenTypeI31ref: (ptr)\n");
    //     assert(i31ref == BinaryenTypeI31ref());
    //     assert(BinaryenTypeArity(i31ref) == 1);
    //     BinaryenTypeExpand(i31ref, &valueType);
    //     assert(valueType == i31ref);

    //     BinaryenType structref = BinaryenTypeStructref();
    //     printf("BinaryenTypeStructref: (ptr)\n");
    //     assert(structref == BinaryenTypeStructref());
    //     assert(BinaryenTypeArity(structref) == 1);
    //     BinaryenTypeExpand(structref, &valueType);
    //     assert(valueType == structref);

    //     BinaryenType arrayref = BinaryenTypeArrayref();
    //     printf("BinaryenTypeArrayref: (ptr)\n");
    //     assert(arrayref == BinaryenTypeArrayref());
    //     assert(BinaryenTypeArity(arrayref) == 1);
    //     BinaryenTypeExpand(arrayref, &valueType);
    //     assert(valueType == arrayref);

    //     BinaryenType stringref = BinaryenTypeStringref();
    //     printf("BinaryenTypeStringref: (ptr)\n");
    //     assert(BinaryenTypeArity(stringref) == 1);
    //     BinaryenTypeExpand(stringref, &valueType);
    //     assert(valueType == stringref);

    //     BinaryenType stringview_wtf8_ = BinaryenTypeStringviewWTF8();
    //     printf("BinaryenTypeStringviewWTF8: (ptr)\n");
    //     assert(BinaryenTypeArity(stringview_wtf8_) == 1);
    //     BinaryenTypeExpand(stringview_wtf8_, &valueType);
    //     assert(valueType == stringview_wtf8_);

    //     BinaryenType stringview_wtf16_ = BinaryenTypeStringviewWTF16();
    //     printf("BinaryenTypeStringviewWTF16: (ptr)\n");
    //     assert(BinaryenTypeArity(stringview_wtf16_) == 1);
    //     BinaryenTypeExpand(stringview_wtf16_, &valueType);
    //     assert(valueType == stringview_wtf16_);

    //     BinaryenType stringview_iter_ = BinaryenTypeStringviewIter();
    //     printf("BinaryenTypeStringviewIter: (ptr)\n");
    //     assert(BinaryenTypeArity(stringview_iter_) == 1);
    //     BinaryenTypeExpand(stringview_iter_, &valueType);
    //     assert(valueType == stringview_iter_);

    //     BinaryenType nullref = BinaryenTypeNullref();
    //     printf("BinaryenTypeNullref: (ptr)\n");
    //     assert(BinaryenTypeArity(nullref) == 1);
    //     BinaryenTypeExpand(nullref, &valueType);
    //     assert(valueType == nullref);

    //     BinaryenType nullexternref = BinaryenTypeNullExternref();
    //     printf("BinaryenTypeNullExternref: (ptr)\n");
    //     assert(BinaryenTypeArity(nullexternref) == 1);
    //     BinaryenTypeExpand(nullexternref, &valueType);
    //     assert(valueType == nullexternref);

    //     BinaryenType nullfuncref = BinaryenTypeNullFuncref();
    //     printf("BinaryenTypeNullFuncref: (ptr)\n");
    //     assert(BinaryenTypeArity(nullfuncref) == 1);
    //     BinaryenTypeExpand(nullfuncref, &valueType);
    //     assert(valueType == nullfuncref);

    //     printf("BinaryenTypeAuto: %zd\n", BinaryenTypeAuto());

    //     BinaryenType pair[] = {i32, i32};

    //     BinaryenType i32_pair = BinaryenTypeCreate(pair, 2);
    //     assert(BinaryenTypeArity(i32_pair) == 2);
    //     pair[0] = pair[1] = none;
    //     BinaryenTypeExpand(i32_pair, pair);
    //     assert(pair[0] == i32 && pair[1] == i32);

    //     BinaryenType duplicate_pair = BinaryenTypeCreate(pair, 2);
    //     assert(duplicate_pair == i32_pair);

    //     pair[0] = pair[1] = f32;
    //     BinaryenType float_pair = BinaryenTypeCreate(pair, 2);
    //     assert(float_pair != i32_pair);

    //     BinaryenPackedType notPacked = BinaryenPackedTypeNotPacked();
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
