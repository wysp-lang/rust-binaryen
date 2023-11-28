use binaryen_sys::*;
pub use binaryen_sys::{self as ffi};
use bitflags::bitflags;
use std::{
    ffi::{CStr, CString},
    fmt::Debug,
    mem::transmute,
    ptr::null_mut,
};

#[repr(transparent)]
pub struct Module {
    r: BinaryenModuleRef,
}

impl Module {
    pub fn new() -> Self {
        Self {
            r: unsafe { BinaryenModuleCreate() },
        }
    }

    pub fn binaryen_const(&mut self, value: Literal) -> BinaryenExpressionRef {
        unsafe { BinaryenConst(self.r, value.to_c_type()) }
    }

    pub fn local_get(& self, index: u32, typ: Type) -> BinaryenExpressionRef {
        unsafe{BinaryenLocalGet(self.r, index, typ.id)}
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
pub struct Type {
    id: BinaryenType,
}

impl Type {
    pub fn none() -> Self {
        Self {
            id: unsafe { BinaryenTypeNone() },
        }
    }
    pub fn int32() -> Self {
        Self {
            id: unsafe { BinaryenTypeInt32() },
        }
    }
    pub fn int64() -> Self {
        Self {
            id: unsafe { BinaryenTypeInt64() },
        }
    }
    pub fn float32() -> Self {
        Self {
            id: unsafe { BinaryenTypeFloat32() },
        }
    }
    pub fn float64() -> Self {
        Self {
            id: unsafe { BinaryenTypeFloat64() },
        }
    }
    pub fn vec128() -> Self {
        Self {
            id: unsafe { BinaryenTypeVec128() },
        }
    }
    pub fn funcref() -> Self {
        Self {
            id: unsafe { BinaryenTypeFuncref() },
        }
    }
    pub fn externref() -> Self {
        Self {
            id: unsafe { BinaryenTypeExternref() },
        }
    }
    pub fn anyref() -> Self {
        Self {
            id: unsafe { BinaryenTypeAnyref() },
        }
    }
    pub fn eqref() -> Self {
        Self {
            id: unsafe { BinaryenTypeEqref() },
        }
    }
    pub fn i31ref() -> Self {
        Self {
            id: unsafe { BinaryenTypeI31ref() },
        }
    }
    pub fn structref() -> Self {
        Self {
            id: unsafe { BinaryenTypeStructref() },
        }
    }
    pub fn arrayref() -> Self {
        Self {
            id: unsafe { BinaryenTypeArrayref() },
        }
    }
    pub fn stringref() -> Self {
        Self {
            id: unsafe { BinaryenTypeStringref() },
        }
    }
    pub fn stringview_wtf8() -> Self {
        Self {
            id: unsafe { BinaryenTypeStringviewWTF8() },
        }
    }
    pub fn stringview_wtf16() -> Self {
        Self {
            id: unsafe { BinaryenTypeStringviewWTF16() },
        }
    }
    pub fn stringview_iter() -> Self {
        Self {
            id: unsafe { BinaryenTypeStringviewIter() },
        }
    }
    pub fn nullref() -> Self {
        Self {
            id: unsafe { BinaryenTypeNullref() },
        }
    }
    pub fn null_externref() -> Self {
        Self {
            id: unsafe { BinaryenTypeNullExternref() },
        }
    }
    pub fn null_funcref() -> Self {
        Self {
            id: unsafe { BinaryenTypeNullFuncref() },
        }
    }
    pub fn unreachable() -> Self {
        Self {
            id: unsafe { BinaryenTypeUnreachable() },
        }
    }

    /// Not a real type. Used as the last parameter to BinaryenBlock to let
    /// the API figure out the type instead of providing one.
    pub fn auto() -> Self {
        Self {
            id: unsafe { BinaryenTypeAuto() },
        }
    }

    pub fn tuple(types: &[Type]) -> Self {
        Self {
            id: unsafe {
                BinaryenTypeCreate(transmute(types.as_ptr()), types.len().try_into().unwrap())
            },
        }
    }

    pub fn arity(&self) -> u32 {
        unsafe { BinaryenTypeArity(self.id) }
    }

    pub fn iter(&self) -> impl Iterator<Item = Type> {
        let size = self.arity() as usize;
        let mut slice = (&[Type { id: 0 }]).repeat(size);
        unsafe { BinaryenTypeExpand(self.id, slice.as_mut_ptr() as *mut usize) };
        slice.into_iter()
    }

    pub fn get_heap_type(&self) -> HeapType {
        HeapType {
            id: unsafe { BinaryenTypeGetHeapType(self.id) },
        }
    }

    pub fn from_heap_type(heap_type: &HeapType, nullable: bool) -> Self {
        heap_type.get_type(nullable)
    }

    pub fn is_nullable(&self) -> bool {
        unsafe { BinaryenTypeIsNullable(self.id) }
    }
}

impl Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ptr = unsafe { BinaryenTypeToString(self.id) };
        let c_str = unsafe { CStr::from_ptr(ptr) };
        write!(f, "{}", c_str.to_str().map_err(|_| std::fmt::Error)?)?;
        unsafe { BinaryenStringFree(ptr) };
        Ok(())
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct HeapType {
    id: BinaryenHeapType,
}
impl HeapType {
    pub fn ext() -> Self {
        Self {
            id: unsafe { BinaryenHeapTypeExt() },
        }
    }
    pub fn func() -> Self {
        Self {
            id: unsafe { BinaryenHeapTypeFunc() },
        }
    }
    pub fn any() -> Self {
        Self {
            id: unsafe { BinaryenHeapTypeAny() },
        }
    }
    pub fn eq() -> Self {
        Self {
            id: unsafe { BinaryenHeapTypeEq() },
        }
    }
    pub fn i31() -> Self {
        Self {
            id: unsafe { BinaryenHeapTypeI31() },
        }
    }
    pub fn struct_() -> Self {
        Self {
            id: unsafe { BinaryenHeapTypeStruct() },
        }
    }
    pub fn array() -> Self {
        Self {
            id: unsafe { BinaryenHeapTypeArray() },
        }
    }
    pub fn string() -> Self {
        Self {
            id: unsafe { BinaryenHeapTypeString() },
        }
    }
    pub fn stringview_wtf8() -> Self {
        Self {
            id: unsafe { BinaryenHeapTypeStringviewWTF8() },
        }
    }
    pub fn stringview_wtf16() -> Self {
        Self {
            id: unsafe { BinaryenHeapTypeStringviewWTF16() },
        }
    }
    pub fn stringview_iter() -> Self {
        Self {
            id: unsafe { BinaryenHeapTypeStringviewIter() },
        }
    }
    pub fn none() -> Self {
        Self {
            id: unsafe { BinaryenHeapTypeNone() },
        }
    }
    pub fn noext() -> Self {
        Self {
            id: unsafe { BinaryenHeapTypeNoext() },
        }
    }
    pub fn nofunc() -> Self {
        Self {
            id: unsafe { BinaryenHeapTypeNofunc() },
        }
    }

    pub fn is_basic(&self) -> bool {
        unsafe { BinaryenHeapTypeIsBasic(self.id) }
    }
    pub fn is_signature(&self) -> bool {
        unsafe { BinaryenHeapTypeIsSignature(self.id) }
    }
    pub fn is_struct(&self) -> bool {
        unsafe { BinaryenHeapTypeIsStruct(self.id) }
    }
    pub fn is_array(&self) -> bool {
        unsafe { BinaryenHeapTypeIsArray(self.id) }
    }
    pub fn is_bottom(&self) -> bool {
        unsafe { BinaryenHeapTypeIsBottom(self.id) }
    }

    pub fn get_bottom(&self) -> Self {
        Self {
            id: unsafe { BinaryenHeapTypeGetBottom(self.id) },
        }
    }

    pub fn from_type(typ: &Type) -> Self {
        typ.get_heap_type()
    }

    pub fn get_type(&self, nullable: bool) -> Type {
        Type {
            id: unsafe { BinaryenTypeFromHeapType(self.id, nullable) },
        }
    }
}

impl Debug for HeapType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ptr = unsafe { BinaryenHeapTypeToString(self.id) };
        let c_str = unsafe { CStr::from_ptr(ptr) };
        write!(f, "{}", c_str.to_str().map_err(|_| std::fmt::Error)?)?;
        unsafe { BinaryenStringFree(ptr) };
        Ok(())
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum PackedType {
    NotPacked = wasm_Field_PackedType_not_packed as isize,
    I8 = wasm_Field_PackedType_i8_ as isize,
    I16 = wasm_Field_PackedType_i16_ as isize,
}

bitflags! {
    #[repr(transparent)]
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct Features: u32 {
         const None = wasm_FeatureSet_Feature_None;
         const Atomics = wasm_FeatureSet_Feature_Atomics;
         const MutableGlobals = wasm_FeatureSet_Feature_MutableGlobals;
         const TruncSat = wasm_FeatureSet_Feature_TruncSat;
         const SIMD = wasm_FeatureSet_Feature_SIMD;
         const BulkMemory = wasm_FeatureSet_Feature_BulkMemory;
         const SignExt = wasm_FeatureSet_Feature_SignExt;
         const ExceptionHandling = wasm_FeatureSet_Feature_ExceptionHandling;
         const TailCall = wasm_FeatureSet_Feature_TailCall;
         const ReferenceTypes = wasm_FeatureSet_Feature_ReferenceTypes;
         const Multivalue = wasm_FeatureSet_Feature_Multivalue;
         const GC = wasm_FeatureSet_Feature_GC;
         const Memory64 = wasm_FeatureSet_Feature_Memory64;
         const RelaxedSIMD = wasm_FeatureSet_Feature_RelaxedSIMD;
         const ExtendedConst = wasm_FeatureSet_Feature_ExtendedConst;
         const Strings = wasm_FeatureSet_Feature_Strings;
         const MultiMemory = wasm_FeatureSet_Feature_MultiMemory;
         const TypedContinuations = wasm_FeatureSet_Feature_TypedContinuations;
         const MVP = wasm_FeatureSet_Feature_MVP;
         const Default = wasm_FeatureSet_Feature_Default;
         const All = wasm_FeatureSet_Feature_All;
    }
}

pub enum Literal {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
    V128([u8; 16]),
}

impl Literal {
    fn to_c_type(&self) -> BinaryenLiteral {
        match self {
            Literal::I32(x) => BinaryenLiteral {
                type_: 2,
                __bindgen_anon_1: BinaryenLiteral__bindgen_ty_1 { i32_: *x },
            },
            Literal::I64(x) => BinaryenLiteral {
                type_: 3,
                __bindgen_anon_1: BinaryenLiteral__bindgen_ty_1 { i64_: *x },
            },
            Literal::F32(x) => BinaryenLiteral {
                type_: 4,
                __bindgen_anon_1: BinaryenLiteral__bindgen_ty_1 { f32_: *x },
            },
            Literal::F64(x) => BinaryenLiteral {
                type_: 5,
                __bindgen_anon_1: BinaryenLiteral__bindgen_ty_1 { f64_: *x },
            },
            Literal::V128(x) => BinaryenLiteral {
                type_: 6,
                __bindgen_anon_1: BinaryenLiteral__bindgen_ty_1 { v128: *x },
            },
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

    let stringref = Type::stringref();
    assert_eq!(format!("{:?}", stringref), "stringref");

    assert_eq!(HeapType::ext().is_bottom(), false);
    assert_eq!(HeapType::noext().is_bottom(), true);
    assert_eq!(HeapType::ext().get_bottom(), HeapType::noext());

    let eq = Type::eqref().get_heap_type();
    assert_eq!(eq, HeapType::eq());
    let ref_null_eq = Type::from_heap_type(&eq, true);
    assert_eq!(ref_null_eq.get_heap_type(), eq);
    assert!(ref_null_eq.is_nullable());
    let ref_eq = eq.get_type(false);
    assert_ne!(ref_eq, ref_null_eq);
    assert_eq!(HeapType::from_type(&ref_eq), eq);
    assert!(!ref_eq.is_nullable());
}

#[test]
fn test_features() {
    assert_eq!(
        Features::Default,
        Features::SignExt | Features::MutableGlobals
    );
}

#[test]
fn test_core() {
    // Module creation

    let mut module = Module::new();

    // Literals and Consts

    let constI32 = module.binaryen_const(Literal::I32(1));
}

#[test]
fn hello_world() {
    let mut module = Module::new();
    let ii: [Type; 2] = [Type::int32(), Type::int32()];

    let params = Type::tuple(&ii);

    let results = Type::int32();

    
}
