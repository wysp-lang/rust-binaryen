mod type_builder;
mod types;

use binaryen_sys::*;
pub use binaryen_sys::{self as ffi};
use bitflags::bitflags;
use std::{
    ffi::{CStr, CString},
    fmt::Debug,
    marker::PhantomData,
    mem::transmute,
    ptr::{null_mut, slice_from_raw_parts},
};

use crate::types::{HeapType, Type};

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

    pub fn binaryen_const(&self, value: Literal) -> Expression {
        Expression::new(self, unsafe { BinaryenConst(self.r, value.to_c_type()) })
    }

    pub fn local_get(&self, index: u32, typ: Type) -> Expression {
        Expression::new(self, unsafe { BinaryenLocalGet(self.r, index, typ.id) })
    }

    pub fn binary(&self, op: BinaryOp, left: Expression, right: Expression) -> Expression {
        Expression::new(self, unsafe {
            BinaryenBinary(self.r, op as i32, left.r, right.r)
        })
    }

    pub fn add_function(
        &self,
        name: &str,
        params: Type,
        results: Type,
        var_types: &[Type],
        body: Expression,
    ) -> Function {
        let name = CString::new(name).expect("null error");
        let r = unsafe {
            BinaryenAddFunction(
                self.r,
                name.as_ptr(),
                params.id,
                results.id,
                transmute(var_types.as_ptr()),
                var_types.len() as u32,
                body.r,
            )
        };
        Function {
            r,
            phantom_data: Default::default(),
        }
    }

    pub fn compile(&self) -> Vec<u8> {
        let result = unsafe { BinaryenModuleAllocateAndWrite(self.r, null_mut()) };
        let slice = slice_from_raw_parts(result.binary as *const u8, result.binaryBytes);
        let vec = (unsafe { &*slice }).to_vec();
        unsafe { BinaryenStringFree(result.binary as *mut i8) };
        vec
    }

    pub fn add_function_export(&self, internal_name: &str, external_name: &str) {
        let internal_name = CString::new(internal_name).expect("null error");
        let external_name = CString::new(external_name).expect("null error");
        let _export = unsafe {
            BinaryenAddFunctionExport(
                self.r,
                internal_name.as_ptr() as *const i8,
                external_name.as_ptr() as *const i8,
            )
        };
    }

    pub fn set_heap_type_name(&self, heap_type: HeapType, name: &str) {
        let cstr = CString::new(name).expect("null error");
        unsafe { BinaryenModuleSetTypeName(self.r, heap_type.id, cstr.as_ptr()) }
    }
}

impl Debug for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unsafe { BinaryenSetColorsEnabled(false) };
        let ptr = unsafe { BinaryenModuleToString(self.r) };
        let c_str = unsafe { CStr::from_ptr(ptr) };
        write!(f, "{}", c_str.to_str().map_err(|_| std::fmt::Error)?)?;
        unsafe { BinaryenStringFree(ptr) };
        Ok(())
    }
}

pub struct Function<'m> {
    r: BinaryenFunctionRef,
    phantom_data: PhantomData<&'m Module>,
}

impl Function<'_> {
    pub fn name(&self) -> String {
        let n = unsafe { BinaryenFunctionGetName(self.r) };
        let c_str = unsafe { CStr::from_ptr(n) }.to_str().unwrap_or("");
        String::from(c_str)
    }
}

#[derive(Clone, Copy)]
pub struct Expression<'m> {
    r: BinaryenExpressionRef,
    phantom_data: PhantomData<&'m Module>,
}

impl<'m> Expression<'m> {
    fn new(_module: &'m Module, r: BinaryenExpressionRef) -> Self {
        Self {
            r,
            phantom_data: PhantomData,
        }
    }
}

impl Debug for Expression<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unsafe { BinaryenSetColorsEnabled(false) };
        let ptr = unsafe { BinaryenExpressionToString(self.r) };
        let c_str = unsafe { CStr::from_ptr(ptr) };
        write!(f, "{}", c_str.to_str().map_err(|_| std::fmt::Error)?)?;
        unsafe { BinaryenStringFree(ptr) };
        Ok(())
    }
}

#[repr(u32)]
pub enum BinaryOp {
    AddInt32 = wasm_BinaryOp_AddInt32,
    SubInt32 = wasm_BinaryOp_SubInt32,
    MulInt32 = wasm_BinaryOp_MulInt32,
    DivSInt32 = wasm_BinaryOp_DivSInt32,
    DivUInt32 = wasm_BinaryOp_DivUInt32,
    RemSInt32 = wasm_BinaryOp_RemSInt32,
    RemUInt32 = wasm_BinaryOp_RemUInt32,
    AndInt32 = wasm_BinaryOp_AndInt32,
    OrInt32 = wasm_BinaryOp_OrInt32,
    XorInt32 = wasm_BinaryOp_XorInt32,
    ShlInt32 = wasm_BinaryOp_ShlInt32,
    ShrSInt32 = wasm_BinaryOp_ShrSInt32,
    ShrUInt32 = wasm_BinaryOp_ShrUInt32,
    RotLInt32 = wasm_BinaryOp_RotLInt32,
    RotRInt32 = wasm_BinaryOp_RotRInt32,
    EqInt32 = wasm_BinaryOp_EqInt32,
    NeInt32 = wasm_BinaryOp_NeInt32,
    LtSInt32 = wasm_BinaryOp_LtSInt32,
    LtUInt32 = wasm_BinaryOp_LtUInt32,
    LeSInt32 = wasm_BinaryOp_LeSInt32,
    LeUInt32 = wasm_BinaryOp_LeUInt32,
    GtSInt32 = wasm_BinaryOp_GtSInt32,
    GtUInt32 = wasm_BinaryOp_GtUInt32,
    GeSInt32 = wasm_BinaryOp_GeSInt32,
    GeUInt32 = wasm_BinaryOp_GeUInt32,
    AddInt64 = wasm_BinaryOp_AddInt64,
    SubInt64 = wasm_BinaryOp_SubInt64,
    MulInt64 = wasm_BinaryOp_MulInt64,
    DivSInt64 = wasm_BinaryOp_DivSInt64,
    DivUInt64 = wasm_BinaryOp_DivUInt64,
    RemSInt64 = wasm_BinaryOp_RemSInt64,
    RemUInt64 = wasm_BinaryOp_RemUInt64,
    AndInt64 = wasm_BinaryOp_AndInt64,
    OrInt64 = wasm_BinaryOp_OrInt64,
    XorInt64 = wasm_BinaryOp_XorInt64,
    ShlInt64 = wasm_BinaryOp_ShlInt64,
    ShrSInt64 = wasm_BinaryOp_ShrSInt64,
    ShrUInt64 = wasm_BinaryOp_ShrUInt64,
    RotLInt64 = wasm_BinaryOp_RotLInt64,
    RotRInt64 = wasm_BinaryOp_RotRInt64,
    EqInt64 = wasm_BinaryOp_EqInt64,
    NeInt64 = wasm_BinaryOp_NeInt64,
    LtSInt64 = wasm_BinaryOp_LtSInt64,
    LtUInt64 = wasm_BinaryOp_LtUInt64,
    LeSInt64 = wasm_BinaryOp_LeSInt64,
    LeUInt64 = wasm_BinaryOp_LeUInt64,
    GtSInt64 = wasm_BinaryOp_GtSInt64,
    GtUInt64 = wasm_BinaryOp_GtUInt64,
    GeSInt64 = wasm_BinaryOp_GeSInt64,
    GeUInt64 = wasm_BinaryOp_GeUInt64,
    AddFloat32 = wasm_BinaryOp_AddFloat32,
    SubFloat32 = wasm_BinaryOp_SubFloat32,
    MulFloat32 = wasm_BinaryOp_MulFloat32,
    DivFloat32 = wasm_BinaryOp_DivFloat32,
    CopySignFloat32 = wasm_BinaryOp_CopySignFloat32,
    MinFloat32 = wasm_BinaryOp_MinFloat32,
    MaxFloat32 = wasm_BinaryOp_MaxFloat32,
    EqFloat32 = wasm_BinaryOp_EqFloat32,
    NeFloat32 = wasm_BinaryOp_NeFloat32,
    LtFloat32 = wasm_BinaryOp_LtFloat32,
    LeFloat32 = wasm_BinaryOp_LeFloat32,
    GtFloat32 = wasm_BinaryOp_GtFloat32,
    GeFloat32 = wasm_BinaryOp_GeFloat32,
    AddFloat64 = wasm_BinaryOp_AddFloat64,
    SubFloat64 = wasm_BinaryOp_SubFloat64,
    MulFloat64 = wasm_BinaryOp_MulFloat64,
    DivFloat64 = wasm_BinaryOp_DivFloat64,
    CopySignFloat64 = wasm_BinaryOp_CopySignFloat64,
    MinFloat64 = wasm_BinaryOp_MinFloat64,
    MaxFloat64 = wasm_BinaryOp_MaxFloat64,
    EqFloat64 = wasm_BinaryOp_EqFloat64,
    NeFloat64 = wasm_BinaryOp_NeFloat64,
    LtFloat64 = wasm_BinaryOp_LtFloat64,
    LeFloat64 = wasm_BinaryOp_LeFloat64,
    GtFloat64 = wasm_BinaryOp_GtFloat64,
    GeFloat64 = wasm_BinaryOp_GeFloat64,
    EqVecI8x16 = wasm_BinaryOp_EqVecI8x16,
    NeVecI8x16 = wasm_BinaryOp_NeVecI8x16,
    LtSVecI8x16 = wasm_BinaryOp_LtSVecI8x16,
    LtUVecI8x16 = wasm_BinaryOp_LtUVecI8x16,
    GtSVecI8x16 = wasm_BinaryOp_GtSVecI8x16,
    GtUVecI8x16 = wasm_BinaryOp_GtUVecI8x16,
    LeSVecI8x16 = wasm_BinaryOp_LeSVecI8x16,
    LeUVecI8x16 = wasm_BinaryOp_LeUVecI8x16,
    GeSVecI8x16 = wasm_BinaryOp_GeSVecI8x16,
    GeUVecI8x16 = wasm_BinaryOp_GeUVecI8x16,
    EqVecI16x8 = wasm_BinaryOp_EqVecI16x8,
    NeVecI16x8 = wasm_BinaryOp_NeVecI16x8,
    LtSVecI16x8 = wasm_BinaryOp_LtSVecI16x8,
    LtUVecI16x8 = wasm_BinaryOp_LtUVecI16x8,
    GtSVecI16x8 = wasm_BinaryOp_GtSVecI16x8,
    GtUVecI16x8 = wasm_BinaryOp_GtUVecI16x8,
    LeSVecI16x8 = wasm_BinaryOp_LeSVecI16x8,
    LeUVecI16x8 = wasm_BinaryOp_LeUVecI16x8,
    GeSVecI16x8 = wasm_BinaryOp_GeSVecI16x8,
    GeUVecI16x8 = wasm_BinaryOp_GeUVecI16x8,
    EqVecI32x4 = wasm_BinaryOp_EqVecI32x4,
    NeVecI32x4 = wasm_BinaryOp_NeVecI32x4,
    LtSVecI32x4 = wasm_BinaryOp_LtSVecI32x4,
    LtUVecI32x4 = wasm_BinaryOp_LtUVecI32x4,
    GtSVecI32x4 = wasm_BinaryOp_GtSVecI32x4,
    GtUVecI32x4 = wasm_BinaryOp_GtUVecI32x4,
    LeSVecI32x4 = wasm_BinaryOp_LeSVecI32x4,
    LeUVecI32x4 = wasm_BinaryOp_LeUVecI32x4,
    GeSVecI32x4 = wasm_BinaryOp_GeSVecI32x4,
    GeUVecI32x4 = wasm_BinaryOp_GeUVecI32x4,
    EqVecI64x2 = wasm_BinaryOp_EqVecI64x2,
    NeVecI64x2 = wasm_BinaryOp_NeVecI64x2,
    LtSVecI64x2 = wasm_BinaryOp_LtSVecI64x2,
    GtSVecI64x2 = wasm_BinaryOp_GtSVecI64x2,
    LeSVecI64x2 = wasm_BinaryOp_LeSVecI64x2,
    GeSVecI64x2 = wasm_BinaryOp_GeSVecI64x2,
    EqVecF32x4 = wasm_BinaryOp_EqVecF32x4,
    NeVecF32x4 = wasm_BinaryOp_NeVecF32x4,
    LtVecF32x4 = wasm_BinaryOp_LtVecF32x4,
    GtVecF32x4 = wasm_BinaryOp_GtVecF32x4,
    LeVecF32x4 = wasm_BinaryOp_LeVecF32x4,
    GeVecF32x4 = wasm_BinaryOp_GeVecF32x4,
    EqVecF64x2 = wasm_BinaryOp_EqVecF64x2,
    NeVecF64x2 = wasm_BinaryOp_NeVecF64x2,
    LtVecF64x2 = wasm_BinaryOp_LtVecF64x2,
    GtVecF64x2 = wasm_BinaryOp_GtVecF64x2,
    LeVecF64x2 = wasm_BinaryOp_LeVecF64x2,
    GeVecF64x2 = wasm_BinaryOp_GeVecF64x2,
    AndVec128 = wasm_BinaryOp_AndVec128,
    OrVec128 = wasm_BinaryOp_OrVec128,
    XorVec128 = wasm_BinaryOp_XorVec128,
    AndNotVec128 = wasm_BinaryOp_AndNotVec128,
    AddVecI8x16 = wasm_BinaryOp_AddVecI8x16,
    AddSatSVecI8x16 = wasm_BinaryOp_AddSatSVecI8x16,
    AddSatUVecI8x16 = wasm_BinaryOp_AddSatUVecI8x16,
    SubVecI8x16 = wasm_BinaryOp_SubVecI8x16,
    SubSatSVecI8x16 = wasm_BinaryOp_SubSatSVecI8x16,
    SubSatUVecI8x16 = wasm_BinaryOp_SubSatUVecI8x16,
    MinSVecI8x16 = wasm_BinaryOp_MinSVecI8x16,
    MinUVecI8x16 = wasm_BinaryOp_MinUVecI8x16,
    MaxSVecI8x16 = wasm_BinaryOp_MaxSVecI8x16,
    MaxUVecI8x16 = wasm_BinaryOp_MaxUVecI8x16,
    AvgrUVecI8x16 = wasm_BinaryOp_AvgrUVecI8x16,
    AddVecI16x8 = wasm_BinaryOp_AddVecI16x8,
    AddSatSVecI16x8 = wasm_BinaryOp_AddSatSVecI16x8,
    AddSatUVecI16x8 = wasm_BinaryOp_AddSatUVecI16x8,
    SubVecI16x8 = wasm_BinaryOp_SubVecI16x8,
    SubSatSVecI16x8 = wasm_BinaryOp_SubSatSVecI16x8,
    SubSatUVecI16x8 = wasm_BinaryOp_SubSatUVecI16x8,
    MulVecI16x8 = wasm_BinaryOp_MulVecI16x8,
    MinSVecI16x8 = wasm_BinaryOp_MinSVecI16x8,
    MinUVecI16x8 = wasm_BinaryOp_MinUVecI16x8,
    MaxSVecI16x8 = wasm_BinaryOp_MaxSVecI16x8,
    MaxUVecI16x8 = wasm_BinaryOp_MaxUVecI16x8,
    AvgrUVecI16x8 = wasm_BinaryOp_AvgrUVecI16x8,
    Q15MulrSatSVecI16x8 = wasm_BinaryOp_Q15MulrSatSVecI16x8,
    ExtMulLowSVecI16x8 = wasm_BinaryOp_ExtMulLowSVecI16x8,
    ExtMulHighSVecI16x8 = wasm_BinaryOp_ExtMulHighSVecI16x8,
    ExtMulLowUVecI16x8 = wasm_BinaryOp_ExtMulLowUVecI16x8,
    ExtMulHighUVecI16x8 = wasm_BinaryOp_ExtMulHighUVecI16x8,
    AddVecI32x4 = wasm_BinaryOp_AddVecI32x4,
    SubVecI32x4 = wasm_BinaryOp_SubVecI32x4,
    MulVecI32x4 = wasm_BinaryOp_MulVecI32x4,
    MinSVecI32x4 = wasm_BinaryOp_MinSVecI32x4,
    MinUVecI32x4 = wasm_BinaryOp_MinUVecI32x4,
    MaxSVecI32x4 = wasm_BinaryOp_MaxSVecI32x4,
    MaxUVecI32x4 = wasm_BinaryOp_MaxUVecI32x4,
    DotSVecI16x8ToVecI32x4 = wasm_BinaryOp_DotSVecI16x8ToVecI32x4,
    ExtMulLowSVecI32x4 = wasm_BinaryOp_ExtMulLowSVecI32x4,
    ExtMulHighSVecI32x4 = wasm_BinaryOp_ExtMulHighSVecI32x4,
    ExtMulLowUVecI32x4 = wasm_BinaryOp_ExtMulLowUVecI32x4,
    ExtMulHighUVecI32x4 = wasm_BinaryOp_ExtMulHighUVecI32x4,
    AddVecI64x2 = wasm_BinaryOp_AddVecI64x2,
    SubVecI64x2 = wasm_BinaryOp_SubVecI64x2,
    MulVecI64x2 = wasm_BinaryOp_MulVecI64x2,
    ExtMulLowSVecI64x2 = wasm_BinaryOp_ExtMulLowSVecI64x2,
    ExtMulHighSVecI64x2 = wasm_BinaryOp_ExtMulHighSVecI64x2,
    ExtMulLowUVecI64x2 = wasm_BinaryOp_ExtMulLowUVecI64x2,
    ExtMulHighUVecI64x2 = wasm_BinaryOp_ExtMulHighUVecI64x2,
    AddVecF32x4 = wasm_BinaryOp_AddVecF32x4,
    SubVecF32x4 = wasm_BinaryOp_SubVecF32x4,
    MulVecF32x4 = wasm_BinaryOp_MulVecF32x4,
    DivVecF32x4 = wasm_BinaryOp_DivVecF32x4,
    MinVecF32x4 = wasm_BinaryOp_MinVecF32x4,
    MaxVecF32x4 = wasm_BinaryOp_MaxVecF32x4,
    PMinVecF32x4 = wasm_BinaryOp_PMinVecF32x4,
    PMaxVecF32x4 = wasm_BinaryOp_PMaxVecF32x4,
    AddVecF64x2 = wasm_BinaryOp_AddVecF64x2,
    SubVecF64x2 = wasm_BinaryOp_SubVecF64x2,
    MulVecF64x2 = wasm_BinaryOp_MulVecF64x2,
    DivVecF64x2 = wasm_BinaryOp_DivVecF64x2,
    MinVecF64x2 = wasm_BinaryOp_MinVecF64x2,
    MaxVecF64x2 = wasm_BinaryOp_MaxVecF64x2,
    PMinVecF64x2 = wasm_BinaryOp_PMinVecF64x2,
    PMaxVecF64x2 = wasm_BinaryOp_PMaxVecF64x2,
    NarrowSVecI16x8ToVecI8x16 = wasm_BinaryOp_NarrowSVecI16x8ToVecI8x16,
    NarrowUVecI16x8ToVecI8x16 = wasm_BinaryOp_NarrowUVecI16x8ToVecI8x16,
    NarrowSVecI32x4ToVecI16x8 = wasm_BinaryOp_NarrowSVecI32x4ToVecI16x8,
    NarrowUVecI32x4ToVecI16x8 = wasm_BinaryOp_NarrowUVecI32x4ToVecI16x8,
    SwizzleVecI8x16 = wasm_BinaryOp_SwizzleVecI8x16,
    RelaxedSwizzleVecI8x16 = wasm_BinaryOp_RelaxedSwizzleVecI8x16,
    RelaxedMinVecF32x4 = wasm_BinaryOp_RelaxedMinVecF32x4,
    RelaxedMaxVecF32x4 = wasm_BinaryOp_RelaxedMaxVecF32x4,
    RelaxedMinVecF64x2 = wasm_BinaryOp_RelaxedMinVecF64x2,
    RelaxedMaxVecF64x2 = wasm_BinaryOp_RelaxedMaxVecF64x2,
    RelaxedQ15MulrSVecI16x8 = wasm_BinaryOp_RelaxedQ15MulrSVecI16x8,
    DotI8x16I7x16SToVecI16x8 = wasm_BinaryOp_DotI8x16I7x16SToVecI16x8,
    InvalidBinary = wasm_BinaryOp_InvalidBinary,
}

impl Drop for Module {
    fn drop(&mut self) {
        unsafe { BinaryenModuleDispose(self.r) };
        self.r = null_mut();
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
    let module = Module::new();
    let ii: [Type; 2] = [Type::int32(), Type::int32()];

    let params = Type::tuple(&ii);

    let results = Type::int32();

    let x = module.local_get(0, Type::int32());
    let y = module.local_get(1, Type::int32());
    let add = module.binary(BinaryOp::AddInt32, x, y);
    assert_eq!(
        format!("{:?}", add),
        "(i32.add\n (local.get $0)\n (local.get $1)\n)"
    );

    let function = module.add_function("adder", params, results, &[], add);
    assert_eq!(function.name(), "adder");

    module.add_function_export("adder", "the_adder");

    assert_eq!(format!("{:?}", module), "(module\n (type $0 (func (param i32 i32) (result i32)))\n (export \"the_adder\" (func $adder))\n (func $adder (param $0 i32) (param $1 i32) (result i32)\n  (i32.add\n   (local.get $0)\n   (local.get $1)\n  )\n )\n)\n");

    assert_eq!(
        module.compile(),
        vec![
            0, 97, 115, 109, 1, 0, 0, 0, 1, 7, 1, 96, 2, 127, 127, 1, 127, 3, 2, 1, 0, 7, 13, 1, 9,
            116, 104, 101, 95, 97, 100, 100, 101, 114, 0, 0, 10, 9, 1, 7, 0, 32, 0, 32, 1, 106, 11
        ]
    );
}
