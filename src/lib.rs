use binaryen_sys::*;
pub use binaryen_sys::{self as ffi};
use bitflags::bitflags;
use std::{
    ffi::{CStr, CString},
    fmt::Debug,
    fs::File,
    io::Write,
    marker::PhantomData,
    mem::transmute,
    ptr::{null_mut, slice_from_raw_parts},
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
        unsafe { BinaryenSetColorsEnabled(false) };
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
        unsafe { BinaryenSetColorsEnabled(false) };
        let ptr = unsafe { BinaryenHeapTypeToString(self.id) };
        let c_str = unsafe { CStr::from_ptr(ptr) };
        write!(f, "{}", c_str.to_str().map_err(|_| std::fmt::Error)?)?;
        unsafe { BinaryenStringFree(ptr) };
        Ok(())
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[repr(u32)]
pub enum PackedType {
    NotPacked = wasm_Field_PackedType_not_packed,
    I8 = wasm_Field_PackedType_i8_,
    I16 = wasm_Field_PackedType_i16_,
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

pub struct TypeBuilder {
    r: TypeBuilderRef,
}

impl TypeBuilder {
    pub fn new(size: u32) -> Self {
        TypeBuilder {
            r: unsafe { TypeBuilderCreate(size) },
        }
    }

    pub fn get_size(&self) -> u32 {
        unsafe { TypeBuilderGetSize(self.r) }
    }

    pub fn set_struct_type(
        &mut self,
        index: u32,
        field_types: &[Type],
        field_packed_types: &[PackedType],
        field_mutables: &[bool],
    ) {
        assert!(
            index < self.get_size()
                && field_mutables.len() == field_types.len()
                && field_mutables.len() == field_types.len()
        );
        unsafe {
            TypeBuilderSetStructType(
                self.r,
                index,
                field_types.as_ptr() as *mut BinaryenType, // Type is repr(transparent) for BinaryenType
                field_packed_types.as_ptr() as *mut BinaryenPackedType, // PackedType is repr(u32) and BinaryenPackedType is u32
                field_mutables.as_ptr() as *mut bool,
                field_mutables.len() as i32,
            )
        }
    }

    // consumes self, because the builder is deleted
    pub fn build(self) -> Result<Vec<HeapType>, (BinaryenIndex, TypeBuilderErrorReason)> {
        let mut error_index: BinaryenIndex = 0;
        let mut error_reason: TypeBuilderErrorReason = 0;
        let mut heap_types: Vec<HeapType> = Vec::new();
        heap_types.resize(self.get_size() as usize, HeapType { id: 0 });
        let x = unsafe {
            TypeBuilderBuildAndDispose(
                self.r,
                heap_types.as_mut_ptr() as *mut usize,
                &mut error_index,
                &mut error_reason,
            )
        };
        if x {
            Ok(heap_types)
        } else {
            Err((error_index, error_reason))
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

    let mut file = File::create("hello_world.wasm").unwrap();
    file.write_all(&module.compile()).unwrap();
}
