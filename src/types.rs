use binaryen_sys::*;
use bitflags::bitflags;
use std::{ffi::CStr, fmt::Debug, mem::transmute};
#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct Type {
    pub(crate) id: BinaryenType,
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
fn type_builder() {
    let mut _builder = TypeBuilder::new(4);
}
