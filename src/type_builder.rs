use crate::types::{HeapType, PackedType, Type};
use binaryen_sys::*;
use std::mem::transmute;

/// TypeBuilder - allows for the construction of recursive types. Contains a
/// table of `n` mutable HeapTypes and can construct temporary types that are
/// backed by those HeapTypes, referring to them by reference. Those temporary
/// types are owned by the TypeBuilder and should only be used in the
/// construction of HeapTypes to insert into the TypeBuilder. Temporary types
/// should never be used in the construction of normal Types, only other
/// temporary types.
pub struct TypeBuilder {
    r: TypeBuilderRef,
}

impl TypeBuilder {
    /// Construct a new type builder with `n` uninitialized HeapType slots.
    pub fn new(n: u32) -> Self {
        TypeBuilder {
            r: unsafe { TypeBuilderCreate(n) },
        }
    }

    /// Append `n` new uninitialized HeapType slots to the end of the TypeBuilder.
    pub fn grow(&mut self, increase: u32) {
        unsafe { TypeBuilderGrow(self.r, increase) }
    }

    /// The number of HeapType slots in the TypeBuilder.
    pub fn size(&self) -> u32 {
        unsafe { TypeBuilderGetSize(self.r) }
    }

    pub fn create_rec_group(&self, index: u32, length: u32) {
        unsafe { TypeBuilderCreateRecGroup(self.r, index, length) };
    }

    /// Sets the heap type at index `i`.
    pub fn set_heap_type(&mut self, index: u32, thing: Thing) {
        match thing {
            Thing::Signature { params, results } => {
                unsafe { TypeBuilderSetSignatureType(self.r, index, params.id, results.id) };
            }
            Thing::Struct { fields } => {
                let field_types = fields.iter().map(|field| field.typ).collect::<Vec<_>>();
                let field_packed_types = fields
                    .iter()
                    .map(|field| field.packed_type)
                    .collect::<Vec<_>>();
                let field_mutables = fields
                    .iter()
                    .map(|field: &Field| field.mutable)
                    .collect::<Vec<_>>();
                self.set_struct_type(index, &field_types, &field_packed_types, &field_mutables);
            }
            Thing::Array { element } => {
                unsafe {
                    TypeBuilderSetArrayType(
                        self.r,
                        index,
                        element.typ.id,
                        element.packed_type as u32,
                        element.mutable as i32,
                    )
                };
            }
        }
    }

    fn set_struct_type(
        &mut self,
        index: u32,
        field_types: &[Type],
        field_packed_types: &[PackedType],
        field_mutables: &[bool],
    ) {
        assert!(
            index < self.size()
                && field_mutables.len() == field_types.len()
                && field_packed_types.len() == field_types.len()
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

    /// Gets the temporary HeapType at index `i`. This HeapType should only be used
    /// to construct temporary Types using the other methods TypeBuilder.
    pub fn get_temp_heap_type(&self, i: u32) -> HeapType {
        unsafe {
            HeapType {
                id: TypeBuilderGetTempHeapType(self.r, i),
            }
        }
    }

    /// Gets a temporary reference type for use in initializing the
    /// TypeBuilder's HeapTypes. The HeapType may be a temporary
    /// HeapType owned by this builder or a canonical HeapType.
    pub fn get_temp_ref_type(&self, heap_type: HeapType, nullable: bool) -> Type {
        unsafe {
            Type {
                id: TypeBuilderGetTempRefType(self.r, heap_type.id, nullable as i32),
            }
        }
    }

    /// Gets a temporary tuple type for use in initializing the
    /// TypeBuilder's HeapTypes.
    pub fn get_temp_tuple_type(&self, types: &[Type]) -> Type {
        unsafe {
            Type {
                id: TypeBuilderGetTempTupleType(
                    self.r,
                    transmute(types.as_ptr()),
                    types.len() as u32,
                ),
            }
        }
    }

    /// In nominal mode, or for nominal types, declare the HeapType being built at
    /// index `i` to be an immediate subtype of the given HeapType. Does nothing
    /// for equirecursive types.
    pub fn set_sub_type(&mut self, i: u32, super_type: HeapType) {
        unsafe { TypeBuilderSetSubType(self.r, i, super_type.id) }
    }

    // consumes self, because the builder is deleted
    pub fn build(self) -> Result<Vec<HeapType>, (BinaryenIndex, TypeBuilderErrorReason)> {
        let mut error_index: BinaryenIndex = 0;
        let mut error_reason: TypeBuilderErrorReason = 0;
        let mut heap_types: Vec<HeapType> = Vec::new();
        heap_types.resize(self.size() as usize, HeapType::none());
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

pub struct Field {
    typ: Type,
    packed_type: PackedType,
    mutable: bool,
}

impl Field {
    pub fn new(typ: Type, mutable: bool) -> Self {
        Self {
            typ,
            packed_type: PackedType::NotPacked,
            mutable,
        }
    }

    pub fn packed(typ: Type, mutable: bool, packed_type: PackedType) -> Self {
        Self {
            typ,
            packed_type,
            mutable,
        }
    }
}

pub enum Thing {
    Signature { params: Type, results: Type },
    Struct { fields: Vec<Field> },
    Array { element: Field },
}

#[test]
fn test_type_builder_growth() {
    let mut builder = TypeBuilder::new(0);
    assert_eq!(builder.size(), 0);
    builder.grow(3);
    assert_eq!(builder.size(), 3);
    builder.grow(0);
    assert_eq!(builder.size(), 3);
}

#[test]
fn test_type_builder_super_basic() {
    let mut builder = TypeBuilder::new(1);
    let ref_node = builder.get_temp_ref_type(builder.get_temp_heap_type(0), true);
    builder.set_heap_type(
        0,
        Thing::Struct {
            fields: vec![Field::new(Type::int32(), true), Field::new(ref_node, true)],
        },
    );
    builder.create_rec_group(0, 1);
    let result = builder.build().unwrap();

    assert_eq!(
        format!("{:?}", result),
        "[(type $struct.0 (struct (field (mut i32)) (field (mut (ref null $struct.0)))))]"
    );
}

#[test]
fn test_type_builder_basic() {
    let mut builder = TypeBuilder::new(3);
    assert_eq!(builder.size(), 3);

    let ref_sig = builder.get_temp_ref_type(builder.get_temp_heap_type(0), false);
    let ref_struct = builder.get_temp_ref_type(builder.get_temp_heap_type(1), false);
    let ref_array = builder.get_temp_ref_type(builder.get_temp_heap_type(2), false);
    let ref_null_array = builder.get_temp_ref_type(builder.get_temp_heap_type(2), true);
    let ref_null_any = Type::ref_(HeapType::any(), true);

    let sig = Thing::Signature {
        params: ref_struct,
        results: builder.get_temp_tuple_type(&[ref_array, Type::int32()]),
    };
    let struct_ = Thing::Struct {
        fields: vec![Field::new(ref_struct, false)],
    };
    let array = Thing::Array {
        element: Field::new(ref_null_any, true),
    };

    builder.set_heap_type(0, sig);
    builder.set_heap_type(1, struct_);
    builder.set_heap_type(2, array);

    builder.create_rec_group(0, 3);

    let result = builder.build().unwrap();
    assert_eq!(format!("{:?}", result), "[(type $func.0 (func (param (ref $struct.0)) (result (ref $array.0) i32))), (type $struct.0 (struct (field (ref $struct.0)))), (type $array.0 (array (mut anyref)))]");
}
