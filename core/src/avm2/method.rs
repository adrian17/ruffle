//! AVM2 methods

use crate::avm2::activation::Activation;
use crate::avm2::object::{ClassObject, Object};
use crate::avm2::script::TranslationUnit;
use crate::avm2::value::{abc_default_value, Value};
use crate::avm2::Error;
use crate::avm2::Multiname;
use crate::string::AvmString;
use gc_arena::{Collect, Gc, GcCell, MutationContext};
use std::collections::HashMap;
use std::cell::Cell;
use std::cell::RefCell;
use std::fmt;
use std::ops::Deref;
use std::rc::Rc;
use swf::avm2::types::{
    AbcFile, Index, Method as AbcMethod, MethodBody as AbcMethodBody,
    MethodFlags as AbcMethodFlags, MethodParam as AbcMethodParam,
};

/// Represents a function defined in Ruffle's code.
///
/// Parameters are as follows:
///
///  * The AVM2 runtime
///  * The action context
///  * The current `this` object
///  * The arguments this function was called with
///
/// Native functions are allowed to return a value or `None`. `None` indicates
/// that the given value will not be returned on the stack and instead will
/// resolve on the AVM stack, as if you had called a non-native function. If
/// your function yields `None`, you must ensure that the top-most activation
/// in the AVM1 runtime will return with the value of this function.
pub type NativeMethodImpl = for<'gc> fn(
    &mut Activation<'_, 'gc>,
    Option<Object<'gc>>,
    &[Value<'gc>],
) -> Result<Value<'gc>, Error<'gc>>;

/// Configuration of a single parameter of a method.
#[derive(Clone, Collect, Debug)]
#[collect(no_drop)]
pub struct ParamConfig<'gc> {
    /// The name of the parameter.
    pub param_name: AvmString<'gc>,

    /// The name of the type of the parameter.
    pub param_type_name: Multiname<'gc>,

    /// The default value for this parameter.
    pub default_value: Option<Value<'gc>>,
}

impl<'gc> ParamConfig<'gc> {
    fn from_abc_param(
        config: &AbcMethodParam,
        txunit: TranslationUnit<'gc>,
        activation: &mut Activation<'_, 'gc>,
    ) -> Result<Self, Error<'gc>> {
        let param_name = if let Some(name) = &config.name {
            txunit
                .pool_string(name.0, &mut &mut activation.borrow_gc())?
                .into()
        } else {
            AvmString::from("<Unnamed Parameter>")
        };
        let param_type_name = txunit
            .pool_multiname_static_any(config.kind, &mut &mut activation.borrow_gc())?
            .deref()
            .clone();

        let default_value = if let Some(dv) = &config.default_value {
            Some(abc_default_value(txunit, dv, activation)?)
        } else {
            None
        };

        Ok(Self {
            param_name,
            param_type_name,
            default_value,
        })
    }

    pub fn of_type(name: impl Into<AvmString<'gc>>, param_type_name: Multiname<'gc>) -> Self {
        Self {
            param_name: name.into(),
            param_type_name,
            default_value: None,
        }
    }

    pub fn optional(
        name: impl Into<AvmString<'gc>>,
        param_type_name: Multiname<'gc>,
        default_value: impl Into<Value<'gc>>,
    ) -> Self {
        Self {
            param_name: name.into(),
            param_type_name,
            default_value: Some(default_value.into()),
        }
    }
}

/// Represents a reference to an AVM2 method and body.
#[derive(Collect, Clone)]
#[collect(no_drop)]
pub struct BytecodeMethod<'gc> {
    /// The translation unit this function was defined in.
    pub txunit: TranslationUnit<'gc>,

    /// The underlying ABC file of the above translation unit.
    #[collect(require_static)]
    pub abc: Rc<AbcFile>,

    /// The ABC method this function uses.
    pub abc_method: u32,

    /// The ABC method body this function uses.
    pub abc_method_body: Option<u32>,

    pub try_optimize: Cell<bool>,
    pub optimized_method_body: RefCell<Option<AbcMethodBody>>,

    /// The parameter signature of this method.
    pub signature: Vec<ParamConfig<'gc>>,

    /// The return type of this method.
    pub return_type: Multiname<'gc>,

    /// The associated activation class. None if not needed. Initialized lazily.
    pub activation_class: Option<GcCell<'gc, Option<ClassObject<'gc>>>>,

    /// Whether or not this method was declared as a free-standing function.
    ///
    /// A free-standing function corresponds to the `Function` trait type, and
    /// is instantiated with the `newfunction` opcode.
    pub is_function: bool,
}

impl<'gc> BytecodeMethod<'gc> {
    /// Construct an `BytecodeMethod` from an `AbcFile` and method index.
    pub fn from_method_index(
        txunit: TranslationUnit<'gc>,
        abc_method: Index<AbcMethod>,
        is_function: bool,
        activation: &mut Activation<'_, 'gc>,
    ) -> Result<Self, Error<'gc>> {
        let abc = txunit.abc();
        let mut signature = Vec::new();

        if abc.methods.get(abc_method.0 as usize).is_some() {
            let method = &abc.methods[abc_method.0 as usize];
            for param in &method.params {
                signature.push(ParamConfig::from_abc_param(param, txunit, activation)?);
            }

            let return_type = txunit
                .pool_multiname_static_any(method.return_type, &mut &mut activation.borrow_gc())?
                .deref()
                .clone();

            let activation_class = if method.flags.contains(AbcMethodFlags::NEED_ACTIVATION) {
                Some(GcCell::allocate(activation.context.gc_context, None))
            } else {
                None
            };

            for (index, method_body) in abc.method_bodies.iter().enumerate() {
                if method_body.method.0 == abc_method.0 {
                    return Ok(Self {
                        txunit,
                        abc: txunit.abc(),
                        abc_method: abc_method.0,
                        abc_method_body: Some(index as u32),
                        try_optimize: Cell::new(true),
                        optimized_method_body: RefCell::new(None),
                        signature,
                        return_type,
                        is_function,
                        activation_class,
                    });
                }
            }
        }

        Ok(Self {
            txunit,
            abc: txunit.abc(),
            abc_method: abc_method.0,
            abc_method_body: None,
            try_optimize: Cell::new(true),
            optimized_method_body: RefCell::new(None),
            signature,
            return_type: Multiname::any(activation.context.gc_context),
            is_function,
            activation_class: None,
        })
    }

    /// Get the underlying ABC file.
    pub fn abc(&self) -> Rc<AbcFile> {
        self.txunit.abc()
    }

    /// Get the underlying translation unit this method was defined in.
    pub fn translation_unit(&self) -> TranslationUnit<'gc> {
        self.txunit
    }

    /// Get a reference to the ABC method entry this refers to.
    pub fn method(&self) -> &AbcMethod {
        self.abc.methods.get(self.abc_method as usize).unwrap()
    }

    /// Get a reference to the ABC method body entry this refers to.
    ///
    /// Some methods do not have bodies; this returns `None` in that case.
    pub fn body(&self) -> Option<&AbcMethodBody> {
        if let Some(abc_method_body) = self.abc_method_body {
            self.abc.method_bodies.get(abc_method_body as usize)
        } else {
            None
        }
    }

    #[inline(never)]
    pub fn optimize(
        &self,
        activation: &mut Activation<'_, 'gc>,
        this_class: Option<ClassObject<'gc>>,
    ) {
        let body = self.body().expect("Cannot execute non-native method without body");

        let mut new_body = body.clone(); // todo skip cloning vec?
        let new_code = &mut new_body.parsed_code;

        use swf::avm2::read::Reader;
        use swf::avm2::types::Op;

        let mut byte_offset_to_idx = HashMap::new();
        let mut idx_to_byte_offset = vec![0];
        byte_offset_to_idx.insert(0, 0);

        use swf::extensions::ReadSwfExt;

        //log::error!("----------");

        let mut reader = Reader::new(&body.code);
        loop {
            let op = if let Ok(op) = reader.read_op() { op } else { break };
            let byte_offset = reader.as_slice().as_ptr() as usize - body.code.as_ptr() as usize;

            new_code.push(op);

            byte_offset_to_idx.insert(byte_offset as i32, new_code.len() as i32);
            idx_to_byte_offset.push(byte_offset as i32);
        }

        // adjust jump offsets from byte-based to idx-based
        for (i, op) in new_code.iter_mut().enumerate() {
            let i = i as i32;
            let adjusted = |i, offset, one_off| {
                //log::error!("at idx {} (byte {}), adjusting offset {}", i, op_byte_offset, offset);
                let byte_offset = if one_off {
                    idx_to_byte_offset.get((i+1) as usize).unwrap()
                }  else {
                    idx_to_byte_offset.get(i as usize).unwrap()
                };
                let new_byte_offset = byte_offset + offset;
                let new_idx = byte_offset_to_idx.get(&new_byte_offset).copied().unwrap_or(i+1); // TODO
                new_idx - i - 1
            };
            match op {
                Op::IfEq { offset }
                    | Op::IfFalse { offset }
                    | Op::IfGe { offset }
                    | Op::IfGt { offset }
                    | Op::IfLe { offset }
                    | Op::IfLt { offset }
                    | Op::IfNe { offset }
                    | Op::IfNge { offset }
                    | Op::IfNgt { offset }
                    | Op::IfNle { offset }
                    | Op::IfNlt { offset }
                    | Op::IfStrictEq { offset }
                    | Op::IfStrictNe { offset }
                    | Op::IfTrue { offset }
                    | Op::Jump { offset } =>
                {
                    *offset = adjusted(i, *offset, true);
                },
                Op::LookupSwitch { default_offset, case_offsets } =>
                {
                    *default_offset = adjusted(i, *default_offset, false);
                    for case in case_offsets.iter_mut() {
                        *case = adjusted(i, *case, false);
                    }
                },
                _ => {}
            }
        }

        // same with exceptions
        for exception in new_body.exceptions.iter_mut() {
            exception.from_offset = byte_offset_to_idx.get(&(exception.from_offset as i32)).copied().unwrap() as u32;
            exception.to_offset = byte_offset_to_idx.get(&(exception.to_offset as i32)).copied().unwrap() as u32;
            exception.target_offset = byte_offset_to_idx.get(&(exception.target_offset as i32)).copied().unwrap() as u32;
        }

        // idea:
        // - record all jump targets (basic block edges)
        // - create a set of valid optimizable locals
        //   - local is optimizable if:
        //     - its setLocal is always preceded by the same cast (and is not a jump target)
        //     - if it's `this` (in an instance method) or typed argument, its type also needs to match
        // - for every getlocal->get/set/init/callproperty pair:
        //   - the *property op can be optimized if:
        //     - local is a valid optimizable local
        //     - the pair doesn't cross a jump target
        //     - the *property op multiname is a static multiname
        //     - the lookup succeeds in a valid replacement
        //     - do a replacement:
        //       - get/set/initproperty on slot -> get/set/init(?)slot
        //       - get/set/initproperty on getter -> callmethod
        //       - callproperty on method -> callmethod
        //   - if the getproperty pair is followed by another *property:
        //     - we can grab the property's type from `slot_classes`
        //     - (again, need to make sure this doesn't cross a jump target)

        // questions:
        //  - are we sure initproperty ops behave the same? Maybe they just get optimized to setslot?

        // missing parts in the current implementation:
        // - anything regarding basic blocks
        //   - more careful validation
        //   - within basic blocks, we could track variables that are set to typed value and immediately
        //     used, even if they aren't typed consistently across entire function, or not at all
        // - a lot of checks end in a panic - they aren't supposed to be generated
        //   by "normal" compilers so MVP treated them as sanity checks,
        //   but generally they are supposed to disable optimizations for the local
        //   (one example is a `coerce->setlocal` to a different type than variable's previous type)
        // - set/initproperty
        // - can there be holes in bytecode? Can we safely read_op() in a loop?

        // longer term:
        // - fancier patterns (`++this.thing`)
        // - pretty sure this is also a good place for verification
        // - refactor to not rely on current Activation state
        //   - (it's extremely ugly that we grab `this_class` from activation
        //      instead from method information itself,
        //      and use runtime type of `this` to check if method is static)
        // - allow generating different opcodes than swf,
        //   and collapsing/expanding opcodes

        use crate::avm2::property::Property;
        use crate::avm2::property::resolve_class_private;
        use crate::avm2::property::PropertyClass;
        use crate::avm2::property::ResolveOutcome;
        use crate::avm2::object::TObject;

        let mut local_types: Vec<Option<ClassObject<'gc>>> = vec![None; body.num_locals as usize];

        if let Some(this_class) = this_class {
            if let Some(this) = activation.this() {
                if this.is_of_type(this_class, &mut activation.context) {
                    // it's a non-static method
                    // (let's ignore freestanding functions or static methods for now)
                    local_types[0] = Some(this_class);
                }
            }
        }

        let signature = self.signature();
        for (i, param) in signature.iter().enumerate() {
            let param_type = activation.resolve_type(&param.param_type_name).unwrap();
            local_types[i+1] = param_type;
        }

        let mut current_type: Option<ClassObject<'gc>> = None;
        for op in new_code.iter() {
            if let Op::Coerce { index: coerce_index } = *op {
                let multiname = self
                    .translation_unit()
                    .pool_maybe_uninitialized_multiname(coerce_index, &mut activation.borrow_gc()).unwrap();
                if !multiname.has_lazy_component() {
                    let cls = activation.resolve_type(&multiname).unwrap();
                    current_type = cls;
                }
            } else if let Op::SetLocal { index: local_index } = *op {
                let local_index = local_index as usize;

                if local_index < signature.len() + 1 {
                    if local_types[local_index] != current_type {
                        // todo: does verifier accept code like this?
                        // if it does, we should just deoptimize (set type to None)
                        //panic!("arg {:?} does not match {:?}", local_types[local_index], current_type);

                        // TODO: this is not correct, it opens the door to overwrite type by a new one
                        local_types[local_index] = None;
                    }
                } else {
                    if local_types[local_index].is_some() && local_types[local_index] != current_type {
                        // todo: does verifier accept code like this?
                        // if it does, we should just deoptimize (set type to None)
                        //panic!("local {:?} does not match {:?}", local_types[local_index], current_type);

                        // TODO: this is not correct, it opens the door to overwrite type by a new one
                        local_types[local_index] = None;
                    } else {
                        // first time or overwriting with the same
                        local_types[local_index] = current_type;
                    }
                }
                current_type = None;
            }
        }

        let mut current_type: Option<ClassObject<'gc>> = None;
        for op in new_code.iter_mut() {
            if let Op::GetLocal { index: local_index } = *op {
                current_type = local_types[local_index as usize];
            } else if let Op::GetProperty { index: name_index } = *op {
                let cls = current_type;
                current_type = None;
                if let Some(cls) = cls {
                    if !cls.inner_class_definition().read().is_interface() {
                        let multiname = self
                            .translation_unit()
                            .pool_maybe_uninitialized_multiname(name_index, &mut activation.borrow_gc()).unwrap();
                        if !multiname.has_lazy_component() {
                            match cls.instance_vtable().get_trait(&multiname) {
                                Some(Property::Slot { slot_id }) | Some(Property::ConstSlot { slot_id }) => {
                                    // todo: I hate this :D
                                    // slot_classes _really_ shouldn't be this lazy,
                                    // they should all be ready by the time class is instantiated, if not earlier
                                    let new_cls = cls.instance_vtable().slot_classes()[slot_id as usize].clone();
                                    match new_cls {
                                        PropertyClass::Class(class) => { current_type = Some(class); },
                                        PropertyClass::Name(gc) => {
                                            let (name, unit) = &*gc;
                                            let outcome = resolve_class_private(name, *unit, activation).unwrap();
                                            if let ResolveOutcome::Class(class) = outcome {
                                                current_type = Some(class);
                                            }
                                        }
                                        PropertyClass::Any => {},
                                    }
                                    *op = Op::GetSlot { index: slot_id };
                                },
                                Some(Property::Virtual { get: Some(get), .. }) => {
                                    *op = Op::CallMethod { num_args: 0, index: Index::new(get) };
                                }
                                _ => {}
                            }
                        }
                    }
                }
            } else {
                current_type = None;
            }
        }

        *self.optimized_method_body.borrow_mut() = Some(new_body);
    }

    /// Get the list of method params for this method.
    pub fn signature(&self) -> &[ParamConfig<'gc>] {
        &self.signature
    }

    /// Get the name of this method.
    pub fn method_name(&self) -> &str {
        let name_index = self.method().name.0 as usize;
        if name_index == 0 {
            return "";
        }

        self.abc
            .constant_pool
            .strings
            .get(name_index - 1)
            .map(|s| s.as_str())
            .unwrap_or("")
    }

    /// Determine if a given method is variadic.
    ///
    /// Variadic methods shove excess parameters into a final register.
    pub fn is_variadic(&self) -> bool {
        self.method()
            .flags
            .intersects(AbcMethodFlags::NEED_ARGUMENTS | AbcMethodFlags::NEED_REST)
    }

    /// Determine if a given method is unchecked.
    ///
    /// A method is unchecked if all of the following are true:
    ///
    ///  * The method was declared as a free-standing function
    ///  * The function does not use rest-parameters
    ///  * The function's parameters have no declared types or default values
    pub fn is_unchecked(&self) -> bool {
        if !self.is_function {
            return false;
        }

        for param in self.signature() {
            if !param.param_type_name.is_any_name() || param.default_value.is_some() {
                return false;
            }
        }

        !self.method().flags.contains(AbcMethodFlags::NEED_REST)
    }
}

/// An uninstantiated method
#[derive(Clone, Collect)]
#[collect(no_drop)]
pub struct NativeMethod<'gc> {
    /// The function to call to execute the method.
    #[collect(require_static)]
    pub method: NativeMethodImpl,

    /// The name of the method.
    pub name: &'static str,

    /// The parameter signature of the method.
    pub signature: Vec<ParamConfig<'gc>>,

    /// The return type of this method.
    pub return_type: Multiname<'gc>,

    /// Whether or not this method accepts parameters beyond those
    /// mentioned in the parameter list.
    pub is_variadic: bool,
}

impl<'gc> fmt::Debug for NativeMethod<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("NativeMethod")
            .field("method", &format!("{:p}", &self.method))
            .field("name", &self.name)
            .field("signature", &self.signature)
            .field("is_variadic", &self.is_variadic)
            .finish()
    }
}

/// An uninstantiated method that can either be natively implemented or sourced
/// from an ABC file.
#[derive(Clone, Collect)]
#[collect(no_drop)]
pub enum Method<'gc> {
    /// A native method.
    Native(Gc<'gc, NativeMethod<'gc>>),

    /// An ABC-provided method entry.
    Bytecode(Gc<'gc, BytecodeMethod<'gc>>),
}

impl<'gc> From<Gc<'gc, BytecodeMethod<'gc>>> for Method<'gc> {
    fn from(bm: Gc<'gc, BytecodeMethod<'gc>>) -> Self {
        Self::Bytecode(bm)
    }
}

impl<'gc> Method<'gc> {
    /// Define a builtin method with a particular param configuration.
    pub fn from_builtin_and_params(
        method: NativeMethodImpl,
        name: &'static str,
        signature: Vec<ParamConfig<'gc>>,
        is_variadic: bool,
        mc: MutationContext<'gc, '_>,
    ) -> Self {
        Self::Native(Gc::allocate(
            mc,
            NativeMethod {
                method,
                name,
                signature,
                // FIXME - take in the real return type. This is needed for 'describeType'
                return_type: Multiname::any(mc),
                is_variadic,
            },
        ))
    }

    /// Define a builtin with no parameter constraints.
    pub fn from_builtin(
        method: NativeMethodImpl,
        name: &'static str,
        mc: MutationContext<'gc, '_>,
    ) -> Self {
        Self::Native(Gc::allocate(
            mc,
            NativeMethod {
                method,
                name,
                signature: Vec::new(),
                // FIXME - take in the real return type. This is needed for 'describeType'
                return_type: Multiname::any(mc),
                is_variadic: true,
            },
        ))
    }

    /// Access the bytecode of this method.
    ///
    /// This function returns `Err` if there is no bytecode for this method.
    pub fn into_bytecode(self) -> Result<Gc<'gc, BytecodeMethod<'gc>>, Error<'gc>> {
        match self {
            Method::Native { .. } => {
                Err("Attempted to unwrap a native method as a user-defined one".into())
            }
            Method::Bytecode(bm) => Ok(bm),
        }
    }

    pub fn return_type(&self) -> Multiname<'gc> {
        match self {
            Method::Native(nm) => nm.return_type.clone(),
            Method::Bytecode(bm) => bm.return_type.clone(),
        }
    }

    pub fn signature(&self) -> &[ParamConfig<'gc>] {
        match self {
            Method::Native(nm) => &nm.signature,
            Method::Bytecode(bm) => bm.signature(),
        }
    }

    pub fn is_variadic(&self) -> bool {
        match self {
            Method::Native(nm) => nm.is_variadic,
            Method::Bytecode(bm) => bm.is_variadic(),
        }
    }

    /// Check if this method needs `arguments`.
    pub fn needs_arguments_object(&self) -> bool {
        match self {
            Method::Native { .. } => false,
            Method::Bytecode(bm) => bm.method().flags.contains(AbcMethodFlags::NEED_ARGUMENTS),
        }
    }
}
