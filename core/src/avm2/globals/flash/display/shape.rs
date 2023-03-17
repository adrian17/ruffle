//! `flash.display.Shape` builtin/prototype

use crate::avm2::activation::Activation;
use crate::avm2::object::{Object, StageObject, TObject, ClassObject};
use crate::avm2::value::Value;
use crate::avm2::Error;
use crate::avm2::Multiname;
use crate::display_object::{Graphic, TDisplayObject};

pub fn shape_allocator<'gc>(
    class: ClassObject<'gc>,
    activation: &mut Activation<'_, 'gc>,
) -> Result<Object<'gc>, Error<'gc>> {

    use crate::frame_lifecycle::catchup_display_object_to_frame;
    use crate::vminterface::Instantiator;

    let shape_cls = activation.avm2().classes().shape;

    let mut class_object = Some(class);
    let orig_class = class;
    while let Some(class) = class_object {

        if class == shape_cls {
            let mut display_object = Graphic::new_with_avm2(&mut activation.context).into();
            let obj = StageObject::for_display_object(activation, display_object, orig_class)?;
            display_object.set_object2(activation.context.gc_context, obj.into());
            return Ok(obj.into());
        }

        if let Some((movie, symbol)) = activation
            .context
            .library
            .avm2_class_registry()
            .class_symbol(class)
        {
            let mut child = activation
                .context
                .library
                .library_for_movie_mut(movie)
                .instantiate_by_id(symbol, activation.context.gc_context)?;

            let obj = StageObject::for_display_object(activation, child, orig_class)?;
            child.set_object2(activation.context.gc_context, obj.into());

            // [NA] Should these run for everything?
            child.post_instantiation(&mut activation.context, None, Instantiator::Avm2, false);
            catchup_display_object_to_frame(&mut activation.context, child);

            return Ok(obj.into());
        }
        class_object = class.superclass_object();
    }
    unreachable!("A Shape subclass should have Shape in superclass chain");
}

/// Implements `flash.display.Shape`'s 'init' method, which is called from the constructor
pub fn init<'gc>(
    activation: &mut Activation<'_, 'gc>,
    this: Option<Object<'gc>>,
    _args: &[Value<'gc>],
) -> Result<Value<'gc>, Error<'gc>> {
    if let Some(this) = this {
        activation.super_init(this, &[])?;
    }

    Ok(Value::Undefined)
}

/// Implements `graphics`.
pub fn get_graphics<'gc>(
    activation: &mut Activation<'_, 'gc>,
    this: Option<Object<'gc>>,
    _args: &[Value<'gc>],
) -> Result<Value<'gc>, Error<'gc>> {
    if let Some(mut this) = this {
        if let Some(dobj) = this.as_display_object() {
            // Lazily initialize the `Graphics` object in a hidden property.
            let graphics = match this.get_property(
                &Multiname::new(activation.avm2().flash_display_internal, "_graphics"),
                activation,
            )? {
                Value::Undefined | Value::Null => {
                    let graphics = Value::from(StageObject::graphics(activation, dobj)?);
                    this.set_property(
                        &Multiname::new(activation.avm2().flash_display_internal, "_graphics"),
                        graphics,
                        activation,
                    )?;
                    graphics
                }
                graphics => graphics,
            };
            return Ok(graphics);
        }
    }

    Ok(Value::Undefined)
}
