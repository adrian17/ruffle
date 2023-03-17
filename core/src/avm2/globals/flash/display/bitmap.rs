//! `flash.display.Bitmap` builtin/prototype

use crate::avm2::activation::Activation;
use crate::avm2::globals::flash::display::bitmap_data::fill_bitmap_data_from_symbol;
use crate::avm2::object::{BitmapDataObject, Object, TObject, ClassObject, StageObject};
use crate::avm2::value::Value;
use crate::avm2::Error;

use crate::bitmap::bitmap_data::BitmapData;
use crate::character::Character;
use crate::display_object::{TDisplayObject, Bitmap};
use crate::{avm2_stub_getter, avm2_stub_setter};
use gc_arena::GcCell;

pub fn bitmap_allocator<'gc>(
    class: ClassObject<'gc>,
    activation: &mut Activation<'_, 'gc>,
) -> Result<Object<'gc>, Error<'gc>> {

    use crate::frame_lifecycle::catchup_display_object_to_frame;
    use crate::vminterface::Instantiator;

    let bitmap_cls = activation.avm2().classes().bitmap;
    let bitmapdata_cls = activation.context.avm2.classes().bitmapdata;

    let mut class_object = Some(class);
    let orig_class = class;
    while let Some(class) = class_object {

        if class == bitmap_cls {
            let bitmap_data = GcCell::allocate(activation.context.gc_context, BitmapData::dummy());
            let mut display_object = Bitmap::new_with_bitmap_data(&mut activation.context, 0, bitmap_data, false).into();
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
            if let Some(Character::Bitmap(bitmap)) = activation
                .context
                .library
                .library_for_movie_mut(movie)
                .character_by_id(symbol)
                .cloned()
            {
                // copy the data, so we don't share mutations
                let bitmap_data = GcCell::allocate(activation.context.gc_context, BitmapData::default());
                fill_bitmap_data_from_symbol(activation, &bitmap, bitmap_data);
                let bitmap_data_obj = BitmapDataObject::from_bitmap_data(
                    activation,
                    bitmap_data,
                    bitmapdata_cls
                )?;

                let dummy_bitmap_data = GcCell::allocate(activation.context.gc_context, BitmapData::dummy());
                let mut child = Bitmap::new_with_bitmap_data(&mut activation.context, 0, dummy_bitmap_data, false).into();

                let mut obj = StageObject::for_display_object(activation, child, orig_class)?;
                child.set_object2(activation.context.gc_context, obj.into());

                obj.set_public_property("bitmapData", bitmap_data_obj.into(), activation)?;

                // [NA] Should these run for everything?
                child.post_instantiation(&mut activation.context, None, Instantiator::Avm2, false);
                catchup_display_object_to_frame(&mut activation.context, child);

                return Ok(obj.into());
            }

        }
        class_object = class.superclass_object();
    }
    unreachable!("A Bitmap subclass should have Bitmap in superclass chain");
}

/// Implements `flash.display.Bitmap`'s `init` method, which is called from the constructor
pub fn init<'gc>(
    activation: &mut Activation<'_, 'gc>,
    this: Option<Object<'gc>>,
    args: &[Value<'gc>],
) -> Result<Value<'gc>, Error<'gc>> {
    if let Some(mut this) = this {
        activation.super_init(this, &[])?;

        let bitmap_data = args
            .get(0)
            .cloned()
            .unwrap_or(Value::Null)
            .as_object()
            .and_then(|bd| bd.as_bitmap_data());
        //TODO: Pixel snapping is not supported
        let _pixel_snapping = args
            .get(1)
            .cloned()
            .unwrap_or_else(|| "auto".into())
            .coerce_to_string(activation)?;
        let smoothing = args
            .get(2)
            .cloned()
            .unwrap_or_else(|| false.into())
            .coerce_to_boolean();

        if let Some(bitmap) = this.as_display_object().and_then(|dobj| dobj.as_bitmap()) {
            if let Some(bitmap_data) = bitmap_data {
                bitmap.set_bitmap_data(&mut activation.context, bitmap_data);
            }
            bitmap.set_smoothing(activation.context.gc_context, smoothing);
        } else {
            unreachable!();
        }
    }

    Ok(Value::Undefined)
}

/// Implements `Bitmap.bitmapData`'s getter.
pub fn get_bitmap_data<'gc>(
    _activation: &mut Activation<'_, 'gc>,
    this: Option<Object<'gc>>,
    _args: &[Value<'gc>],
) -> Result<Value<'gc>, Error<'gc>> {
    if let Some(bitmap) = this
        .and_then(|this| this.as_display_object())
        .and_then(|dobj| dobj.as_bitmap())
    {
        let mut value = bitmap.bitmap_data_wrapper().object2();

        // AS3 expects an unset BitmapData to be null, not 'undefined'
        if matches!(value, Value::Undefined) {
            value = Value::Null;
        }
        return Ok(value);
    }

    Ok(Value::Undefined)
}

/// Implements `Bitmap.bitmapData`'s setter.
pub fn set_bitmap_data<'gc>(
    activation: &mut Activation<'_, 'gc>,
    this: Option<Object<'gc>>,
    args: &[Value<'gc>],
) -> Result<Value<'gc>, Error<'gc>> {
    if let Some(bitmap) = this
        .and_then(|this| this.as_display_object())
        .and_then(|dobj| dobj.as_bitmap())
    {
        let bitmap_data = args.get(0).unwrap_or(&Value::Null);
        let bitmap_data = if matches!(bitmap_data, Value::Null) {
            GcCell::allocate(activation.context.gc_context, BitmapData::dummy())
        } else {
            bitmap_data
                .coerce_to_object(activation)?
                .as_bitmap_data()
                .ok_or_else(|| Error::RustError("Argument was not a BitmapData".into()))?
        };
        bitmap.set_bitmap_data(&mut activation.context, bitmap_data);
    }

    Ok(Value::Undefined)
}

/// Stub `Bitmap.pixelSnapping`'s getter
pub fn get_pixel_snapping<'gc>(
    activation: &mut Activation<'_, 'gc>,
    _this: Option<Object<'gc>>,
    _args: &[Value<'gc>],
) -> Result<Value<'gc>, Error<'gc>> {
    avm2_stub_getter!(activation, "flash.display.Bitmap", "pixelSnapping");
    Ok("auto".into())
}

/// Stub `Bitmap.pixelSnapping`'s setter
pub fn set_pixel_snapping<'gc>(
    activation: &mut Activation<'_, 'gc>,
    _this: Option<Object<'gc>>,
    _args: &[Value<'gc>],
) -> Result<Value<'gc>, Error<'gc>> {
    avm2_stub_setter!(activation, "flash.display.Bitmap", "pixelSnapping");
    Ok(Value::Undefined)
}

/// Implement `Bitmap.smoothing`'s getter
pub fn get_smoothing<'gc>(
    _activation: &mut Activation<'_, 'gc>,
    this: Option<Object<'gc>>,
    _args: &[Value<'gc>],
) -> Result<Value<'gc>, Error<'gc>> {
    if let Some(bitmap) = this
        .and_then(|this| this.as_display_object())
        .and_then(|dobj| dobj.as_bitmap())
    {
        return Ok(bitmap.smoothing().into());
    }

    Ok(Value::Undefined)
}

/// Implement `Bitmap.smoothing`'s setter
pub fn set_smoothing<'gc>(
    activation: &mut Activation<'_, 'gc>,
    this: Option<Object<'gc>>,
    args: &[Value<'gc>],
) -> Result<Value<'gc>, Error<'gc>> {
    if let Some(bitmap) = this
        .and_then(|this| this.as_display_object())
        .and_then(|dobj| dobj.as_bitmap())
    {
        let smoothing = args.get(0).unwrap_or(&Value::Undefined).coerce_to_boolean();
        bitmap.set_smoothing(activation.context.gc_context, smoothing);
    }

    Ok(Value::Undefined)
}
