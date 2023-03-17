//! `flash.display.Loader` builtin/prototype

use crate::avm2::activation::Activation;
use crate::avm2::object::LoaderInfoObject;
use crate::avm2::object::{TObject, ClassObject, StageObject};
use crate::avm2::value::Value;
use crate::avm2::Multiname;
use crate::avm2::{Error, Object};
use crate::backend::navigator::Request;
use crate::display_object::{MovieClip, TDisplayObject, LoaderDisplay};
use crate::loader::MovieLoaderEventHandler;
use crate::tag_utils::SwfMovie;
use std::sync::Arc;

pub fn loader_allocator<'gc>(
    class: ClassObject<'gc>,
    activation: &mut Activation<'_, 'gc>,
) -> Result<Object<'gc>, Error<'gc>> {

    use crate::frame_lifecycle::catchup_display_object_to_frame;
    use crate::vminterface::Instantiator;

    let loader_cls = activation.avm2().classes().loader;

    let mut class_object = Some(class);
    let orig_class = class;
    while let Some(class) = class_object {

        if class == loader_cls {
            let mut display_object = LoaderDisplay::new_with_avm2(
                activation.context.gc_context,
                activation.context.swf.clone(),
            ).into();
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
    unreachable!("A Loader subclass should have Loader in superclass chain");
}

pub fn init<'gc>(
    activation: &mut Activation<'_, 'gc>,
    this: Option<Object<'gc>>,
    _args: &[Value<'gc>],
) -> Result<Value<'gc>, Error<'gc>> {
    if let Some(mut this) = this {
        // Some LoaderInfo properties (such as 'bytesLoaded' and 'bytesTotal') are always
        // accessible, even before the 'init' event has fired. Using an empty movie gives
        // us the correct value (0) for them.
        let loader_info = LoaderInfoObject::not_yet_loaded(
            activation,
            Arc::new(SwfMovie::empty(activation.context.swf.version())),
            Some(this),
            None,
            false,
        )?;
        this.set_property(
            &Multiname::new(
                activation.avm2().flash_display_internal,
                "_contentLoaderInfo",
            ),
            loader_info.into(),
            activation,
        )?;
    }

    Ok(Value::Undefined)
}

pub fn load<'gc>(
    activation: &mut Activation<'_, 'gc>,
    this: Option<Object<'gc>>,
    args: &[Value<'gc>],
) -> Result<Value<'gc>, Error<'gc>> {
    if let Some(this) = this {
        let url_request = args[0].as_object().unwrap();
        let context = args
            .get(1)
            .and_then(|v| v.coerce_to_object(activation).ok());

        let url = url_request
            .get_public_property("url", activation)?
            .coerce_to_string(activation)?;

        // This is a dummy MovieClip, which will get overwritten in `Loader`
        let content = MovieClip::new(
            Arc::new(SwfMovie::empty(activation.context.swf.version())),
            activation.context.gc_context,
        );

        let loader_info = this
            .get_property(
                &Multiname::new(
                    activation.avm2().flash_display_internal,
                    "_contentLoaderInfo",
                ),
                activation,
            )?
            .as_object()
            .unwrap();

        let future = activation.context.load_manager.load_movie_into_clip(
            activation.context.player.clone(),
            content.into(),
            // FIXME - set options from the `URLRequest`
            Request::get(url.to_string()),
            Some(url.to_string()),
            Some(MovieLoaderEventHandler::Avm2LoaderInfo(loader_info)),
            context,
        );
        activation.context.navigator.spawn_future(future);
    }
    Ok(Value::Undefined)
}

pub fn load_bytes<'gc>(
    activation: &mut Activation<'_, 'gc>,
    this: Option<Object<'gc>>,
    args: &[Value<'gc>],
) -> Result<Value<'gc>, Error<'gc>> {
    if let Some(this) = this {
        let arg0 = args[0].as_object().unwrap();
        let bytearray = arg0.as_bytearray().unwrap();
        let context = args
            .get(1)
            .and_then(|v| v.coerce_to_object(activation).ok());

        // This is a dummy MovieClip, which will get overwritten in `Loader`
        let content = MovieClip::new(
            Arc::new(SwfMovie::empty(activation.context.swf.version())),
            activation.context.gc_context,
        );

        let loader_info = this
            .get_property(
                &Multiname::new(
                    activation.avm2().flash_display_internal,
                    "_contentLoaderInfo",
                ),
                activation,
            )?
            .as_object()
            .unwrap();

        let future = activation.context.load_manager.load_movie_into_clip_bytes(
            activation.context.player.clone(),
            content.into(),
            bytearray.bytes().to_vec(),
            Some(MovieLoaderEventHandler::Avm2LoaderInfo(loader_info)),
            context,
        );
        activation.context.navigator.spawn_future(future);
    }
    Ok(Value::Undefined)
}
