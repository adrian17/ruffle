use crate::avm1::activation::Activation;
use crate::avm1::error::Error;
use crate::avm1::globals::system::SystemProperties;
use crate::avm1::{Avm1, Object, UpdateContext};
use crate::backend::audio::NullAudioBackend;
use crate::backend::input::NullInputBackend;
use crate::backend::navigator::NullNavigatorBackend;
use crate::backend::render::NullRenderer;
use crate::backend::storage::MemoryStorageBackend;
use crate::context::ActionQueue;
use crate::display_object::{MovieClip, TDisplayObject};
use crate::library::Library;
use crate::loader::LoadManager;
use crate::prelude::*;
use crate::tag_utils::{SwfMovie, SwfSlice};
use gc_arena::{rootless_arena, MutationContext};
use rand::{rngs::SmallRng, SeedableRng};
use std::collections::{BTreeMap, HashMap};
use std::sync::Arc;

pub fn with_avm<F>(swf_version: u8, test: F)
where
    F: for<'a, 'gc> FnOnce(
        &mut Activation<'_, 'gc>,
        &mut UpdateContext<'a, 'gc, '_>,
        Object<'gc>,
    ) -> Result<(), Error<'gc>>,
{
    fn in_the_arena<'a, 'gc: 'a, F>(swf_version: u8, test: F, gc_context: MutationContext<'gc, '_>)
    where
        F: FnOnce(
            &mut Activation<'_, 'gc>,
            &mut UpdateContext<'_, 'gc, '_>,
            Object<'gc>,
        ) -> Result<(), Error<'gc>>,
    {
        let mut avm = Avm1::new(gc_context, swf_version);
        let swf = Arc::new(SwfMovie::empty(swf_version));
        let mut root: DisplayObject<'gc> =
            MovieClip::new(SwfSlice::empty(swf.clone()), gc_context).into();
        root.set_depth(gc_context, 0);
        let mut levels = BTreeMap::new();
        levels.insert(0, root);

        let mut context = UpdateContext {
            gc_context,
            global_time: 0,
            player_version: 32,
            swf: &swf,
            levels: &mut levels,
            rng: &mut SmallRng::from_seed([0u8; 16]),
            audio: &mut NullAudioBackend::new(),
            input: &mut NullInputBackend::new(),
            action_queue: &mut ActionQueue::new(),
            background_color: &mut Color {
                r: 0,
                g: 0,
                b: 0,
                a: 0,
            },
            library: &mut Library::default(),
            navigator: &mut NullNavigatorBackend::new(),
            renderer: &mut NullRenderer::new(),
            system_prototypes: avm.prototypes().clone(),
            mouse_hovered_object: None,
            mouse_position: &(Twips::new(0), Twips::new(0)),
            drag_object: &mut None,
            stage_size: (Twips::from_pixels(550.0), Twips::from_pixels(400.0)),
            player: None,
            load_manager: &mut LoadManager::new(),
            system: &mut SystemProperties::default(),
            instance_counter: &mut 0,
            storage: &mut MemoryStorageBackend::default(),
            shared_objects: &mut HashMap::new(),
            unbound_text_fields: &mut Vec::new(),
        };
        root.post_instantiation(&mut avm, &mut context, root, None, false);
        root.set_name(context.gc_context, "");

        fn run_test<'a, 'gc: 'a, F>(
            activation: &mut Activation<'_, 'gc>,
            context: &mut UpdateContext<'_, 'gc, '_>,
            root: DisplayObject<'gc>,
            test: F,
        ) where
            F: FnOnce(
                &mut Activation<'_, 'gc>,
                &mut UpdateContext<'_, 'gc, '_>,
                Object<'gc>,
            ) -> Result<(), Error<'gc>>,
        {
            let this = root.object().coerce_to_object(activation, context);
            let result = test(activation, context, this);
            if let Err(e) = result {
                panic!("Encountered exception during test: {}", e);
            }
        }

        let globals = avm.global_object_cell();
        let mut activation = Activation::from_nothing(
            &mut avm,
            context.swf.version(),
            globals,
            context.gc_context,
            *context.levels.get(&0).unwrap(),
        );

        run_test(&mut activation, &mut context, root, test)
    }

    rootless_arena(|gc_context| in_the_arena(swf_version, test, gc_context))
}

macro_rules! test_method {
    ( $test: ident, $name: expr, $object: expr, $($versions: expr => { $([$($arg: expr),*] => $out: expr),* }),* ) => {
        #[test]
        fn $test() {
            use $crate::avm1::test_utils::*;
            $(
                for version in &$versions {
                    with_avm(*version, |activation, context, _root| -> Result<(), Error> {
                        let object = $object(activation, context);
                        let function = object.get($name, activation, context)?;

                        $(
                            #[allow(unused_mut)]
                            let mut args: Vec<Value> = Vec::new();
                            $(
                                args.push($arg.into());
                            )*
                            assert_eq!(function.call(activation, context, object, None, &args)?, $out.into(), "{:?} => {:?} in swf {}", args, $out, version);
                        )*

                        Ok(())
                    });
                }
            )*
        }
    };
}
