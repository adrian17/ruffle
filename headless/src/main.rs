use ruffle_core::backend::audio::NullAudioBackend;
use ruffle_core::backend::locale::NullLocaleBackend;
use ruffle_core::backend::log::LogBackend;
use ruffle_core::backend::navigator::NullExecutor;
use ruffle_core::backend::navigator::NullNavigatorBackend;
use ruffle_core::backend::render::NullRenderer;
use ruffle_core::backend::storage::MemoryStorageBackend;
use ruffle_core::backend::ui::NullUiBackend;
use ruffle_core::backend::video::NullVideoBackend;
use ruffle_core::tag_utils::SwfMovie;
use ruffle_core::Player;
use std::cell::RefCell;
use std::error::Error;
use std::path::Path;
use std::rc::Rc;
use std::sync::Arc;
use std::time::Duration;

struct TestLogBackend {
    trace_output: Rc<RefCell<Vec<String>>>,
}

impl TestLogBackend {
    pub fn new(trace_output: Rc<RefCell<Vec<String>>>) -> Self {
        Self { trace_output }
    }
}

impl LogBackend for TestLogBackend {
    fn avm_trace(&self, message: &str) {
        self.trace_output.borrow_mut().push(message.to_string());
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let out = run_swf("/home/adrian/ffdec/swfs/buddy.swf", 1)?;
    println!("{:?}", out);

    Ok(())
}

fn run_swf(swf_path: &str, num_frames: u32) -> Result<String, Box<dyn Error>> {
    let base_path = Path::new(swf_path).parent().unwrap();
    let (mut executor, channel) = NullExecutor::new();
    let movie = SwfMovie::from_path(swf_path, None)?;
    let frame_time = 1000.0 / movie.header().frame_rate as f64;
    let trace_output = Rc::new(RefCell::new(Vec::new()));

    let player = Player::new(
        Box::new(NullRenderer),
        Box::new(NullAudioBackend::new()),
        Box::new(NullNavigatorBackend::with_base_path(base_path, channel)),
        Box::new(MemoryStorageBackend::default()),
        Box::new(NullLocaleBackend::new()),
        Box::new(NullVideoBackend::new()),
        Box::new(TestLogBackend::new(trace_output.clone())),
        Box::new(NullUiBackend::new()),
    )?;
    player.lock().unwrap().set_root_movie(Arc::new(movie));
    player
        .lock()
        .unwrap()
        .set_max_execution_duration(Duration::from_secs(300));

    for _ in 0..num_frames {
        player.lock().unwrap().run_frame();
        player.lock().unwrap().update_timers(frame_time);
        executor.poll_all().unwrap();
    }

    executor.block_all().unwrap();

    let trace = trace_output.borrow().join("\n");
    Ok(trace)
}
