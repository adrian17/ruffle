use ruffle_core::backend::log::LogBackend;
use ruffle_core::backend::navigator::NullExecutor;
use ruffle_core::tag_utils::SwfMovie;
use ruffle_core::PlayerBuilder;
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
    let args: Vec<String> = std::env::args().collect();
    let out = run_swf(&args[1], 1)?;
    println!("{}", out);

    Ok(())
}

fn run_swf(swf_path: &str, num_frames: u32) -> Result<String, Box<dyn Error>> {
    let base_path = Path::new(swf_path).parent().unwrap();
    let executor = NullExecutor::new();
    let movie = SwfMovie::from_path(swf_path, None)?;
    let frame_time = 1000.0 / movie.frame_rate().to_f64();
    let trace_output = Rc::new(RefCell::new(Vec::new()));

    let builder = PlayerBuilder::new()
        .with_log(TestLogBackend::new(trace_output.clone()));

    let player = builder.build();

    player.lock().unwrap().set_root_movie(movie);
    player
        .lock()
        .unwrap()
        .set_max_execution_duration(Duration::from_secs(300));

    for _ in 0..num_frames {
        player.lock().unwrap().run_frame();
        player.lock().unwrap().update_timers(frame_time);
        //executor.poll_all().unwrap();
    }

    //executor.block_all().unwrap();

    let trace = trace_output.borrow().join("\n");
    Ok(trace)
}
