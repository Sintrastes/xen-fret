/// Events produced by a microphone stream.
#[derive(Debug)]
pub enum MicEvent {
    Started { sample_rate: u32 },
    Samples { rate: u32, data: Vec<f32> },
    Stopped,
    Error(String),
}
