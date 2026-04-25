pub mod sequencer;
pub mod backend;
pub mod mic;

#[cfg(feature = "web")]
pub mod web_audio;

#[cfg(feature = "native")]
pub mod native_audio;

#[cfg(feature = "native")]
pub mod native_mic;

// Back-compat re-export so existing `xen_sequencer::audio::*` call sites keep working on web.
#[cfg(feature = "web")]
pub use web_audio as audio;
