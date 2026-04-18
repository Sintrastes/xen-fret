use dioxus::prelude::*;

pub use app_common::state::{AppState, DiagramMode};

// GlobalSignal: accessible from any component without context setup.
// Reading it inside a component subscribes that component to re-render on changes.
pub static APP_STATE: GlobalSignal<AppState> =
    Signal::global(|| app_common::storage::load());
