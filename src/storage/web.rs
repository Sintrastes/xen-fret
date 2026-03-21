use crate::state::AppState;

fn local_storage() -> Option<web_sys::Storage> {
    web_sys::window()?.local_storage().ok()?
}

pub fn load() -> AppState {
    let Some(ls) = local_storage() else { return AppState::default() };

    let mut state = AppState::default();

    macro_rules! load_key {
        ($key:expr, $field:expr) => {
            if let Ok(Some(s)) = ls.get_item($key) {
                if let Ok(v) = serde_json::from_str(&s) {
                    $field = v;
                }
            }
        };
    }

    load_key!("xen-fret:temperaments",     state.temperaments);
    load_key!("xen-fret:notation_systems", state.notation_systems);
    load_key!("xen-fret:tunings",          state.tunings);
    load_key!("xen-fret:scales",           state.scales);
    load_key!("xen-fret:chords",           state.chords);
    load_key!("xen-fret:instruments",      state.instruments);
    load_key!("xen-fret:preferences",      state.preferences);

    state.restore_selections();
    state
}

pub fn save(state: &AppState) {
    let Some(ls) = local_storage() else { return };

    macro_rules! save_key {
        ($key:expr, $val:expr) => {
            if let Ok(s) = serde_json::to_string($val) {
                let _ = ls.set_item($key, &s);
            }
        };
    }

    save_key!("xen-fret:temperaments",     &state.temperaments);
    save_key!("xen-fret:notation_systems", &state.notation_systems);
    save_key!("xen-fret:tunings",          &state.tunings);
    save_key!("xen-fret:scales",           &state.scales);
    save_key!("xen-fret:chords",           &state.chords);
    save_key!("xen-fret:instruments",      &state.instruments);
    save_key!("xen-fret:preferences",      &state.preferences);
}
