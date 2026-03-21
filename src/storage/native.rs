use std::collections::BTreeMap;
use std::sync::OnceLock;

use cozo::{DataValue, DbInstance, ScriptMutability};

use crate::state::AppState;

static DB: OnceLock<DbInstance> = OnceLock::new();

fn get_db() -> &'static DbInstance {
    DB.get_or_init(|| {
        let dir = dirs::data_dir()
            .unwrap_or_else(|| std::path::PathBuf::from("."))
            .join("xen-fret");
        std::fs::create_dir_all(&dir).ok();
        let db_path = dir.join("xen-fret.db");
        let db = DbInstance::new("sqlite", db_path.to_str().unwrap_or("."), "")
            .expect("Failed to open CozoDB database");
        // Create KV table if it doesn't exist; ignore error if it already exists.
        let _ = db.run_script(
            ":create xf_data { key: String => value: String }",
            Default::default(),
            ScriptMutability::Mutable,
        );
        db
    })
}

fn str_val(s: impl AsRef<str>) -> DataValue {
    DataValue::Str(s.as_ref().into())
}

fn json_str(v: &impl serde::Serialize) -> DataValue {
    DataValue::Str(serde_json::to_string(v).unwrap_or_default().into())
}

pub fn save(state: &AppState) {
    let db = get_db();
    let rows = vec![
        DataValue::List(vec![str_val("temperaments"),     json_str(&state.temperaments)]),
        DataValue::List(vec![str_val("notation_systems"), json_str(&state.notation_systems)]),
        DataValue::List(vec![str_val("tunings"),          json_str(&state.tunings)]),
        DataValue::List(vec![str_val("scales"),           json_str(&state.scales)]),
        DataValue::List(vec![str_val("chords"),           json_str(&state.chords)]),
        DataValue::List(vec![str_val("instruments"),      json_str(&state.instruments)]),
        DataValue::List(vec![str_val("preferences"),      json_str(&state.preferences)]),
    ];
    let mut params: BTreeMap<String, DataValue> = BTreeMap::new();
    params.insert("data".to_string(), DataValue::List(rows));
    let _ = db.run_script(
        "?[key, value] <- $data :replace xf_data { key: String => value: String }",
        params,
        ScriptMutability::Mutable,
    );
}

pub fn load() -> AppState {
    let db = get_db();
    let Ok(result) = db.run_script(
        "?[key, value] := *xf_data[key, value]",
        Default::default(),
        ScriptMutability::Immutable,
    ) else {
        return AppState::default();
    };

    let mut state = AppState::default();

    for row in result.rows {
        let (key, val) = match (row.get(0), row.get(1)) {
            (Some(DataValue::Str(k)), Some(DataValue::Str(v))) => {
                (k.as_str().to_owned(), v.as_str().to_owned())
            }
            _ => continue,
        };
        match key.as_str() {
            "temperaments" => {
                if let Ok(v) = serde_json::from_str(&val) { state.temperaments = v; }
            }
            "notation_systems" => {
                if let Ok(v) = serde_json::from_str(&val) { state.notation_systems = v; }
            }
            "tunings" => {
                if let Ok(v) = serde_json::from_str(&val) { state.tunings = v; }
            }
            "scales" => {
                if let Ok(v) = serde_json::from_str(&val) { state.scales = v; }
            }
            "chords" => {
                if let Ok(v) = serde_json::from_str(&val) { state.chords = v; }
            }
            "instruments" => {
                if let Ok(v) = serde_json::from_str(&val) { state.instruments = v; }
            }
            "preferences" => {
                if let Ok(v) = serde_json::from_str(&val) { state.preferences = v; }
            }
            _ => {}
        }
    }

    state.restore_selections();
    state
}
