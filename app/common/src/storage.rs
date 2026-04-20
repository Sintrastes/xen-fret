use crate::state::AppState;

#[cfg(not(target_arch = "wasm32"))]
mod native;
#[cfg(target_arch = "wasm32")]
mod web;

#[cfg(not(target_arch = "wasm32"))]
pub fn set_data_dir(path: std::path::PathBuf) { native::set_data_dir(path); }

#[cfg(not(target_arch = "wasm32"))]
pub fn load() -> AppState { native::load() }
#[cfg(target_arch = "wasm32")]
pub fn load() -> AppState { web::load() }

#[cfg(not(target_arch = "wasm32"))]
pub fn save(state: &AppState) { native::save(state); }
#[cfg(target_arch = "wasm32")]
pub fn save(state: &AppState) { web::save(state); }

#[cfg(not(target_arch = "wasm32"))]
pub fn load_window_geometry() -> Option<crate::geometry::WindowGeometry> {
    native::load_window_geometry()
}

#[cfg(not(target_arch = "wasm32"))]
pub fn save_window_geometry(geo: &crate::geometry::WindowGeometry) {
    native::save_window_geometry(geo);
}
