mod app;
mod render;

use app::AppModel;
use relm4::RelmApp;

fn main() {
    let app = RelmApp::new("org.xenharmonic.XenFret");
    app.run::<AppModel>(());
}
