use crate::state::APP_STATE;
use dioxus::prelude::*;

/// Call once inside a component to start tracking the system dark/light preference.
///
/// Each platform uses the most efficient available mechanism:
/// - **Web / WebView (Android, iOS)**: `window.matchMedia` change event — truly event-driven.
/// - **Linux**: XDG Desktop Portal D-Bus `SettingChanged` signal — event-driven, zero polling.
/// - **macOS**: `CFNotificationCenter` `AppleInterfaceThemeChangedNotification` — event-driven.
/// - **Windows**: 2 s poll via OS thread.
/// - **Other native**: one-shot detection at startup.
pub fn use_system_dark_watcher() {
    // ── Web / WebView (Android, iOS) ──────────────────────────────────────────
    #[cfg(target_arch = "wasm32")]
    use_effect(move || {
        spawn(async move {
            let mut e = dioxus::document::eval(
                r#"
                var mq = window.matchMedia('(prefers-color-scheme: dark)');
                dioxus.send(mq.matches);
                mq.addEventListener('change', function(e) { dioxus.send(e.matches); });
                "#,
            );
            loop {
                match e.recv().await {
                    Ok(serde_json::Value::Bool(dark)) => {
                        APP_STATE.write().system_dark = dark;
                    }
                    _ => break,
                }
            }
        });
    });

    // ── Linux: XDG Desktop Portal D-Bus SettingChanged signal ────────────────
    #[cfg(target_os = "linux")]
    use_effect(move || {
        let is_dark = dark_light::detect() == dark_light::Mode::Dark;
        APP_STATE.write().system_dark = is_dark;
        spawn(async move {
            watch_linux().await;
        });
    });

    // ── macOS: CFNotificationCenter (event-driven) ────────────────────────────
    #[cfg(target_os = "macos")]
    use_effect(move || {
        let is_dark = dark_light::detect() == dark_light::Mode::Dark;
        APP_STATE.write().system_dark = is_dark;

        let (tx, mut rx) = tokio::sync::mpsc::channel::<bool>(4);
        if macos::TX.set(tx).is_ok() {
            macos::register_observer();
        }

        spawn(async move {
            while let Some(is_dark) = rx.recv().await {
                if APP_STATE.read().system_dark != is_dark {
                    APP_STATE.write().system_dark = is_dark;
                }
            }
        });
    });

    // ── Windows: poll every 2 s via OS thread ───────────────────────────────
    #[cfg(target_os = "windows")]
    use_effect(move || {
        let is_dark = dark_light::detect() == dark_light::Mode::Dark;
        APP_STATE.write().system_dark = is_dark;

        let (tx, mut rx) = tokio::sync::mpsc::channel::<bool>(4);
        std::thread::Builder::new()
            .name("xen-fret-theme-poll".into())
            .spawn(move || loop {
                std::thread::sleep(std::time::Duration::from_secs(2));
                let is_dark = dark_light::detect() == dark_light::Mode::Dark;
                if tx.blocking_send(is_dark).is_err() {
                    break;
                }
            })
            .ok();

        spawn(async move {
            while let Some(is_dark) = rx.recv().await {
                if APP_STATE.read().system_dark != is_dark {
                    APP_STATE.write().system_dark = is_dark;
                }
            }
        });
    });

    // ── Other native platforms: one-shot at startup ───────────────────────────
    #[cfg(not(any(
        target_arch = "wasm32",
        target_os = "linux",
        target_os = "macos",
        target_os = "windows",
    )))]
    use_effect(move || {
        let is_dark = dark_light::detect() == dark_light::Mode::Dark;
        APP_STATE.write().system_dark = is_dark;
    });
}

// ── Linux ─────────────────────────────────────────────────────────────────────

#[cfg(target_os = "linux")]
#[zbus::proxy(
    interface = "org.freedesktop.portal.Settings",
    default_service = "org.freedesktop.portal.Desktop",
    default_path = "/org/freedesktop/portal/desktop"
)]
trait PortalSettings {
    #[zbus(signal)]
    fn setting_changed(
        &self,
        namespace: String,
        key: String,
        value: zbus::zvariant::OwnedValue,
    ) -> zbus::Result<()>;
}

#[cfg(target_os = "linux")]
async fn watch_linux() {
    use futures_util::StreamExt;

    let conn = match zbus::Connection::session().await {
        Ok(c) => c,
        Err(_) => return,
    };
    let proxy = match PortalSettingsProxy::new(&conn).await {
        Ok(p) => p,
        Err(_) => return,
    };
    let mut stream = match proxy.receive_setting_changed().await {
        Ok(s) => s,
        Err(_) => return,
    };

    while let Some(signal) = stream.next().await {
        if let Ok(args) = signal.args() {
            if args.namespace == "org.freedesktop.appearance" && args.key == "color-scheme" {
                let is_dark = dark_light::detect() == dark_light::Mode::Dark;
                if APP_STATE.read().system_dark != is_dark {
                    APP_STATE.write().system_dark = is_dark;
                }
            }
        }
    }
}

// ── macOS ─────────────────────────────────────────────────────────────────────
//
// Registers a CFNotificationCenter observer for AppleInterfaceThemeChangedNotification.
// The callback reads the current appearance via `defaults read` (bypassing
// NSUserDefaults' thread-local cache) and sends the result through a channel.

#[cfg(target_os = "macos")]
mod macos {
    use std::ffi::{c_void, CString};
    use std::sync::OnceLock;

    use core_foundation_sys::base::CFRelease;
    use core_foundation_sys::dictionary::CFDictionaryRef;
    use core_foundation_sys::notification_center::{
        CFNotificationCenterAddObserver, CFNotificationCenterGetDistributedCenter,
        CFNotificationCenterRef, CFNotificationSuspensionBehaviorDeliverImmediately,
    };
    use core_foundation_sys::string::{
        kCFStringEncodingUTF8, CFStringCreateWithCString, CFStringRef,
    };

    pub static TX: OnceLock<tokio::sync::mpsc::Sender<bool>> = OnceLock::new();

    fn read_is_dark() -> bool {
        std::process::Command::new("defaults")
            .args(["read", "-g", "AppleInterfaceStyle"])
            .output()
            .map(|o| String::from_utf8_lossy(&o.stdout).trim() == "Dark")
            .unwrap_or(false)
    }

    extern "C" fn on_theme_changed(
        _center: CFNotificationCenterRef,
        _observer: *mut c_void,
        _name: CFStringRef,
        _object: *const c_void,
        _user_info: CFDictionaryRef,
    ) {
        if let Some(tx) = TX.get() {
            let _ = tx.try_send(read_is_dark());
        }
    }

    pub fn register_observer() {
        unsafe {
            let center = CFNotificationCenterGetDistributedCenter();
            let name_cstr =
                CString::new("AppleInterfaceThemeChangedNotification").unwrap();
            let name = CFStringCreateWithCString(
                std::ptr::null(),
                name_cstr.as_ptr(),
                kCFStringEncodingUTF8,
            );
            CFNotificationCenterAddObserver(
                center,
                std::ptr::null(),
                on_theme_changed,
                name,
                std::ptr::null(),
                CFNotificationSuspensionBehaviorDeliverImmediately,
            );
            CFRelease(name as *const _);
        }
    }
}
