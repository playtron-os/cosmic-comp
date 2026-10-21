// SPDX-License-Identifier: MIT

//! Against a private `dbus-daemon`, never the session's bus.

use super::*;
use std::io::{BufRead, BufReader};
use std::process::{Child, Command, Stdio};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::time::Duration;

struct PrivateBus {
    daemon: Child,
    address: String,
    dir: std::path::PathBuf,
}

/// Listens in /tmp: a long temp dir would overflow the socket path.
const BUS_CONFIG: &str = r#"<!DOCTYPE busconfig PUBLIC "-//freedesktop//DTD D-Bus Bus Configuration 1.0//EN"
 "http://www.freedesktop.org/standards/dbus/1.0/busconfig.dtd">
<busconfig>
  <type>session</type>
  <listen>unix:tmpdir=/tmp</listen>
  <auth>EXTERNAL</auth>
  <policy context="default">
    <allow send_destination="*" eavesdrop="true"/>
    <allow eavesdrop="true"/>
    <allow own="*"/>
  </policy>
</busconfig>"#;

impl PrivateBus {
    fn start() -> Self {
        static NEXT: AtomicUsize = AtomicUsize::new(0);
        let n = NEXT.fetch_add(1, Ordering::Relaxed);
        let dir = std::env::temp_dir().join(format!("game-mode-client-{}-{n}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let config = dir.join("bus.conf");
        std::fs::write(&config, BUS_CONFIG).unwrap();
        let mut daemon = Command::new("dbus-daemon")
            .arg(format!("--config-file={}", config.display()))
            .args(["--nofork", "--print-address=1"])
            .stdout(Stdio::piped())
            .spawn()
            .expect("dbus-daemon runs the private bus");
        let mut address = String::new();
        BufReader::new(daemon.stdout.take().unwrap())
            .read_line(&mut address)
            .unwrap();
        assert!(!address.trim().is_empty(), "the private bus did not start");
        Self {
            daemon,
            address: address.trim().to_owned(),
            dir,
        }
    }

    fn builder(&self) -> zbus::connection::Builder<'static> {
        zbus::connection::Builder::address(self.address.as_str())
            .unwrap_or_else(|err| panic!("{err}: {:?}", self.address))
    }

    async fn connect(&self) -> zbus::Connection {
        self.builder().build().await.unwrap()
    }
}

impl Drop for PrivateBus {
    fn drop(&mut self) {
        let _ = self.daemon.kill();
        let _ = self.daemon.wait();
        let _ = std::fs::remove_dir_all(&self.dir);
    }
}

/// The next item, or a panic rather than a hung test.
async fn next<S: Stream + Unpin>(stream: &mut S) -> S::Item {
    let timeout = async {
        async_io::Timer::after(Duration::from_secs(5)).await;
        panic!("nothing arrived within 5 s");
    };
    futures_util::pin_mut!(timeout);
    match futures_util::future::select(stream.next(), timeout).await {
        futures_util::future::Either::Left((item, _)) => item.expect("the stream ended"),
        futures_util::future::Either::Right(_) => unreachable!(),
    }
}

/// Game mode as the compositor serves it, cut down to what readers see.
struct Compositor {
    active: bool,
    app_id: u32,
}

#[zbus::interface(name = "one.playtron.GameMode")]
impl Compositor {
    #[zbus(property)]
    fn active(&self) -> bool {
        self.active
    }

    #[zbus(property)]
    fn active_app_id(&self) -> u32 {
        self.app_id
    }

    #[zbus(signal)]
    async fn state_changed(
        emitter: &zbus::object_server::SignalEmitter<'_>,
        active: bool,
        app_id: u32,
    ) -> zbus::Result<()>;
}

#[test]
fn a_game_is_running_only_when_game_mode_holds_one() {
    let state = |active, app_id| State { active, app_id };
    assert!(state(true, 1_245_620).game_running());
    assert!(!state(true, LAUNCHER_APP_ID).game_running());
    assert!(!state(true, 0).game_running());
    assert!(!state(false, 1_245_620).game_running());
}

#[test]
fn states_start_with_now_then_follow_each_change() {
    let bus = PrivateBus::start();
    zbus::block_on(async {
        let compositor = bus
            .builder()
            .serve_at(
                OBJECT_PATH,
                Compositor {
                    active: true,
                    app_id: LAUNCHER_APP_ID,
                },
            )
            .unwrap()
            .name(BUS_NAME)
            .unwrap()
            .build()
            .await
            .unwrap();
        let mut states = Box::pin(states(&bus.connect().await).await.unwrap());
        assert_eq!(
            next(&mut states).await,
            State {
                active: true,
                app_id: LAUNCHER_APP_ID
            }
        );

        let emitter = zbus::object_server::SignalEmitter::new(&compositor, OBJECT_PATH).unwrap();
        Compositor::state_changed(&emitter, true, 1_245_620)
            .await
            .unwrap();
        let game = next(&mut states).await;
        assert!(game.game_running(), "{game:?}");
        Compositor::state_changed(&emitter, false, 0).await.unwrap();
        assert!(!next(&mut states).await.game_running());
    });
}

#[test]
fn without_a_compositor_game_mode_is_off() {
    let bus = PrivateBus::start();
    zbus::block_on(async {
        let mut states = Box::pin(states(&bus.connect().await).await.unwrap());
        assert_eq!(next(&mut states).await, State::default());
    });
}
