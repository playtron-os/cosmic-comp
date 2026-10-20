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
        let dir =
            std::env::temp_dir().join(format!("speech-to-text-client-{}-{n}", std::process::id()));
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

#[test]
fn holds_arrive_in_order() {
    let bus = PrivateBus::start();
    zbus::block_on(async {
        let compositor = bus
            .builder()
            .serve_at(OBJECT_PATH, Announcer)
            .unwrap()
            .name(BUS_NAME)
            .unwrap()
            .build()
            .await
            .unwrap();
        // The streams keep their connection: the caller need not.
        let mut heard = Box::pin(holds(&bus.connect().await).await.unwrap());

        let sent = [
            Hold::Started {
                session: 1,
                app_id: "one.playtron.Mail".into(),
            },
            Hold::Finished(1),
            Hold::Started {
                session: 2,
                app_id: String::new(),
            },
            Hold::Cancelled(2),
        ];
        let emitter = SignalEmitter::new(&compositor, OBJECT_PATH).unwrap();
        for hold in &sent {
            hold.emit(&emitter).await.unwrap();
        }
        for hold in &sent {
            assert_eq!(&next(&mut heard).await, hold);
        }
    });
}

#[test]
fn a_hold_from_anyone_else_is_ignored() {
    let bus = PrivateBus::start();
    zbus::block_on(async {
        let impostor = bus
            .builder()
            .serve_at(OBJECT_PATH, Announcer)
            .unwrap()
            .build()
            .await
            .unwrap();
        let compositor = bus
            .builder()
            .serve_at(OBJECT_PATH, Announcer)
            .unwrap()
            .name(BUS_NAME)
            .unwrap()
            .build()
            .await
            .unwrap();
        let listener = bus.connect().await;
        let mut heard = Box::pin(holds(&listener).await.unwrap());

        let from = |conn| SignalEmitter::new(conn, OBJECT_PATH).unwrap();
        Hold::Cancelled(7).emit(&from(&impostor)).await.unwrap();
        Hold::Finished(8).emit(&from(&compositor)).await.unwrap();
        assert_eq!(next(&mut heard).await, Hold::Finished(8));
    });
}

#[test]
fn the_host_is_followed_as_it_comes_and_goes() {
    let bus = PrivateBus::start();
    zbus::block_on(async {
        let mut present = Box::pin(host_present(&bus.connect().await).await.unwrap());
        assert!(!next(&mut present).await, "no host yet");

        let host = Host::claim(bus.builder()).await.unwrap();
        assert!(next(&mut present).await, "the host arrived");

        host.release().await.unwrap();
        assert!(!next(&mut present).await, "the host let go");

        let host = Host::claim(bus.builder()).await.unwrap();
        assert!(next(&mut present).await, "a host came back");

        drop(host);
        assert!(!next(&mut present).await, "dropping the host lets go too");
    });
}

#[test]
fn a_host_already_there_is_seen_at_once() {
    let bus = PrivateBus::start();
    zbus::block_on(async {
        let _host = Host::claim(bus.builder()).await.unwrap();
        let mut present = Box::pin(host_present(&bus.connect().await).await.unwrap());
        assert!(next(&mut present).await);
    });
}

#[test]
fn a_second_host_is_refused() {
    let bus = PrivateBus::start();
    zbus::block_on(async {
        let _first = Host::claim(bus.builder()).await.unwrap();
        assert!(Host::claim(bus.builder()).await.is_err());
    });
}

#[test]
fn other_messages_are_not_holds() {
    let message = zbus::Message::signal(OBJECT_PATH, "org.freedesktop.DBus.Properties", "Started")
        .unwrap()
        .build(&(1u32, ""))
        .unwrap();
    assert_eq!(Hold::from_message(&message), None);
    let message = zbus::Message::signal(OBJECT_PATH, INTERFACE, "Started")
        .unwrap()
        .build(&(3u32, "one.playtron.Chord"))
        .unwrap();
    assert_eq!(
        Hold::from_message(&message),
        Some(Hold::Started {
            session: 3,
            app_id: "one.playtron.Chord".into()
        })
    );
    assert_eq!(Hold::Cancelled(4).session(), 4);
}

/// A compositor serving both interfaces, as the real one does.
async fn compositor(bus: &PrivateBus, claims: &InlineClaims) -> zbus::Connection {
    bus.builder()
        .serve_at(OBJECT_PATH, Announcer)
        .unwrap()
        .serve_at(OBJECT_PATH, Inline::new(claims.clone()))
        .unwrap()
        .name(BUS_NAME)
        .unwrap()
        .build()
        .await
        .unwrap()
}

/// Waits for state another task changes, or panics rather than hang.
async fn eventually(what: &str, done: impl Fn() -> bool) {
    for _ in 0..500 {
        if done() {
            return;
        }
        async_io::Timer::after(Duration::from_millis(10)).await;
    }
    panic!("{what} did not happen within 5 s");
}

#[test]
fn a_claimed_apps_holds_reach_it_and_never_the_host() {
    let bus = PrivateBus::start();
    zbus::block_on(async {
        let claims = InlineClaims::default();
        let compositor = compositor(&bus, &claims).await;
        let mut host = Box::pin(holds(&bus.connect().await).await.unwrap());
        let mut app = Box::pin(
            claim_inline(bus.builder(), "one.playtron.Game")
                .await
                .unwrap(),
        );
        assert!(claims.claimed("one.playtron.Game"));
        assert!(!claims.claimed("one.playtron.Mail"));

        let emitter = SignalEmitter::new(&compositor, OBJECT_PATH).unwrap();
        let started = Hold::Started {
            session: 1,
            app_id: "one.playtron.Game".into(),
        };
        started.emit_inline(&emitter).await.unwrap();
        Hold::Finished(1).emit_inline(&emitter).await.unwrap();
        let elsewhere = Hold::Started {
            session: 2,
            app_id: "one.playtron.Mail".into(),
        };
        elsewhere.emit(&emitter).await.unwrap();

        assert_eq!(next(&mut app).await, started);
        assert_eq!(next(&mut app).await, Hold::Finished(1));
        // The host's first hold is the unclaimed one: the claimed hold passed it by.
        assert_eq!(next(&mut host).await, elsewhere);
    });
}

#[test]
fn another_apps_claimed_holds_pass_by() {
    let bus = PrivateBus::start();
    zbus::block_on(async {
        let claims = InlineClaims::default();
        let compositor = compositor(&bus, &claims).await;
        let mut game = Box::pin(
            claim_inline(bus.builder(), "one.playtron.Game")
                .await
                .unwrap(),
        );
        let _notes = claim_inline(bus.builder(), "one.playtron.Notes")
            .await
            .unwrap();

        let emitter = SignalEmitter::new(&compositor, OBJECT_PATH).unwrap();
        let theirs = Hold::Started {
            session: 1,
            app_id: "one.playtron.Notes".into(),
        };
        let ours = Hold::Started {
            session: 2,
            app_id: "one.playtron.Game".into(),
        };
        for hold in [theirs, Hold::Cancelled(1), ours.clone(), Hold::Cancelled(2)] {
            hold.emit_inline(&emitter).await.unwrap();
        }
        assert_eq!(next(&mut game).await, ours);
        assert_eq!(next(&mut game).await, Hold::Cancelled(2));
    });
}

#[test]
fn a_claim_ends_with_its_stream() {
    let bus = PrivateBus::start();
    zbus::block_on(async {
        let claims = InlineClaims::default();
        let _compositor = compositor(&bus, &claims).await;
        let app = claim_inline(bus.builder(), "one.playtron.Game")
            .await
            .unwrap();
        let other = claim_inline(bus.builder(), "one.playtron.Game")
            .await
            .unwrap();
        drop(app);
        // The other instance still claims the app id.
        async_io::Timer::after(Duration::from_millis(200)).await;
        assert!(claims.claimed("one.playtron.Game"));
        drop(other);
        eventually("forgetting the last claim", || {
            !claims.claimed("one.playtron.Game")
        })
        .await;
    });
}

#[test]
fn a_claim_can_be_released_and_needs_an_app_id() {
    let bus = PrivateBus::start();
    zbus::block_on(async {
        let claims = InlineClaims::default();
        let _compositor = compositor(&bus, &claims).await;
        let app = bus.connect().await;
        let call = |method: &'static str, app_id: &'static str| {
            let app = app.clone();
            async move {
                app.call_method(
                    Some(BUS_NAME),
                    OBJECT_PATH,
                    Some(INLINE_INTERFACE),
                    method,
                    &(app_id,),
                )
                .await
            }
        };
        call("Claim", "one.playtron.Game").await.unwrap();
        assert!(claims.claimed("one.playtron.Game"));
        call("Release", "one.playtron.Game").await.unwrap();
        assert!(!claims.claimed("one.playtron.Game"));
        assert!(call("Claim", "").await.is_err());
        assert!(!claims.claimed(""));
    });
}

#[test]
fn inline_steps_are_read_only_from_their_interface() {
    let message = zbus::Message::signal(OBJECT_PATH, INLINE_INTERFACE, "Started")
        .unwrap()
        .build(&(5u32, "one.playtron.Game"))
        .unwrap();
    let started = Hold::Started {
        session: 5,
        app_id: "one.playtron.Game".into(),
    };
    assert_eq!(Hold::from_inline_message(&message), Some(started));
    assert_eq!(Hold::from_message(&message), None);
    let message = zbus::Message::signal(OBJECT_PATH, INTERFACE, "Finished")
        .unwrap()
        .build(&(5u32,))
        .unwrap();
    assert_eq!(Hold::from_inline_message(&message), None);
}
