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
        let listener = bus.connect().await;
        let mut heard = Box::pin(holds(&listener).await.unwrap());

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
        let compositor = bus.connect().await;
        let mut present = Box::pin(host_present(&compositor).await.unwrap());
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
        let compositor = bus.connect().await;
        let mut present = Box::pin(host_present(&compositor).await.unwrap());
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
