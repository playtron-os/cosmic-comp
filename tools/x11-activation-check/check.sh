#!/usr/bin/env bash
# An X11 window that asks to be activated as soon as the window manager maps it, before its
# first frame, as some programs do, opens in front with the keyboard, over a window that
# had it. Runs CC_BIN in tools/headless; CC_BUS_SERVICE must start a stand-in workspace
# registry whose active, running workspace is `work`, which gives that workspace its X server.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
hl=$here/../headless/headless.sh
[ -n "${CC_BUS_SERVICE:-}" ] || { echo "x11-activation-check: set CC_BUS_SERVICE" >&2; exit 2; }
export CC_STATE=${CC_STATE:-$(mktemp -d "${TMPDIR:-/tmp}/x11-activation-check.XXXXXX")}
cargo build --quiet --manifest-path "$here/Cargo.toml"
cargo build --quiet --manifest-path "$here/../focus-check/Cargo.toml"
bin=$here/target/debug/x11-activation-check
window=$here/../focus-check/target/debug/focus-check
"$hl" up >/dev/null
trap '"$hl" down' EXIT
socket=$(cat "$CC_STATE/runtime")/kora-workspaces/work/X11-unix/X0
fail=0
FAIL() { echo "FAIL: $*" >&2; fail=1; }

# saw LOG LINE: the client printed LINE within ten seconds.
saw() {
	for _ in $(seq 100); do grep -qx "$2" "$1" && return 0; sleep 0.1; done
	return 1
}
# Logs are named by start time.
latest() { ls "$CC_STATE"/client-"$1"-*.log | sort | tail -n1; }

"$hl" run "$window" window >/dev/null
sleep 0.2
wayland=$(latest focus-check)
saw "$wayland" "activated true" || { echo "x11-activation-check: the first window never activated" >&2; exit 1; }
sleep 1

"$hl" run "$bin" "$socket" ask >/dev/null
sleep 0.2
asking=$(latest x11-activation-check)
saw "$asking" asked || { echo "x11-activation-check: the X11 window was never mapped" >&2; exit 1; }
saw "$asking" "focus in" || FAIL "the X11 window that asked opened without the keyboard"
saw "$asking" "active true" || FAIL "the X11 window that asked is not the active window"
saw "$wayland" "activated false" || FAIL "the window it opened over kept the keyboard"
# Past the open animation.
sleep 1
"$hl" shot "$CC_STATE/asked.png"

[ "$fail" = 0 ] || exit 1
echo "X11 activation before the first frame: ok"
