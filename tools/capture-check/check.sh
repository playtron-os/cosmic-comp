#!/usr/bin/env bash
# A panel-like layer bar and a window stay in a full-screen capture, and a drawer on the overlay
# layer that marked itself, as GameFrame's quick settings do for their own screenshot, is left
# out of it while it stays on screen. Once unmarked, without the mark, or from a client that
# never binds the global, the drawer is captured. Runs CC_BIN in tools/headless.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
hl=$here/../headless/headless.sh
xwd=${XWD:-$(command -v xwd || true)}
[ -n "$xwd" ] || { echo "check: needs xwd (set XWD) to read the nested screen" >&2; exit 1; }
export CC_STATE=${CC_STATE:-$(mktemp -d "${TMPDIR:-/tmp}/capture-check.XXXXXX")}
cargo build --quiet --manifest-path "$here/Cargo.toml"
bin=$here/target/debug/capture-check
"$hl" up >/dev/null
trap '"$hl" down' EXIT
drawer=$((240 * 64))

# check NAME DRAWER_PIXELS_IN_CAPTURE [show args]
check() {
	local name=$1 want=$2 log shot screen
	shift 2
	"$hl" run "$bin" show "$@" >/dev/null
	log=$(ls -t "$CC_STATE"/client-capture-check-*.log | head -n1)
	for _ in $(seq 100); do grep -q ready "$log" && break; sleep 0.1; done
	grep -q ready "$log" || { echo "$name: the client never drew" >&2; cat "$log" >&2; exit 1; }
	# Past the open animations: a capture taken during one holds blended colours.
	sleep 1
	"$hl" shot "$CC_STATE/$name.ppm"
	DISPLAY=$(cat "$CC_STATE/display") "$xwd" -root -silent >"$CC_STATE/$name.xwd"
	pkill -f "$bin show" || true
	sleep 0.3
	shot=$("$bin" count "$CC_STATE/$name.ppm")
	screen=$("$bin" count "$CC_STATE/$name.xwd")
	echo "$name: capture $shot; screen $screen"
	[[ $shot =~ ^bar=[1-9][0-9]*\ window=[1-9][0-9]*\ drawer=$want$ ]] || { echo "$name: FAILED (capture)" >&2; exit 1; }
	[[ $screen == *" drawer=$drawer" ]] || { echo "$name: FAILED (screen)" >&2; exit 1; }
}

check excluded 0 --exclude
check unmarked "$drawer" --exclude --unmark
check flagless "$drawer"
check no-global "$drawer" --exclude --no-global
echo "capture exclusion: ok"
