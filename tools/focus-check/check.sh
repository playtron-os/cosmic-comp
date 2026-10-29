#!/usr/bin/env bash
# A window on the first of two outputs stays activated, and looks the same, while a clear
# overlay on every output holds the keyboard, as a capture overlay does. The overlay gets
# Esc, and once it is gone the window gets its keys back, though the pointer was left on
# the second output. Runs CC_BIN in tools/headless.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
hl=$here/../headless/headless.sh
export CC_STATE=${CC_STATE:-$(mktemp -d "${TMPDIR:-/tmp}/focus-check.XXXXXX")}
export CC_OUTPUTS=2 CC_SIZE=${CC_SIZE:-2560x800}
cargo build --quiet --manifest-path "$here/Cargo.toml"
bin=$here/target/debug/focus-check
"$hl" up >/dev/null
trap '"$hl" down' EXIT
half=$((${CC_SIZE%x*} / 2)) height=${CC_SIZE#*x}
fail=0

# saw LOG LINE [N]: the client printed LINE, N times, within ten seconds.
saw() {
	for _ in $(seq 100); do [ "$(grep -cx "$2" "$1")" -ge "${3:-1}" ] && return 0; sleep 0.1; done
	return 1
}
# Logs are named by start time; the window keeps writing to its own, so not by mtime.
latest() { ls "$CC_STATE"/client-focus-check-*.log | sort | tail -n1; }

# A window opens on the output under the pointer.
"$hl" move $((half / 4)) $((height - 40))
"$hl" run "$bin" window >/dev/null
window=$(latest)
saw "$window" "activated true" || { echo "focus-check: the window never activated" >&2; exit 1; }
# Past the open animation.
sleep 1
"$hl" shot "$CC_STATE/before.ppm"

"$hl" run "$bin" overlay >/dev/null
sleep 0.2
overlay=$(latest)
saw "$overlay" "keyboard enter" || { echo "focus-check: the overlay never got the keyboard" >&2; exit 1; }
sleep 1
"$hl" shot "$CC_STATE/overlay.ppm"
if sed -n '/^activated true$/,$p' "$window" | grep -qx "activated false"; then
	echo "FAIL: the window was deactivated under the overlay" >&2
	fail=1
fi
changed=$("$bin" diff "$CC_STATE/before.ppm" "$CC_STATE/overlay.ppm" "$half")
if [ "$changed" != 0 ]; then
	echo "FAIL: $changed pixels of the first output changed under the clear overlay" >&2
	fail=1
fi

# As after picking an area on the second output.
"$hl" move $((half + half / 2)) $((height / 2))
"$hl" key Escape
saw "$overlay" "key 1" || { echo "FAIL: the overlay did not get Esc" >&2; fail=1; }
saw "$overlay" "exit" || { echo "focus-check: the overlay did not leave" >&2; exit 1; }
# Focus is restored on the next refresh, which can be up to 150 ms away.
if saw "$window" "keyboard enter" 2; then
	"$hl" key a
	saw "$window" "key 30" || { echo "FAIL: the window did not get its keys" >&2; fail=1; }
else
	echo "FAIL: the window did not get the keyboard back" >&2
	fail=1
fi
[ "$fail" = 0 ] || exit 1
echo "focus under an overlay: ok"
