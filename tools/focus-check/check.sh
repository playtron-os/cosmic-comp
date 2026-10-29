#!/usr/bin/env bash
# A window on the first of two outputs stays activated, and looks the same, while a clear
# overlay on every output holds the keyboard, as a capture overlay does. The overlay gets
# Esc, and the keys typed the moment it is gone reach the window, though the pointer was
# left on the second output. The same once an overlay unmaps or hides instead, and once a
# popup of a bar that never takes the keyboard closes, as a panel popover does. Runs CC_BIN
# in tools/headless.
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
xdisplay=$(cat "$CC_STATE/display")
[ -n "$xdisplay" ] && [ "$xdisplay" != "${DISPLAY:-}" ] || { echo "focus-check: no private X display" >&2; exit 1; }
fail=0

# saw LOG LINE [N]: the client printed LINE, N times, within ten seconds.
saw() {
	for _ in $(seq 100); do [ "$(grep -cx "$2" "$1")" -ge "${3:-1}" ] && return 0; sleep 0.1; done
	return 1
}
# gone LOG LINE: as saw, without sleeping, so that keys typed next land within milliseconds,
# well before the compositor's next focus refresh (up to 150 ms away).
gone() {
	local end=$((SECONDS + 10))
	until grep -qx "$2" "$1"; do [ "$SECONDS" -lt "$end" ] || return 1; done
}
# Straight to the nested X server: headless.sh would add its own start-up time.
typed() { DISPLAY=$xdisplay xdotool key "$@"; }
now() { date +%s%3N; }
# Logs are named by start time; the window keeps writing to its own, so not by mtime.
latest() { ls "$CC_STATE"/client-focus-check-*.log | sort | tail -n1; }
# arrived KEY WHAT MS: the window got KEY, typed within MS ms of WHAT.
arrived() {
	saw "$window" "key $1" ||
		{ echo "FAIL: the window lost a key typed within $3 ms of $2" >&2; fail=1; }
}

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

# The overlay over the window takes the keyboard, so it closes first, while the one on the
# second output is still up. Then the pointer rests there, as after picking an area.
"$hl" click $((half / 2)) $((height / 2))
"$hl" move $((half + half / 2)) $((height / 2))
typed Escape
gone "$overlay" exit || { echo "focus-check: the overlay did not leave" >&2; exit 1; }
t0=$(now)
typed a
t1=$(now)
saw "$overlay" "key 1" || { echo "FAIL: the overlay did not get Esc" >&2; fail=1; }
arrived 30 "the overlay left" $((t1 - t0))

# An overlay that unmaps, attaching no buffer, rather than going away.
"$hl" run "$bin" overlay unmap >/dev/null
sleep 0.2
unmapped=$(latest)
saw "$unmapped" "keyboard enter" || { echo "focus-check: the second overlay never got the keyboard" >&2; exit 1; }
typed Escape
gone "$unmapped" unmapped || { echo "focus-check: the second overlay did not unmap" >&2; exit 1; }
t0=$(now)
typed b
t1=$(now)
arrived 48 "the overlay unmapped" $((t1 - t0))

# An overlay that hides and lets go of the keyboard, as the launcher does on Esc; it stays on
# screen for its close animation.
"$hl" run "$bin" overlay hide >/dev/null
sleep 0.2
hidden=$(latest)
saw "$hidden" "keyboard enter" || { echo "focus-check: the third overlay never got the keyboard" >&2; exit 1; }
typed Escape
gone "$hidden" hidden || { echo "focus-check: the third overlay did not hide" >&2; exit 1; }
t0=$(now)
typed d
t1=$(now)
arrived 32 "the overlay hid" $((t1 - t0))

# A bar on the first output; a click on it opens a popup with the keyboard.
"$hl" move $((half / 4)) $((height / 2))
"$hl" run "$bin" popup >/dev/null
sleep 0.2
bar=$(latest)
saw "$bar" "bar up" || { echo "focus-check: the bar never showed" >&2; exit 1; }
sleep 0.5
"$hl" click $((half / 2)) $((height - 20))
saw "$bar" "keyboard enter" || { echo "focus-check: the popup never got the keyboard" >&2; exit 1; }
typed Escape
gone "$bar" closed || { echo "focus-check: the popup did not close" >&2; exit 1; }
t0=$(now)
typed c
t1=$(now)
arrived 46 "the popup closed" $((t1 - t0))
if grep -qx "key 46" "$bar"; then
	echo "FAIL: the bar got a key typed after its popup closed" >&2
	fail=1
fi

[ "$fail" = 0 ] || exit 1
echo "focus under an overlay: ok"
