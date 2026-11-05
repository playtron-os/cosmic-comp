#!/usr/bin/env bash
# A window with a modal dialog from another client open, as an app under Explorer's file
# picker, takes no input of its own: a click on it focuses and raises the dialog and does not
# reach the window, and neither does a scroll. Once the dialog is gone the window takes both
# again. A dialog whose parent app exits hears that its parent is gone. Runs CC_BIN in
# tools/headless; touch is not covered, as Xvfb has no touch device.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
hl=$here/../headless/headless.sh
export CC_STATE=${CC_STATE:-$(mktemp -d "${TMPDIR:-/tmp}/modal-check.XXXXXX")}
export CC_SIZE=${CC_SIZE:-1920x1080}
cargo build --quiet --manifest-path "$here/Cargo.toml"
bin=$here/target/debug/modal-check
"$hl" up >/dev/null
trap '"$hl" down' EXIT
width=${CC_SIZE%x*} height=${CC_SIZE#*x}
fail=0
FAIL() { echo "FAIL: $*" >&2; fail=1; }

# saw LOG LINE [N]: the client printed LINE, N times, within ten seconds.
saw() {
	for _ in $(seq 100); do [ "$(count "$1" "$2")" -ge "${3:-1}" ] && return 0; sleep 0.1; done
	return 1
}
count() { grep -cx "$2" "$1" || true; }
# Logs are named by start time.
latest() { ls "$CC_STATE"/client-modal-check-*.log | sort | tail -n1; }
# open ROLE [ARGS]: start a client, wait for its window, print its log and pid.
open() {
	local pid log
	pid=$("$hl" run "$bin" "$@")
	sleep 0.2
	log=$(latest)
	saw "$log" mapped || { echo "modal-check: the $1 window never mapped" >&2; exit 1; }
	echo "$log $pid"
}
# quit PID: end a client `run` started; PID is its sandbox, which does not pass signals on.
quit() { kill $(pgrep -P "$1") "$1" 2>/dev/null || true; }
# dialog_shown: how many of the dialog's pixels are on screen, uncovered.
dialog_shown() {
	"$hl" shot "$CC_STATE/shot.ppm"
	"$bin" count "$CC_STATE/shot.ppm" e09020
}

read -r parent parent_pid < <(open parent)
saw "$parent" "keyboard enter" || { echo "modal-check: the parent never got the keyboard" >&2; exit 1; }
handle=$(sed -n 's/^handle //p' "$parent" | head -n1)
[ -n "$handle" ] || { echo "modal-check: the parent was not exported" >&2; exit 1; }
read -r dialog dialog_pid < <(open dialog "$handle")
saw "$dialog" "keyboard enter" || FAIL "the dialog did not get the keyboard"
# Another window takes the keyboard, so the click below has something to change.
read -r other other_pid < <(open window)
saw "$other" "keyboard enter" || { echo "modal-check: the other window never got the keyboard" >&2; exit 1; }
# Past the open animations.
sleep 1

# A spot on the parent clear of the two small windows in the middle.
x=$((width / 2 - 520)) y=$((height / 2 - 300))
"$hl" move "$x" "$y"
saw "$parent" "pointer enter" || { echo "modal-check: $x,$y is not on the parent" >&2; exit 1; }
parent_keys=$(count "$parent" "keyboard enter") dialog_keys=$(count "$dialog" "keyboard enter")
"$hl" click "$x" "$y"
saw "$dialog" "keyboard enter" $((dialog_keys + 1)) || FAIL "a click on the parent did not focus its dialog"
sleep 0.5
[ "$(count "$parent" "keyboard enter")" = "$parent_keys" ] || FAIL "a click gave the parent the keyboard"
[ "$(count "$parent" button)" = 0 ] || FAIL "a click reached the parent"
# 480x300 less its rounded corners and border.
shown=$(dialog_shown)
[ "$shown" -ge 130000 ] || FAIL "the dialog is not in front of its parent after the click ($shown pixels shown)"
"$hl" click "$x" "$y" 5
"$hl" click "$x" "$y" 4
sleep 0.5
[ "$(count "$parent" axis)" = 0 ] || FAIL "a scroll reached the parent"

# Without the dialog the parent takes clicks and scrolls again.
quit "$dialog_pid"
sleep 0.5
"$hl" click "$x" "$y"
saw "$parent" button || FAIL "a click did not reach the parent once its dialog closed"
"$hl" click "$x" "$y" 5
saw "$parent" axis || FAIL "a scroll did not reach the parent once its dialog closed"

# A dialog hears that its parent app went away.
read -r dialog dialog_pid < <(open dialog "$handle")
quit "$parent_pid"
saw "$dialog" "parent destroyed" || FAIL "the dialog was not told its parent closed"
quit "$other_pid"

[ "$fail" = 0 ] || exit 1
echo "modal dialog focus: ok"
