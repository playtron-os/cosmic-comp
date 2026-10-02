#!/usr/bin/env bash
# Record the Halo in every window state with headless.sh, and measure each shot.
#
#   halo-shots.sh OUTDIR [STATE...]
#
# Uses the same CC_BIN / CC_THEMES / CC_DATA_DIRS as headless.sh; HIVE_BIN and
# PANEL_BIN add an overlay-mode first-party app and the dock when set. SCALES and
# MODES pick the matrix (default "1 1.5" and "dark light"). Writes
# OUTDIR/<scale>-<mode>-<state>.png and appends measurements to OUTDIR/measure.jsonl.
# USABLE_TOP is the usable area's top in logical px (18, the panel's top spacer,
# when PANEL_BIN is set; else 0).
set -euo pipefail

here=$(dirname "$(realpath "$0")")
out=${1:?usage: halo-shots.sh OUTDIR [STATE...]}
shift
mkdir -p "$out"
out=$(realpath "$out")
states=("$@")
[ ${#states[@]} -gt 0 ] || states=(normal tier1 tier2 tier3 tier4 overlap overlap-hover fill snap
	fullscreen fullscreen-reveal drag-top overlay)

export CC_STATE=${CC_STATE:-$out/.state}
T=$here/headless.sh
FRONT=1e3a5f
BACK=4a2a5c

python() {
	if command -v python3 >/dev/null; then
		python3 "$@"
	else
		nix shell "${CC_NIXPKGS:-github:NixOS/nixpkgs/34ab99075ac4f7e40cf037eef32cb1c360bb85e9}#python3" -c python3 "$@"
	fi
}

foot_at() { # COLOR WxH
	"$T" run foot -o "colors-dark.background=$1" -o "colors-light.background=$1" \
		--window-size-pixels="$2" >/dev/null
}

measure() { # SHOT STATE [ARGS...]
	local shot=$1 state=$2 usable
	shift 2
	usable=$(python -c "print(round(${USABLE_TOP:-$([ -n "${PANEL_BIN:-}" ] && echo 18 || echo 0)} * $scale))")
	python "$here/halo-measure.py" "$shot" --scale "$scale" --mode "$mode" --clear "$clear" \
		--usable-top "$usable" "$@" |
		sed "s/^{/{\"shot\": \"$(basename "${shot%.ppm}").png\", \"state\": \"$state\", /" >>"$out/measure.jsonl" || true
}

rect() { # SHOT COLOR -> "x y w h" of the window and pill
	python - "$here" "$1" "$2" <<-'EOF'
		import sys
		sys.argv = [sys.argv[0]] + sys.argv[1:]
		exec(open(sys.argv[1] + "/halo-measure.py").read().split("def main")[0])
		img = Image(sys.argv[2])
		win = find_window(img, hex_rgb(sys.argv[3]))
		print(*(win if win else (0, 0, 0, 0)))
	EOF
}

shoot() { # STATE
	local name=$out/$scale-$mode-$1
	"$T" shot "$name.png"
	"$T" shot "$name.ppm"
}

park() { "$T" move 4 4; sleep 0.4; }

run_state() {
	local state=$1
	case $state in
	normal)
		foot_at $FRONT 900x560; sleep 2; park; shoot normal
		measure "$out/$scale-$mode-normal.ppm" normal --window $FRONT ;;
	tier1 | tier2 | tier3 | tier4)
		local w
		case $state in tier1) w=760 ;; tier2) w=560 ;; tier3) w=380 ;; tier4) w=230 ;; esac
		# foot takes --window-size-pixels in logical px under fractional scaling.
		foot_at $FRONT "${w}x300"
		sleep 2; park; shoot "$state"
		measure "$out/$scale-$mode-$state.ppm" "$state" --window $FRONT ;;
	overlap | overlap-hover)
		foot_at $BACK 900x560; sleep 1.5; foot_at $FRONT 700x420; sleep 2; park
		if [ "$state" = overlap-hover ]; then
			read -r l t r b < <(rect "$(shoot_tmp)" $BACK)
			"$T" move $((l + 30)) $((t + 60)); sleep 0.6
		fi
		shoot "$state"
		# Light glass over another window looks like its shadow in pixels: read those by eye.
		[ "$mode" = light ] || measure "$out/$scale-$mode-$state.ppm" "$state" --window $FRONT --window $BACK ;;
	fill)
		foot_at $FRONT 900x560; sleep 2; "$T" key super+m; sleep 1.5; park; shoot fill
		measure "$out/$scale-$mode-fill.ppm" fill --window $FRONT ;;
	snap)
		foot_at $BACK 700x420; sleep 1.5; "$T" key super+shift+Left; sleep 1
		foot_at $FRONT 700x420; sleep 1.5; "$T" key super+shift+Right; sleep 1.5; park; shoot snap
		measure "$out/$scale-$mode-snap.ppm" snap --window $FRONT ;;
	fullscreen | fullscreen-reveal)
		foot_at $FRONT 900x560; sleep 2; "$T" key super+F11; sleep 1.5
		"$T" move 900 600; sleep 0.6
		if [ "$state" = fullscreen-reveal ]; then "$T" move 900 0; sleep 0.8; fi
		shoot "$state"
		measure "$out/$scale-$mode-$state.ppm" "$state" --window $FRONT --fullscreen ;;
	drag-top)
		foot_at $FRONT 900x560; sleep 2
		# Grab the pill by its name: the controls beside it are buttons, not handles.
		read -r x y < <(pill_handle "$(shoot_tmp)" $FRONT)
		"$T" drag $x $y $x 2 16 --hold; sleep 0.4
		shoot drag-top
		measure "$out/$scale-$mode-drag-top.ppm" drag-top --window $FRONT
		"$T" release ;;
	overlay)
		[ -n "${HIVE_BIN:-}" ] || { echo "overlay: HIVE_BIN not set, skipped" >&2; return; }
		"$T" run "$HIVE_BIN" >/dev/null; sleep 6; park; shoot overlay
		measure "$out/$scale-$mode-overlay.ppm" overlay --window 131414 ;;
	*) echo "unknown state $state" >&2; return 1 ;;
	esac
}

pill_handle() { # SHOT COLOR -> a point on the pill's name
	python "$here/halo-measure.py" "$1" --scale "$scale" --mode "$mode" --clear "$clear" --window "$2" |
		python -c "import json, sys; r = json.load(sys.stdin); p = r['pill']; print(round(p[0] + 50 * $scale), round(p[1] + p[3] / 2))"
}

shoot_tmp() {
	"$T" shot "$out/.probe.ppm"
	echo "$out/.probe.ppm"
}

dock() {
	[ -n "${PANEL_BIN:-}" ] || return 0
	"$T" run "$PANEL_BIN" >/dev/null
	sleep 3
}

for scale in ${SCALES:-1 1.5}; do
	for mode in ${MODES:-dark light}; do
		if [ "$mode" = dark ]; then clear=${CLEAR_DARK:-2a2a2e}; else clear=${CLEAR_LIGHT:-d9dce3}; fi
		for state in "${states[@]}"; do
			CC_SCALE=$scale CC_MODE=$mode CC_CLEAR="#$clear" "$T" up >/dev/null
			dock
			run_state "$state" || echo "$state failed at $scale/$mode" >&2
			"$T" down
		done
	done
done
rm -f "$out/.probe.ppm" "$out"/*.ppm
echo "shots in $out, measurements in $out/measure.jsonl"
