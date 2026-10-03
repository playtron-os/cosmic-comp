#!/usr/bin/env bash
# Run cosmic-comp nested and headless, drive it, and screenshot it.
set -euo pipefail

NIXPKGS=${CC_NIXPKGS:-github:NixOS/nixpkgs/34ab99075ac4f7e40cf037eef32cb1c360bb85e9}
TOOLS=(Xvfb xdotool grim wlr-randr dbus-daemon)
NIX_PKGS=(xvfb xdotool grim wlr-randr dbus foot)
NIX_LIBS=(libx11 libxcursor libxi libxcb libxkbcommon libglvnd wayland)

STATE=${CC_STATE:-$PWD/.headless}
die() { echo "headless: $*" >&2; exit 1; }

usage() {
	cat <<'EOF'
usage: headless.sh <command> [args]

  up                 start cosmic-comp (winit backend) on a private Xvfb
  down               stop everything `up` started and remove its runtime dir
  env                print the environment a client needs to reach cosmic-comp
  run CMD [ARGS]     start a Wayland client inside cosmic-comp (detached), print its pid
  shot FILE [GRIM]   capture the output to FILE (PNG, or PPM for *.ppm); extra args go to grim
  move X Y           warp the pointer to output pixel X,Y
  press|release [B]  press or release button B (1 left, 2 middle, 3 right; default 1)
  click X Y [B]      move, press and release
  dclick X Y         double click
  drag X1 Y1 X2 Y2 [STEPS] [--hold]
                     press at X1,Y1, move there in STEPS, release unless --hold
  key KEYS...        xdotool key, e.g. `key super+Up`
  type TEXT          type TEXT into the focused client
  selftest           check pointer, typing and a compositor shortcut end to end (needs foot)
  scale S            set the output scale (1, 1.25, 1.5, 2, ...)
  mode dark|light    switch the colour mode
  pids               list the pids `up` and `run` started

Environment for `up`:
  CC_BIN      cosmic-comp binary (default: target/debug/cosmic-comp)
  CC_STATE    state dir: logs, private HOME and config (default: ./.headless)
  CC_SIZE     output size in physical pixels (default: 1920x1080)
  CC_SCALE    initial output scale (default: 1)
  CC_MODE     dark or light (default: dark)
  CC_THEMES   dir holding <theme>/{dark,light}.ron (default: the built-in fallback)
  CC_THEME    theme to activate from CC_THEMES (default: playtron)
  CC_CLEAR    desktop colour, #RRGGBB (default: #2a2a2e)
  CC_DATA_DIRS extra XDG data dirs (desktop entries, icons) for the compositor and clients
  TMPDIR      parent of the short-lived runtime dir (socket paths must stay short)

Environment for `run`:
  CC_CLIENT_ENV  more variables to pass to clients, by name (they start with an empty environment)
  CC_CLIENT_NET  set to give clients network access (default: none)
EOF
}

need_tools() {
	local missing=0 t
	for t in "${TOOLS[@]}"; do command -v "$t" >/dev/null || missing=1; done
	[ "$missing" = 0 ] && return
	[ -n "${CC_NIX_REEXEC:-}" ] && die "missing tools: ${TOOLS[*]}"
	command -v nix >/dev/null || die "missing tools: ${TOOLS[*]} (and no nix to fetch them)"
	local args=() p
	for p in "${NIX_PKGS[@]}"; do args+=("$NIXPKGS#$p"); done
	CC_NIX_REEXEC=1 exec nix shell "${args[@]}" -c "$0" "$@"
}

lib_path() {
	local d
	for d in ${LD_LIBRARY_PATH//:/ }; do
		[ -e "$d/libX11.so.6" ] && { echo "$LD_LIBRARY_PATH"; return; }
	done
	if ldconfig -p 2>/dev/null | grep -q 'libX11.so.6 '; then
		echo "${LD_LIBRARY_PATH:-}"
		return
	fi
	command -v nix >/dev/null || die "libX11 not found; run inside the repo's nix develop shell"
	local args=() p out
	for p in "${NIX_LIBS[@]}"; do args+=("$NIXPKGS#$p.out"); done
	out=$(nix build --no-link --print-out-paths "${args[@]}" 2>/dev/null | sed 's|$|/lib|' | paste -sd:)
	echo "$out${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
}

runtime() { cat "$STATE/runtime" 2>/dev/null || die "not up (no $STATE/runtime)"; }

isolate() {
	unset WAYLAND_DISPLAY DISPLAY DBUS_SESSION_BUS_ADDRESS NOTIFY_SOCKET SWAYSOCK I3SOCK \
		XDG_SESSION_ID XDG_SEAT XDG_VTNR XDG_SESSION_TYPE XDG_CURRENT_DESKTOP XAUTHORITY
	local r; r=$(runtime)
	export HOME=$STATE/home
	export XDG_CONFIG_HOME=$HOME/.config XDG_DATA_HOME=$HOME/.local/share
	export XDG_STATE_HOME=$HOME/.local/state XDG_CACHE_HOME=$HOME/.cache
	export XDG_DATA_DIRS=$STATE/data${CC_DATA_DIRS:+:$CC_DATA_DIRS} XDG_CONFIG_DIRS=$STATE/etc
	export XDG_RUNTIME_DIR=$r
	export DBUS_SESSION_BUS_ADDRESS=unix:path=$r/bus
	# No system bus: cosmic-comp would otherwise take logind inhibitors on the real seat.
	export DBUS_SYSTEM_BUS_ADDRESS=unix:path=$r/no-system-bus
	# Screenshots get published: nothing on screen may name the real user, host or place.
	export USER=kora LOGNAME=kora HOSTNAME=kora TZ=UTC PS1='kora@kora:~$ '
}

xdo() { DISPLAY=$(cat "$STATE/display") xdotool "$@"; }
comp() { local r; r=$(runtime); XDG_RUNTIME_DIR=$r WAYLAND_DISPLAY=$(cat "$STATE/socket") "$@"; }

wait_for() {
	local i
	for i in $(seq 300); do eval "$1" && return 0; sleep 0.05; done
	die "timed out waiting for: $1"
}

set_mode() {
	local dir=$XDG_CONFIG_HOME/cosmic/com.system76.CosmicTheme.Mode/v1
	mkdir -p "$dir"
	case $1 in dark) echo true >"$dir/is_dark" ;; light) echo false >"$dir/is_dark" ;; *) die "mode: dark|light" ;; esac
}

# Keep a random display only if its X lock names our server: parallel harnesses never share one.
start_xvfb() {
	local size=$1 n pid i try
	for try in $(seq 20); do
		n=$((100 + RANDOM % 900))
		[ -e "/tmp/.X11-unix/X$n" ] || [ -e "/tmp/.X$n-lock" ] && continue
		setsid Xvfb ":$n" -screen 0 "${size}x24" -nolisten tcp -nolisten local -noreset \
			>"$STATE/xvfb.log" 2>&1 &
		pid=$!
		for i in $(seq 200); do
			[ -e "/tmp/.X11-unix/X$n" ] && break
			kill -0 "$pid" 2>/dev/null || break
			sleep 0.05
		done
		if kill -0 "$pid" 2>/dev/null && [ "$(tr -d " " <"/tmp/.X$n-lock" 2>/dev/null)" = "$pid" ]; then
			echo "$pid" >"$STATE/xvfb.pid"
			echo ":$n" >"$STATE/display"
			return 0
		fi
		kill "$pid" 2>/dev/null || true
	done
	die "no free X display"
}

# Hide journald and /run/user from cosmic-comp and its clients when bwrap is there.
sandbox() {
	SANDBOX=()
	if ! command -v bwrap >/dev/null; then
		echo "headless: no bwrap, so clients see the real user and host names" >&2
		return 0
	fi
	SANDBOX=(bwrap --dev-bind / / --tmpfs /run/systemd/journal)
	case $(runtime) in /run/user/*) ;; *) SANDBOX+=(--tmpfs /run/user) ;; esac
	SANDBOX+=(--unshare-uts --hostname kora)
	local file
	for file in passwd group hostname; do
		# bwrap binds onto the file itself, not a symlink to it (NixOS links /etc/hostname).
		[ -e "/etc/$file" ] && SANDBOX+=(--ro-bind "$STATE/$file" "$(readlink -f "/etc/$file")")
	done
	[ -d /run/nscd ] && SANDBOX+=(--tmpfs /run/nscd)
	# Clients get no network unless asked: the dock's weather would locate the host.
	[ "${1:-}" != client ] || [ -n "${CC_CLIENT_NET:-}" ] || SANDBOX+=(--unshare-net)
	SANDBOX+=(--)
}

clean_env() {
	CLEAN_ENV=(env -i -C "$HOME")
	local name
	for name in $(compgen -e); do
		case $name in
		PATH | LANG | LANGUAGE | LC_* | LOCALE_ARCHIVE* | LD_LIBRARY_PATH | FONTCONFIG_* | XKB_* | XCURSOR_* | \
			HOME | USER | LOGNAME | HOSTNAME | TZ | PS1 | XDG_* | WAYLAND_DISPLAY | DISPLAY | DBUS_* | \
			RUST_* | ICED_* | WGPU_* | LIBGL_* | MESA_* | GALLIUM_* | VK_* | __GLX_* | __EGL_* | EGL_* | \
			COSMIC_* | ICETRON_* | KORA_* | AGENTOS_*) ;;
		*) [[ " ${CC_CLIENT_ENV:-} " == *" $name "* ]] || continue ;;
		esac
		CLEAN_ENV+=("$name=${!name}")
	done
}

identity() {
	printf 'root:x:0:0:root:/root:/bin/sh\nkora:x:%s:%s:Kora:%s:/bin/sh\nnobody:x:65534:65534::/:/bin/sh\n' \
		"$(id -u)" "$(id -g)" "$STATE/home" >"$STATE/passwd"
	printf 'root:x:0:\nkora:x:%s:\nnogroup:x:65534:\n' "$(id -g)" >"$STATE/group"
	echo kora >"$STATE/hostname"
}

# bwrap forks the command; record the child so `down` stops it too.
sandboxed() {
	local file=$1 parent child i
	[ ${#SANDBOX[@]} -gt 0 ] || return 0
	parent=$(tail -n1 "$file")
	for i in $(seq 100); do
		child=$(pgrep -P "$parent" | head -n1) && [ -n "$child" ] && break
		sleep 0.02
	done
	[ -z "${child:-}" ] || echo "$child" >>"$file"
}

cmd_up() {
	[ -e "$STATE/runtime" ] && die "already up ($STATE); run down first"
	local bin=${CC_BIN:-target/debug/cosmic-comp} size=${CC_SIZE:-1920x1080} libs
	[ -x "$bin" ] || die "no cosmic-comp binary at $bin (set CC_BIN)"
	bin=$(realpath "$bin")
	libs=$(lib_path)
	mkdir -p "$STATE"
	STATE=$(realpath "$STATE")
	# Wayland socket paths must fit in sun_path (108 bytes), so the runtime dir is short.
	local r; r=$(mktemp -d "${TMPDIR:-/tmp}/cchl.XXXXXX")
	echo "$r" >"$STATE/runtime"
	rm -rf "${STATE:?}/home" "${STATE:?}/data" "${STATE:?}/etc" "${STATE:?}"/*.log "${STATE:?}"/*.pid
	identity
	mkdir -p "$STATE/home/.config" "$STATE/home/.local/share" "$STATE/home/.local/state" \
		"$STATE/home/.cache" "$STATE/data" "$STATE/etc"
	isolate
	if [ -n "${CC_THEMES:-}" ]; then
		mkdir -p "$STATE/data/icetron/themes" "$XDG_CONFIG_HOME/icetron"
		cp -r "$CC_THEMES"/. "$STATE/data/icetron/themes/"
		ln -sfn "$STATE/data/icetron/themes/${CC_THEME:-playtron}" "$XDG_CONFIG_HOME/icetron/current-theme"
	fi
	set_mode "${CC_MODE:-dark}"
	local keys
	keys=$(dirname "$(realpath "$0")")/../../data/keybindings.ron
	if [ -f "$keys" ]; then
		mkdir -p "$STATE/data/cosmic/com.system76.CosmicSettings.Shortcuts/v1" "$XDG_CONFIG_HOME/cosmic"
		cp "$keys" "$STATE/data/cosmic/com.system76.CosmicSettings.Shortcuts/v1/defaults"
	fi

	cat >"$STATE/dbus.conf" <<-EOF
		<!DOCTYPE busconfig PUBLIC "-//freedesktop//DTD D-Bus Bus Configuration 1.0//EN"
		 "http://www.freedesktop.org/standards/dbus/1.0/busconfig.dtd">
		<busconfig>
		  <type>session</type>
		  <listen>unix:path=$r/bus</listen>
		  <policy context="default">
		    <allow send_destination="*" eavesdrop="true"/>
		    <allow eavesdrop="true"/>
		    <allow own="*"/>
		  </policy>
		</busconfig>
	EOF
	setsid dbus-daemon --config-file="$STATE/dbus.conf" --nofork --nopidfile >"$STATE/dbus.log" 2>&1 &
	echo $! >"$STATE/dbus.pid"

	start_xvfb "$size"
	sandbox
	local n; n=$(cut -c2- "$STATE/display")

	export DISPLAY=:$n COSMIC_BACKEND=winit LIBGL_ALWAYS_SOFTWARE=1 LD_LIBRARY_PATH=$libs \
		COSMIC_CLEAR_COLOR=${CC_CLEAR:-#2a2a2e} RUST_LOG=${RUST_LOG:-warn}
	clean_env
	setsid "${SANDBOX[@]}" "${CLEAN_ENV[@]}" "$bin" --no-xwayland >"$STATE/comp.log" 2>&1 &
	echo $! >"$STATE/comp.pid"
	sandboxed "$STATE/comp.pid"
	wait_for "ls $r | grep -qx 'wayland-[0-9]*'"
	ls "$r" | grep -x 'wayland-[0-9]*' | head -n1 >"$STATE/socket"
	local win
	wait_for "xdo search --name . >/dev/null 2>&1"
	win=$(xdo search --name . | head -n1)
	xdo windowmove "$win" 0 0 windowsize "$win" "${size%x*}" "${size#*x}"
	# No window manager hands out focus, so give the keyboard to the nested window.
	xdo windowfocus --sync "$win"
	wait_for "comp wlr-randr | grep -q '${size%x*}x${size#*x} px'"
	sleep 0.5
	[ "${CC_SCALE:-1}" = 1 ] || cmd_scale "$CC_SCALE"
	echo "up: display :$n, $(cat "$STATE/socket") in $r (state $STATE)"
}

cmd_down() {
	local p pid
	for p in clients comp xvfb dbus; do
		[ -f "$STATE/$p.pid" ] || continue
		while read -r pid; do kill "$pid" 2>/dev/null || true; done <"$STATE/$p.pid"
		while read -r pid; do
			for _ in $(seq 40); do kill -0 "$pid" 2>/dev/null || break; sleep 0.05; done
		done <"$STATE/$p.pid"
		rm -f "$STATE/$p.pid"
	done
	if [ -f "$STATE/runtime" ]; then
		local r; r=$(cat "$STATE/runtime")
		sleep 0.3
		case $r in */cchl.*) rm -rf "${r:?}" ;; esac
		rm -f "$STATE/runtime" "$STATE/socket" "$STATE/display"
	fi
}

cmd_env() {
	local r; r=$(runtime)
	echo "export XDG_RUNTIME_DIR=$r WAYLAND_DISPLAY=$(cat "$STATE/socket")"
	echo "export DBUS_SESSION_BUS_ADDRESS=unix:path=$r/bus DBUS_SYSTEM_BUS_ADDRESS=unix:path=$r/no-system-bus"
	echo "export HOME=$STATE/home XDG_CONFIG_HOME=$STATE/home/.config XDG_DATA_HOME=$STATE/home/.local/share"
	echo "export XDG_STATE_HOME=$STATE/home/.local/state XDG_CACHE_HOME=$STATE/home/.cache"
	echo "export USER=kora LOGNAME=kora HOSTNAME=kora TZ=UTC"
	echo "unset DISPLAY"
}

cmd_run() {
	[ $# -gt 0 ] || die "run: missing command"
	isolate
	local log; log=$STATE/client-$(basename "$1")-$(date +%s%N).log
	case $1 in */*) set -- "$(realpath "$1")" "${@:2}" ;; esac
	sandbox client
	export WAYLAND_DISPLAY; WAYLAND_DISPLAY=$(cat "$STATE/socket")
	clean_env
	setsid "${SANDBOX[@]}" "${CLEAN_ENV[@]}" "$@" >"$log" 2>&1 &
	echo $! >>"$STATE/clients.pid"
	sandboxed "$STATE/clients.pid"
	echo $!
}

cmd_shot() {
	local out=${1:?shot FILE}
	shift
	case $out in
	*.ppm) comp grim -t ppm "$@" "$out" ;;
	*) comp grim -t png "$@" "$out" ;;
	esac
}

# Each step waits a frame or two so iced sees press and release apart.
cmd_move() { xdo mousemove "${1:?x}" "${2:?y}"; sleep 0.05; }
cmd_press() { xdo mousedown "${1:-1}"; sleep 0.08; }
cmd_release() { xdo mouseup "${1:-1}"; sleep 0.08; }
cmd_click() { cmd_move "$1" "$2"; cmd_press "${3:-1}"; cmd_release "${3:-1}"; }
cmd_dclick() {
	cmd_move "$1" "$2"
	xdo mousedown 1; sleep 0.04; xdo mouseup 1; sleep 0.06
	xdo mousedown 1; sleep 0.04; xdo mouseup 1; sleep 0.1
}

cmd_drag() {
	local x1=$1 y1=$2 x2=$3 y2=$4 steps=${5:-16} hold=${6:-} i
	cmd_move "$x1" "$y1"
	cmd_press
	for i in $(seq 1 "$steps"); do
		xdo mousemove $((x1 + (x2 - x1) * i / steps)) $((y1 + (y2 - y1) * i / steps))
		sleep 0.03
	done
	sleep 0.15
	[ "$hold" = --hold ] || cmd_release
}

cmd_key() { xdo key --delay 60 "$@"; sleep 0.1; }
cmd_type() { xdo type --delay 30 "$*"; sleep 0.1; }

pixel() {
	local ppm=$STATE/selftest.ppm
	rm -f "$ppm"
	cmd_shot "$ppm" || { echo "- - -"; return; }
	local w
	w=$(head -c 64 "$ppm" | tr -s "\n" " " | awk "{print \$2}")
	local header=$(head -c 64 "$ppm" | tr "\n" " " | awk "{printf \"%s %s %s %s \", \$1, \$2, \$3, \$4}" | wc -c)
	od -An -tu1 -j $((header + ($2 * w + $1) * 3)) -N 3 "$ppm" | xargs
}

near() { # "R G B" "R G B" -> success when every channel is within 24
	local a=($1) b=($2) i
	for i in 0 1 2; do [ $(( a[i] > b[i] ? a[i] - b[i] : b[i] - a[i] )) -le 24 ] || return 1; done
}

cmd_selftest() {
	local up=0 ok=1 px
	[ -e "$STATE/runtime" ] || { cmd_up >/dev/null; up=1; }
	local size=${CC_SIZE:-1920x1080} cx cy
	cx=$(( ${size%x*} / 2 )) cy=$(( ${size#*x} / 2 ))
	cmd_run foot --title "kora@kora: ~" -o colors-dark.background=202020 -o colors-light.background=202020 \
		--window-size-pixels=900x600 bash --norc --noprofile >/dev/null
	sleep 2
	cmd_move "$cx" "$cy"
	cmd_type "printf '\033]11;#00c000\a'"
	cmd_key Return
	sleep 0.8
	px=$(pixel $((cx + 60)) $((cy - 40)))
	if near "$px" "0 192 0"; then echo "PASS typing reaches the focused client"; else echo "FAIL typing: centre is $px"; ok=0; fi
	cmd_key super+m
	sleep 1.5
	px=$(pixel 30 "$cy")
	if near "$px" "0 192 0"; then echo "PASS shortcuts reach the compositor"; else echo "FAIL shortcut: left edge is $px"; ok=0; fi
	[ "$up" = 0 ] || cmd_down
	[ "$ok" = 1 ]
}

cmd_scale() {
	local out
	out=$(comp wlr-randr | awk 'NR==1 {print $1}')
	comp wlr-randr --output "$out" --scale "${1:?scale}"
	sleep 0.6
}

cmd_mode() { isolate; set_mode "${1:?mode}"; sleep 0.8; }

cmd_pids() { cat "$STATE"/*.pid 2>/dev/null || true; }

main() {
	local cmd=${1:-}
	[ -n "$cmd" ] || { usage; exit 1; }
	shift
	case $cmd in
	-h | --help | help) usage ;;
	up | down | env | run | shot | move | press | release | click | dclick | drag | key | type | scale | mode | pids | selftest)
		need_tools "$cmd" "$@"
		"cmd_$cmd" "$@"
		;;
	*) usage; exit 1 ;;
	esac
}

main "$@"
