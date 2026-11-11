#!/usr/bin/env bash
# What the session, another sandbox (Flatpak's engine) and an app the workspace registry boxed see
# of the guarded globals, and whether their clipboard copies go through: without workspaces, and
# with them as the registry is absent, comes, leaves, returns, predates display grants or adds a
# field to them; then whether a boxed app's paste at "Ask me first" goes through. Runs CC_BIN in
# tools/headless; GRANTS_CHECK names a prebuilt grants-check.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
hl=$here/../headless/headless.sh
bin=${GRANTS_CHECK:-}
if [ -z "$bin" ]; then
	cargo build --quiet --manifest-path "$here/Cargo.toml"
	bin=$here/target/debug/grants-check
fi
export CC_STATE=${CC_STATE:-$(mktemp -d "${TMPDIR:-/tmp}/grants-check.XXXXXX")}
app=org.example.Mail
# Rows are per workspace, and a client's workspace is the deepest workspace slice of its cgroup.
ws=$(grep -o 'workspace-[A-Za-z0-9_-]*\.slice' /proc/self/cgroup | tail -n1 | sed 's/^workspace-//; s/\.slice$//' || true)

list() { printf '%s\n' "$@" | LC_ALL=C sort | paste -sd,; }
askable="zwp_virtual_keyboard_manager_v1 ext_image_copy_capture_manager_v1 ext_output_image_capture_source_manager_v1 ext_data_control_manager_v1 zwlr_data_control_manager_v1"
screen="zcosmic_toplevel_info_v1 ext_foreign_toplevel_list_v1"
shell="cosmic_session_hold_manager_v1 zcosmic_special_action_manager_v1 workspace_transition_manager_v1 kora_workspace_realm_manager_v1"
# shellcheck disable=SC2086
everything=$(list $askable $screen zcosmic_toplevel_manager_v1 $shell)
# shellcheck disable=SC2086
held=$(list $askable)
# shellcheck disable=SC2086
opened=$(list $askable $screen)
# shellcheck disable=SC2086
others=$(list $shell)
# Outside every workspace a boxed app has no rows: everything guarded is off.
[ -n "$ws" ] || { held=; opened=; }
refused=refused kept=kept
[ -n "$ws" ] || { refused=hidden; kept=hidden; }

# Run "$@" with the nested session's environment, and refuse any other.
nested() (
	eval "$("$hl" env)"
	r=$(cat "$CC_STATE/runtime")
	[ "$XDG_RUNTIME_DIR" = "$r" ] && [ -S "$r/$WAYLAND_DISPLAY" ] &&
		[ "$DBUS_SESSION_BUS_ADDRESS" = "unix:path=$r/bus" ] || { echo "not the nested session" >&2; exit 1; }
	exec "$@"
)

registry_pid= registry_job=
# start_registry KIND [CLASSES]: APP_ID's classes open (default screen,clipboard; "" for none).
start_registry() {
	local pidfile=$CC_STATE/registry.pid
	rm -f "$pidfile"
	# The stand-in records its own pid: the background job is only the subshell around it.
	nested sh -c 'echo $$ >"$0"; exec "$@"' "$pidfile" "$bin" registry "$1" "${ws:-none}" "$app" \
		"${2-screen,clipboard}" >"$CC_STATE/registry.log" 2>&1 &
	registry_job=$!
	for _ in $(seq 100); do
		[ -s "$pidfile" ] && grep -q ready "$CC_STATE/registry.log" && { registry_pid=$(cat "$pidfile"); return; }
		sleep 0.05
	done
	echo "the stand-in registry never started" >&2; cat "$CC_STATE/registry.log" >&2; exit 1
}
stop_registry() {
	[ -n "$registry_pid" ] || return 0
	kill "$registry_pid" 2>/dev/null || true
	for _ in $(seq 40); do kill -0 "$registry_pid" 2>/dev/null || break; sleep 0.05; done
	# Only the registry's job: a bare wait would also wait for a client coprocess still running.
	wait "$registry_job" 2>/dev/null || true
	registry_pid=
}
trap 'stop_registry; "$hl" down' EXIT

# expect NAME GLOBALS COPY [ENGINE APP_ID]: retried while the compositor follows the bus.
expect() {
	local name=$1 want="globals=$2 copy=$3" got
	shift 3
	for _ in $(seq 20); do
		got=$(nested "$bin" client "$@" | paste -sd' ')
		[ "$got" = "$want" ] && { echo "ok $name"; return; }
		sleep 0.25
	done
	echo "FAILED $name: got $got, want $want" >&2
	exit 1
}

alive() { kill -0 "$(head -n1 "$CC_STATE/comp.pid")" || { echo "the compositor died" >&2; exit 1; }; }

# The three kinds of client, against what a boxed app should get now.
clients() {
	local stage=$1 boxed=$2 copy=$3
	expect "$stage: the session" "$everything" kept
	expect "$stage: another sandbox" "$others" hidden org.flatpak "$app"
	expect "$stage: a boxed app" "$boxed" "$copy" inc.kora.Workspaces.App "$app"
	alive
}

echo "workspace: ${ws:-none}"
env -u COSMIC_WORKSPACES "$hl" up >/dev/null
clients "without workspaces" "$held" "$refused"
# A disconnect line names the process, which is grants-check itself.
if grep -v '"grants-check"' "$CC_STATE/comp.log" | grep -i -E 'grant|registry|held request'; then
	echo "FAILED: the compositor logged about grants without workspaces" >&2
	exit 1
fi
"$hl" down

COSMIC_WORKSPACES=1 "$hl" up >/dev/null
clients "no registry" "$held" "$refused"
start_registry current
clients "registry up" "$opened" "$kept"
stop_registry
clients "registry gone" "$held" "$refused"
start_registry current
clients "registry back" "$opened" "$kept"
stop_registry
start_registry older
clients "older registry" "$held" "$refused"
stop_registry
start_registry newer
clients "newer registry" "$opened" "$kept"
stop_registry

# expect_paste NAME RESULT [window]: a boxed app's paste on a connection that hangs up at once.
expect_paste() {
	local got
	got=$(nested "$bin" paste inc.kora.Workspaces.App "$app" ${3:+"$3"})
	[ "$got" = "paste=$2" ] && { echo "ok $1"; return; }
	echo "FAILED $1: got $got, want paste=$2" >&2
	exit 1
}

# expect_type NAME RESULT: keys a boxed app sends on a connection that hangs up at once, as wtype.
expect_type() {
	local got
	got=$(nested "$bin" type inc.kora.Workspaces.App "$app")
	[ "$got" = "type=$2" ] && { echo "ok $1"; return; }
	echo "FAILED $1: got $got, want type=$2" >&2
	exit 1
}

# Only a workspace's boxed app is asked: elsewhere it has no rows and data control is hidden.
if [ -n "$ws" ]; then
	start_registry allow ""
	expect_paste "a paste the person allows after its client hung up" pasted
	expect_type "keys the person allows after their client hung up" typed
	stop_registry
	start_registry deny ""
	expect_paste "a paste the person denies" empty
	expect_paste "a paste into the app's own window, unasked" pasted window
	expect_type "keys the person denies" nothing
	stop_registry
	# Off hides the virtual keyboard, and the compositor logs that refusal: nothing else would.
	start_registry off input
	# shellcheck disable=SC2086
	expect "input off: a boxed app" "$(list ${askable#zwp_virtual_keyboard_manager_v1 })" "$refused" \
		inc.kora.Workspaces.App "$app"
	grep -q 'row that is off.*input' "$CC_STATE/comp.log" || {
		echo "FAILED input off: the compositor did not log the refusal" >&2
		exit 1
	}
	echo "ok input off: logged"
	stop_registry
	# The registry keeps an app's Deny as its row at Off: the app's data control, taken before,
	# stays connected, and its later copy is refused unasked with a line in the log.
	start_registry current clipboard
	coproc HOLD { nested "$bin" hold inc.kora.Workspaces.App "$app"; }
	read -r said <&"${HOLD[0]}"
	[ "$said" = held ] || { echo "FAILED the holding client: $said" >&2; exit 1; }
	stop_registry
	start_registry off clipboard
	asked=$(grep -c 'holding a request' "$CC_STATE/comp.log" || true)
	sleep 1
	echo go >&"${HOLD[1]}"
	read -r got <&"${HOLD[0]}"
	wait "$HOLD_PID" 2>/dev/null || true
	[ "$got" = copy=refused ] || { echo "FAILED a copy after the row turned off: got $got" >&2; exit 1; }
	grep -q 'refused held requests.*its row is off' "$CC_STATE/comp.log" || {
		echo "FAILED a copy after the row turned off: no refusal in the log" >&2
		exit 1
	}
	[ "$(grep -c 'holding a request' "$CC_STATE/comp.log" || true)" = "$asked" ] || {
		echo "FAILED a copy after the row turned off was asked about" >&2
		exit 1
	}
	echo "ok a row turned off refuses a connected client's copy, logged and unasked"
	stop_registry
	alive
fi
echo "grants: ok"
