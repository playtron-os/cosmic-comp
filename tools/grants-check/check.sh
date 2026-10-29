#!/usr/bin/env bash
# What the session, another sandbox (Flatpak's engine) and an app the workspace registry boxed see
# of the guarded globals, and whether their clipboard copies go through: without workspaces, and
# with them as the registry is absent, comes, leaves, returns, predates display grants or adds a
# field to them. Runs CC_BIN in tools/headless; GRANTS_CHECK names a prebuilt grants-check.
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

registry_pid=
start_registry() {
	local pidfile=$CC_STATE/registry.pid
	rm -f "$pidfile"
	# The stand-in records its own pid: the background job is only the subshell around it.
	nested sh -c 'echo $$ >"$0"; exec "$@"' "$pidfile" "$bin" registry "$1" "${ws:-none}" "$app" \
		screen,clipboard >"$CC_STATE/registry.log" 2>&1 &
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
	wait 2>/dev/null || true
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
	expect "$stage: a boxed app" "$boxed" "$copy" one.playtron.Workspaces.App "$app"
	alive
}

echo "workspace: ${ws:-none}"
env -u COSMIC_WORKSPACES "$hl" up >/dev/null
clients "without workspaces" "$held" "$refused"
if grep -i -E 'grant|registry|held request' "$CC_STATE/comp.log"; then
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
echo "grants: ok"
