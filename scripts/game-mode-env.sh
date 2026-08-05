# game-mode-env.sh — SOURCE this (don't execute it) to point your shell at the
# cosmic-comp that owns one.playtron.GameMode, so scripts/game-mode-test.sh talks
# to the right compositor. It locates the owning bus by ASKING each candidate
# session bus who owns the name (robust — no PID guessing), preferring the dev
# build (target/debug/cosmic-comp) over an installed one.
#
#   source scripts/game-mode-env.sh
#   gm status | gm list | gm tag-game <appid> <id> | gm enter <appid>
#
# Re-source after restarting the compositor (its bus + display change each run).

_gm_dir=$(cd "$(dirname "${BASH_SOURCE[0]:-$0}")" 2>/dev/null && pwd)
_gm_iface="one.playtron.GameMode"
_gm_owner=""
_gm_bus=""
_gm_fallback_pid=""
_gm_fallback_bus=""

# Candidate buses: private ones from `make run-debug` (dbus-run-session) first,
# then the shared user bus.
for _b in /tmp/dbus-* "unix:path=/run/user/$(id -u)/bus"; do
  case "$_b" in
    unix:*) _addr="$_b" ;;
    *) [ -S "$_b" ] || continue; _addr="unix:path=$_b" ;;
  esac
  _pid=$(DBUS_SESSION_BUS_ADDRESS="$_addr" busctl --user status "$_gm_iface" 2>/dev/null | sed -n 's/^PID=//p')
  [ -n "$_pid" ] || continue
  if ps -o args= -p "$_pid" 2>/dev/null | grep -q 'target/debug/cosmic-comp'; then
    _gm_owner="$_pid"
    _gm_bus="$_addr" # dev build — prefer it
    break
  fi
  [ -z "$_gm_fallback_pid" ] && { _gm_fallback_pid="$_pid"; _gm_fallback_bus="$_addr"; }
done
# No dev build found — use whatever owns the name (e.g. an installed compositor).
if [ -z "$_gm_owner" ] && [ -n "$_gm_fallback_pid" ]; then
  _gm_owner="$_gm_fallback_pid"
  _gm_bus="$_gm_fallback_bus"
fi

if [ -z "$_gm_owner" ]; then
  echo "game-mode-env: one.playtron.GameMode is not owned on any session bus." >&2
  echo "               Start the dev build with 'make run-debug' first." >&2
else
  export DBUS_SESSION_BUS_ADDRESS="$_gm_bus"
  # DISPLAY = the owner's XWayland child, if it has one up yet.
  _xwl=$(pgrep -P "$_gm_owner" Xwayland 2>/dev/null | head -1)
  _disp=$(ps -o args= "${_xwl:-0}" 2>/dev/null | grep -oP ':\d+' | head -1)
  [ -n "$_disp" ] && export DISPLAY="$_disp"

  eval "gm() { \"$_gm_dir/game-mode-test.sh\" \"\$@\"; }"

  _dev=$(ps -o args= -p "$_gm_owner" 2>/dev/null | grep -q 'target/debug' && echo " (dev build)" || echo "")
  echo "game-mode-env: one.playtron.GameMode owned by pid=$_gm_owner$_dev"
  echo "  DBUS_SESSION_BUS_ADDRESS=$DBUS_SESSION_BUS_ADDRESS"
  if [ -n "$_disp" ]; then
    echo "  DISPLAY=$DISPLAY"
  else
    echo "  DISPLAY=<unset — XWayland not up yet; launch one X client, then re-source>"
  fi
  echo "  shortcut ready:  gm status | gm list | gm tag-game <appid> <id> | gm enter <appid>"
fi

unset _gm_dir _gm_iface _gm_owner _gm_bus _gm_fallback_pid _gm_fallback_bus _b _addr _pid _xwl _disp _dev
