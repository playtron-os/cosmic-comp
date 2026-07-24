#!/usr/bin/env bash
#
# game-mode-test.sh — drive cosmic-comp's `one.playtron.GameMode` D-Bus
# interface for manual testing of native game mode, plus `xprop` helpers to tag
# XWayland windows with the Steam atoms cosmic-comp reads.
#
# Run this INSIDE the running cosmic-comp session — the interface is owned on
# the user's *session* bus (`busctl --user`).
#
# Requires: busctl. Window tagging also needs `xprop`, and `xdotool` (or
# `xwininfo`) to click-pick a window.
#
# NOTE: `logs` only shows output in a DEBUG build of cosmic-comp — the `gaming`
# tracing target is compiled in only under debug_assertions.
#
# Quick tour:
#   ./game-mode-test.sh status                 # dump all interface state
#   ./game-mode-test.sh tag-game 12345         # click a window -> STEAM_GAME=12345
#   ./game-mode-test.sh enter 12345            # that window goes exclusive-fullscreen
#   ./game-mode-test.sh vrr on                 # toggle tunables while in game mode
#   ./game-mode-test.sh tag-overlay            # click a 2nd window -> mark it an overlay
#   ./game-mode-test.sh exit                   # back to the desktop

set -uo pipefail

SVC="one.playtron.GameMode"
OBJ="/one/playtron/GameMode"
IFACE="one.playtron.GameMode"
BUSCTL=(busctl --user)

_call() { "${BUSCTL[@]}" call "$SVC" "$OBJ" "$IFACE" "$@"; }
_get()  { "${BUSCTL[@]}" get-property "$SVC" "$OBJ" "$IFACE" "$1"; }

# on/true/1/yes -> true, anything else -> false
_bool() { case "${1,,}" in on | true | 1 | yes) echo true ;; *) echo false ;; esac; }

_require_up() {
  if ! "${BUSCTL[@]}" status "$SVC" >/dev/null 2>&1; then
    echo "error: $SVC is not on the session bus." >&2
    echo "       Is cosmic-comp running, and are you in its session" >&2
    echo "       (with DBUS_SESSION_BUS_ADDRESS set)?" >&2
    exit 1
  fi
}

# Echo an X11 window id. Uses $1 if given, else click-to-select.
# NOTE: only XWayland (X11) windows can be selected/tagged — native-Wayland
# apps aren't visible to X tools and can't carry the atoms. Use `list` to see
# taggable windows and pass an id, which is more reliable than clicking.
_pick() {
  if [ -n "${1:-}" ]; then echo "$1"; return; fi
  if command -v xdotool >/dev/null; then
    echo "Click the target XWayland window..." >&2
    xdotool selectwindow
  elif command -v xwininfo >/dev/null; then
    echo "Click the target XWayland window (X11 only — a native-Wayland window" >&2
    echo "won't register; run '$0 list' and pass an id instead)..." >&2
    xwininfo | awk '/Window id:/ {print $4}'
  else
    echo "error: no xdotool/xwininfo to pick a window — run '$0 list' and pass an id." >&2
    exit 1
  fi
}

# Read back an atom so tagging gives feedback (and proves the window is X11).
_verify_atom() { xprop -id "$1" "$2" 2>/dev/null || echo "  (could not read $2 on $1 — is it an XWayland window?)"; }

usage() {
  cat <<'EOF'
game-mode-test.sh — test cosmic-comp game mode (one.playtron.GameMode)

STATE
  status                  Dump every property (Active, FocusedAppId, caps, ...)
  watch                   Live-monitor StateChanged/FocusChanged/CapabilitiesChanged
  logs                    Tail the 'gaming' tracing target (debug builds only)

CONTROL
  enter <appid>           EnterGameMode — fullscreen the window tagged STEAM_GAME=<appid>
  exit                    ExitGameMode
  fps <n>                 SetFpsLimit (0 = uncapped)
  vrr <auto|on|off>       SetVrr
  tearing <on|off>        SetTearing
  hdr <on|off>            SetHdr (stub — no HDR pipeline yet)
  scaling <w> <h> <mode>  SetScaling  (mode: native|integer|fsr|fit|fill|stretch)
  overlay <on|off> [block] SetOverlay (block = route input to the overlay)
  frametime <appid>       AppFrametimeNs — recent frame time (ns) for the app

WINDOW TAGGING — XWayland/X11 windows ONLY. Native-Wayland apps can't carry
these atoms (and X tools can't even see them). Force an app onto XWayland:
    vkcube --wsi xcb          # vulkan-tools cube on X11
    WAYLAND_DISPLAY= <app>    # generic: hide Wayland so the app falls back to X
  list                    List taggable XWayland windows (id + WM_CLASS + title)
  tag-game <appid> [win]  Set STEAM_GAME=<appid>       (make a window a "game")
  tag-overlay [win]       Set STEAM_OVERLAY=1          (Steam-overlay-like window)
  untag-overlay [win]     Remove STEAM_OVERLAY
  tag-input <0|1> [win]   Set STEAM_INPUT_FOCUS        (1 = grab input over the game)
  pick [win]              Print a window id (click-to-select; XWayland only)

TYPICAL FLOW
  vkcube --wsi xcb &                          # a real XWayland test window
  ./game-mode-test.sh list                    # find its id (0x...)  [col 1]
  ./game-mode-test.sh tag-game 12345 0x…       # STEAM_GAME=12345 (read-back confirms)
  ./game-mode-test.sh enter 12345             # -> exclusive fullscreen (direct scanout)
  ./game-mode-test.sh status
  ./game-mode-test.sh exit

  # deferred enter (window not tagged yet):
  ./game-mode-test.sh enter 777               # defers (nothing happens)
  ./game-mode-test.sh tag-game 777 0x…         # tag an existing window -> it enters live

  # overlay over a running game:
  ./game-mode-test.sh tag-overlay 0x…          # marks a 2nd window -> stays above, tearing drops
EOF
}

cmd="${1:-help}"
shift || true

case "$cmd" in
status)
  _require_up
  echo "== $IFACE =="
  for p in Active ActiveAppId FocusedAppId OverlayVisible \
    FpsLimit Tearing Vrr HdrEnabled Scaling \
    VrrSupported HdrSupported TearingSupported \
    RefreshRates DisplayRefreshRate DisplayExternal; do
    printf '  %-18s %s\n' "$p" "$(_get "$p" 2>/dev/null || echo '?')"
  done
  ;;
enter) _require_up; _call EnterGameMode u "${1:?usage: enter <appid>}" ;;
exit) _require_up; _call ExitGameMode ;;
fps) _require_up; _call SetFpsLimit u "${1:?usage: fps <n>  (0 = uncapped)}" ;;
vrr) _require_up; _call SetVrr s "${1:?usage: vrr <auto|on|off>}" ;;
tearing) _require_up; _call SetTearing b "$(_bool "${1:?usage: tearing <on|off>}")" ;;
hdr) _require_up; _call SetHdr b "$(_bool "${1:?usage: hdr <on|off>}")" ;;
scaling)
  _require_up
  _call SetScaling uus "${1:?w}" "${2:?h}" "${3:?mode: native|integer|fsr|fit|fill|stretch}"
  ;;
overlay)
  _require_up
  _call SetOverlay bb "$(_bool "${1:?usage: overlay <on|off> [block]}")" "$(_bool "${2:-off}")"
  ;;
frametime) _require_up; _call AppFrametimeNs u "${1:?usage: frametime <appid>}" ;;
watch)
  _require_up
  echo "Watching $SVC signals (Ctrl-C to stop)..."
  "${BUSCTL[@]}" monitor "$SVC"
  ;;
logs) tail -f "${XDG_RUNTIME_DIR:?XDG_RUNTIME_DIR unset}/cosmic-comp.log" | grep --line-buffered -i gaming ;;
list)
  command -v wmctrl >/dev/null || {
    echo "error: wmctrl not installed (use click-to-pick, or install wmctrl)." >&2
    exit 1
  }
  echo "XWayland windows — column 1 is the id to pass to tag-* (col 3 = WM_CLASS):"
  wmctrl -lx || echo "  (no XWayland windows managed)"
  ;;
tag-game)
  appid="${1:?usage: tag-game <appid> [win]}"
  w=$(_pick "${2:-}")
  xprop -id "$w" -f STEAM_GAME 32c -set STEAM_GAME "$appid"
  echo "tagged window $w  STEAM_GAME=$appid"
  _verify_atom "$w" STEAM_GAME
  ;;
tag-overlay)
  w=$(_pick "${1:-}")
  xprop -id "$w" -f STEAM_OVERLAY 32c -set STEAM_OVERLAY 1
  echo "tagged window $w  STEAM_OVERLAY=1"
  _verify_atom "$w" STEAM_OVERLAY
  ;;
untag-overlay)
  w=$(_pick "${1:-}")
  xprop -id "$w" -remove STEAM_OVERLAY
  echo "removed STEAM_OVERLAY from window $w"
  ;;
tag-input)
  mode="${1:?usage: tag-input <0|1> [win]}"
  w=$(_pick "${2:-}")
  xprop -id "$w" -f STEAM_INPUT_FOCUS 32c -set STEAM_INPUT_FOCUS "$mode"
  echo "tagged window $w  STEAM_INPUT_FOCUS=$mode"
  _verify_atom "$w" STEAM_INPUT_FOCUS
  ;;
pick) _pick "${1:-}" ;;
help | --help | -h) usage ;;
*)
  echo "unknown command: $cmd" >&2
  echo >&2
  usage >&2
  exit 1
  ;;
esac
