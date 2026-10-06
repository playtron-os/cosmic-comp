# Headless compositor harness

`headless.sh` runs this compositor nested and headless, opens real clients in
it, drives the pointer and keyboard, and takes screenshots. Use it to check
window chrome, layer shells and placement without a device and without
touching your own desktop.

## How it is isolated

- cosmic-comp runs its winit backend inside a private `Xvfb` on a random free
  display number (`:100`-`:999`), kept only if its X lock names our server, so
  harnesses started at the same moment never share one. Nothing is drawn on
  your screen.
- Every process gets a fresh `HOME` and XDG directories under `CC_STATE`, a
  short runtime directory under `$TMPDIR`, and its own D-Bus session bus.
- The system bus points at a socket that does not exist, so the compositor
  cannot take logind inhibitors or talk to power daemons on the real machine.
- `--no-xwayland`; no systemd notification (the winit backend never sends it).
- With bubblewrap installed, the compositor and its clients run with
  `/run/systemd/journal` and `/run/user` hidden, so cosmic-comp's journald
  logging stays out of your journal and nothing finds your runtime dir.
- `down` kills only the pids `up` and `run` recorded, and removes the runtime
  directory.

## Nothing personal in the shots

Screenshots end up in public pull requests, so nothing on screen may name you,
your machine or where you are:

- The compositor and every client run as user `kora` on host `kora`: `USER`,
  `LOGNAME` and `HOSTNAME` are set, and with bubblewrap `getpwuid`,
  `gethostname` and `/etc/hostname` say so too (a private `/etc/passwd`,
  `/etc/group` and UTS namespace; nscd is hidden so it cannot answer for the
  real files). Without bubblewrap only the environment changes, and the
  harness says so.
- Clients start in the private `HOME` with an empty environment plus the
  session's and toolkit or renderer variables (`RUST_*`, `ICED_*`, `WGPU_*`,
  `LIBGL_*`, `COSMIC_*`, `KORA_*`, `AGENTOS_*` and the like). Name any other
  variable a client needs in `CC_CLIENT_ENV="NAME ..."`. The caller's prompt,
  `PROMPT_COMMAND`, history and working directory never reach them.
- `TZ=UTC`, and clients have no network (`CC_CLIENT_NET=1` gives it back), so
  the dock's clock and weather cannot place you.
- `PS1` is `kora@kora:~$ `; `halo-shots.sh` and `selftest` start foot with a
  fixed title and `bash --norc --noprofile`.
- Keep `CC_STATE` itself on a neutral path if a client might show it.

## Requirements

`Xvfb`, `xdotool`, `grim`, `wlr-randr`, `dbus-daemon`, plus the libraries winit
opens at run time (`libX11`, `libXcursor`, `libXi`, `libxkbcommon`, EGL). When
the tools are missing and `nix` exists, the script re-runs itself under
`nix shell` with them; outside the repo's `nix develop` shell it fetches the
libraries the same way. Rendering is software (`LIBGL_ALWAYS_SOFTWARE=1`), so
screenshots are deterministic across machines.

## Quick start

```bash
cargo build
# CC_THEMES: target/themes of an icetron-theme-playtron build (cargo build there)
export CC_THEMES=/path/to/icetron-theme-playtron/target/themes CC_STATE=/tmp/cc-shots
tools/headless/headless.sh up
tools/headless/headless.sh run foot --window-size-pixels=900x560
tools/headless/headless.sh shot /tmp/cc-shots/foot.png
tools/headless/headless.sh scale 1.5
tools/headless/headless.sh drag 960 300 700 500        # drag a window by its Halo
tools/headless/headless.sh mode light
tools/headless/headless.sh shot /tmp/cc-shots/foot-light.png
tools/headless/headless.sh down
```

Without `CC_THEMES` the compositor uses its built-in fallback theme, which has
bar headers rather than the Halo.

## Driving it

- Coordinates are output pixels, as they appear in a screenshot: logical
  coordinates times the scale.
- `press`, `release`, `click` and `drag` leave a frame between events, because
  iced ignores a press and release that land in the same frame. `dclick` is
  fast enough to count as a double click.
- `key super+m` sends keys and `type TEXT` types text; the compositor sees
  them first, the focused client gets the rest. `up` gives the nested window
  X keyboard focus and installs the repo's `data/keybindings.ron`.
- `selftest` checks the path end to end: it types into foot and presses Super+M,
  and fails unless both show up in a shot.
- Clients run with the nested environment; `eval "$(headless.sh env)"` gives a
  shell the same environment, e.g. to run `wlr-randr` or `grim` by hand.
- First-party iced apps run as they do on a device: pass their binary to
  `run`. Apps that ask for the Halo overlay mode get it from this compositor.
- Logs: `CC_STATE/comp.log` (stderr) and the runtime dir's `cosmic-comp.log`;
  `RUST_LOG` is passed through.

## Several outputs

`CC_OUTPUTS=2` (with, say, `CC_SIZE=3840x1080`) splits the nested window into
that many outputs side by side, `WINIT-0` on the left. Each is drawn on its own
and copied into its slice, so output placement, per-output desktops and moving
between outputs can be checked headless. Pointer coordinates span the whole
window, and `scale` sets every output.

## Comparing builds

Point `CC_BIN` at another build and use another `CC_STATE` to run two
compositors side by side, e.g. a `master` worktree for the "before" shots and
the branch for "after".

## Halo state shots

`halo-shots.sh OUTDIR` records the Halo in every window state the design
specifies (normal, the four overflow tiers, overlapping windows with the
background one hovered, Fill, snapped halves, fullscreen hidden and revealed, a
drag clamped at the top, an overlay-mode app) at scales 1 and 1.5, dark and
light, and runs `halo-measure.py` on each shot. `PANEL_BIN` adds the dock and
`HIVE_BIN` the overlay-mode app. The measurements land in
`OUTDIR/measure.jsonl`: pill height, the gap between pill and window, and the
window's distance from the usable area's top, against 32, 4 and 40 x scale.
The overlapping states are measured in dark mode only: light glass over another
window cannot be told from its shadow by pixels.
