# UI performance measurement

This compositor can produce a **defensible UI performance report** on demand for
due-diligence purposes. Reports are written by `src/perf/` and triggered by global
hotkeys:

- **`Ctrl+Alt+Super+Shift+F12`** — live UI report (frame rate, frametime, dropped
  frames, input latency, UI RAM).
- **`Ctrl+Alt+Super+Shift+F11`** — cold-start (app-launch) benchmark.

Each writes a report to `$HOME` and opens the default file manager focused on it.

## Output

| Hotkey | Files | Schema |
| --- | --- | --- |
| F12 | `cosmic-perf-<unix>.{json,txt}` | `cosmic-comp.perf/1` |
| F11 | `cosmic-coldstart-<unix>.{json,txt}` | `cosmic-comp.coldstart/2` |

The `.json` is machine-readable; the `.txt` is a human summary. After writing,
the report is revealed via freedesktop `FileManager1.ShowItems` (falls back to
opening the containing folder for file managers that don't implement it).

## Metrics and how accurate each one is

Be precise about this — the whole point is numbers that survive scrutiny.

| Metric | Status | Source |
| --- | --- | --- |
| Frame rate | **Ground truth** | DRM vblank presentation timestamps |
| Frametime / jitter (p50/p95/p99/p99.9) | **Ground truth** | `present[i] − present[i−1]` from the hardware clock |
| Dropped frames | **Ground truth** | DRM vblank **sequence-number** gaps (a gap of *N* = *N−1* skipped vblanks) |
| Render time | **Ground truth** | compositor wake→submit duration per frame |
| UI RAM (CPU) | **Ground truth** | kernel **PSS** from `/proc/<pid>/smaps_rollup` |
| UI RAM (GPU) | Best-effort | DRM `fdinfo` resident memory, deduplicated by `drm-client-id` (driver-dependent) |
| Input latency (pointer→scanout) | **Measured** (compositor pipeline) | libinput HW timestamp → DRM scanout; add a one-time HW-tester offset for end-to-end |
| Cold-start (launch→first frame) | **Measured** (relaunch harness) | exec → first-buffer commit, phased; true cold needs a page-cache drop (root) |

### Why the frame metrics are ground truth, not an approximation

cosmic-comp already measures frame timing for frame pacing — it has to, to wake
the right amount of time before each vblank (`src/backend/kms/surface/timings.rs`).
The perf report reads the **same** per-frame data the pacer uses: the hardware
vblank timestamp delivered in the DRM page-flip event and the vblank sequence
number. There is no sampling, polling, or estimation layer.

### Backend caveat (important)

Frame and input metrics are **only valid on the native `drm/kms` backend** (a real
session). The nested `winit`/`x11` backends use synthetic timing; the report
prints the backend and flags those as *NOT AUTHORITATIVE*, and omits per-output
frame stats for them. Always capture on bare metal.

## Live report — how to capture (F12)

1. Boot a real HumainOS/cosmic-comp session (DRM/KMS backend).
2. Drive the workload you want to characterize (scroll a list, animate a panel,
   move the pointer, open windows, etc.).
3. Press **`Ctrl+Alt+Super+Shift+F12`**.
4. The report is written and the file manager opens focused on
   `~/cosmic-perf-<unix>.txt`.

The frame history covers roughly the **last ~16 s** of presented frames per
output (`SAMPLE_CAP = 1000`). The report is written synchronously on key press (a
brief one-off cost); the frame data it reports was already collected before that
point, so the cost does not affect the numbers.

### Input latency (pointer→scanout)

Each pointer-motion event's libinput hardware timestamp
(`process_input_event` → `State::record_pointer_latency`) is enqueued on every
output. When a frame presents (`on_vblank`), the output consumes the pending
inputs processed at or before that frame's `render_start` and records one sample
— `present_ts − hw_ts` of the **newest** such input (the position that frame
actually displays; older batched inputs were superseded, so counting them would
inflate the tail — this matches how a hardware latency tester measures
displayed-state motion-to-photon). Both ends are CLOCK_MONOTONIC hardware
timestamps, so the **compositor-pipeline** latency is exact.

What software **cannot** see — USB/firmware latency *before* libinput and panel
pixel-response *after* scanout — is a fixed per-hardware offset. Measure it
**once** with a photodiode latency tester (OSLTT/LDAT) or a high-speed camera,
store the constant, and report `end-to-end = measured + offset` with both
components shown. This is why "100% accurate end-to-end input latency" needs the
one-time hardware step; the compositor portion alone is exact.

## Cold-start — active relaunch harness (F11)

Implemented in `src/perf/coldstart.rs`. Destructive (it repeatedly launches and
kills the app), hence a separate hotkey. For a configured target (default
**agentos-mail-suite**, the heaviest representative windowed app; override with
`COSMIC_COLDSTART_APP` / `COSMIC_COLDSTART_ITERS`), it:

1. kills existing instances of the target;
2. for N iterations: best-effort drops the page cache → `spawn_command`s the
   target and stamps `t0`, then — correlating by the launching app's PID/`comm` —
   captures three timestamps and kills the app;
3. writes `~/cosmic-coldstart-<unix>.{json,txt}` and reveals it.

### Phase breakdown

Each launch is broken into measured phases (correlated by process name across the
client-connect, `new_toplevel`, and first-buffer-commit hooks):

| Phase | Span | What it captures |
| --- | --- | --- |
| `connect` | `t0 → t_connect` | process start + runtime init + wayland connect |
| `build` | `t_connect → t_toplevel` | app constructs its UI / creates the window |
| `frame` | `t_toplevel → t1` | app renders its first frame of content |
| `total` | `t0 → t1` | launch(exec) → first-buffer commit |

`t1` is the **first-buffer commit** — the moment the app produced its first frame
of content; on-screen scanout follows within one refresh interval. That final
commit→scanout tail is the only unmeasured piece (it would need surface-thread
plumbing to attribute a present to a specific new surface) and is bounded by one
refresh, so it is documented rather than estimated.

The report lists per-iteration `connect / build / frame / total` plus p50 per
phase, so you can see *where* a slow launch spends its time (runtime startup vs UI
construction vs first render).

True **cold** start needs the page-cache drop, which requires root. cosmic-comp
usually isn't root, so the drop is best-effort: the report records whether it was
effective and highlights **iteration 1** (the coldest). For all-cold iterations,
run the compositor as root or boot fresh.

## Cross-checks (do these for the due-diligence pack)

Each metric should agree with an independent tool:

- **FPS / frametime** — run a known workload (e.g. `vkmark`, a fullscreen video)
  and compare the report's fps against the tool's own counter. With the
  `profile-with-tracy` feature, capture a Tracy trace simultaneously and compare
  the per-frame timeline.
- **UI RAM** — compare the report's PSS against `smem -P <name>` or
  `cat /proc/<pid>/smaps_rollup`. They should match to the KiB.
- **Dropped frames** — induce a stall (e.g. a heavy GPU load) and confirm the
  sequence-gap count rises; cross-check against Tracy's `sequence delta` plot.
- **Input latency** — calibrate the software component against a photodiode
  latency tester; the tester's reading minus the report's `input→scanout` gives
  the fixed USB+panel offset to add for end-to-end.
- **Cold-start** — sanity-check against a stopwatch or an app-side `clock_gettime`
  stamp for one launch.
