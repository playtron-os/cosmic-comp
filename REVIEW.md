# Review policy

Preserve existing `one.playtron.GameMode` signatures. New controls must be additive,
authorized through the same caller checks, and safe for a client on an older compositor.

Confirmed consumers:

- [playserve](https://github.com/playtron-os/playserve):
  `crates/models/src/zbus_generated_interfaces/game_mode.rs`,
  `crates/system/src/display/game_mode_manager.rs`,
  `crates/system/src/display/gamescope_watch.rs`, and
  `service/src/app/common/launcher.rs`.
- [kora-workspaces](https://github.com/playtron-os/kora-workspaces):
  `src/router.rs` forwards the interface and Unix file descriptors;
  `src/policy.rs` reserves peer attestation for the router.

For gaming changes, verify first-visible-frame placement, replacement-window gaps,
callback delivery through filtered surface trees, fractional output scaling, and
filter fallback without a destination-size change. Keep pixel comparisons separate
from timing and scanout measurements on hardware. Follow the build and lint commands
in `CLAUDE.md`.
