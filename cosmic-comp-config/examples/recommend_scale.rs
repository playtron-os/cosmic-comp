// SPDX-License-Identifier: GPL-3.0-only

//! Print the recommended display scale for a panel using the same heuristic the
//! compositor seeds onto a freshly-seen output. Lets you validate the heuristic
//! against real hardware without restarting the session — feed it the values
//! `cosmic-randr list` / `wlr-randr` report (physical size in mm + the current
//! mode), with `embedded=1` for built-in eDP panels (connector name `eDP-*`).
//!
//! Usage:
//!   cargo run -p cosmic-comp-config --features output --example recommend_scale \
//!       -- <embedded:0|1> <w_mm> <h_mm> <w_px> <h_px>

use cosmic_comp_config::output::scale::calculate_scale;

fn main() {
    let a: Vec<String> = std::env::args().skip(1).collect();
    if a.len() != 5 {
        eprintln!("usage: recommend_scale <embedded:0|1> <w_mm> <h_mm> <w_px> <h_px>");
        std::process::exit(2);
    }
    let embedded = a[0] != "0";
    let size_mm = (a[1].parse().unwrap(), a[2].parse().unwrap());
    let res = (a[3].parse().unwrap(), a[4].parse().unwrap());
    let scale = calculate_scale(embedded, size_mm, res);
    println!(
        "embedded={embedded} size={}x{}mm res={}x{} -> recommended {}% (scale {scale})",
        size_mm.0,
        size_mm.1,
        res.0,
        res.1,
        (scale * 100.0).round() as i32,
    );
}
