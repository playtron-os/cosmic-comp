// SPDX-License-Identifier: GPL-3.0-only

//! Heuristics for choosing a sensible default ("recommended") display scale.
//!
//! This lives in the config crate — rather than the compositor binary — so that
//! other tools (cosmic-settings, a first-run / FTUE app) can compute and label
//! the exact same "Recommended" value the compositor seeds onto a freshly-seen
//! output, e.g. "125% (Recommended)".

/// Compute a recommended fractional scale for an output from its physical size
/// and active resolution.
///
/// `embedded` is `true` for built-in panels (i.e. the connector interface is
/// embedded DisplayPort / eDP); it biases the heuristic towards the laptop DPI
/// target. The caller maps its connector type onto this flag — in the
/// compositor that is `interface == connector::Interface::EmbeddedDisplayPort`.
///
/// `monitor_size_mm` is the physical size reported by EDID; `(0, 0)` (or other
/// non-sensical values) trigger a resolution-only fallback.
pub fn calculate_scale(embedded: bool, monitor_size_mm: (u32, u32), resolution: (u16, u16)) -> f64 {
    let (w_mm, h_mm) = monitor_size_mm;
    let (w_px, h_px) = resolution;
    let shorter_res = w_px.min(h_px);
    let fallback_scale = match shorter_res {
        px if px <= 1600 => 1.0,
        _ => 2.0,
    };
    if w_mm == 0 || h_mm == 0 {
        // possibly projector, but could just be some no-brand display
        return fallback_scale;
    }

    let (w_in, h_in) = (w_mm as f64 / 25.4, h_mm as f64 / 25.4);
    // due to edid's setting non-sensicle values,
    // lets be really careful we don't devide by 0 (or non-normal values) when deriving values from size.
    // (also the size should be positive, but the u16 arguments already force that.)
    if !w_in.is_normal() || !h_in.is_normal() {
        return fallback_scale;
    }

    let diag = (w_in.powf(2.) * h_in.powf(2.)).sqrt();
    let dpi = (w_px as f64 / w_in + h_px as f64 / h_in) / 2.0;

    match diag {
        _diag if diag < 20. || embedded => {
            // likely laptop
            scale_from_dpi(dpi, 144, shorter_res)
        }
        // 16:10 has a height of 19.6, anything lower is most likely an ultrawide
        _diag if diag >= 40. && h_in >= 19. => {
            // likely TV
            match shorter_res {
                px if px <= 1200 => 1.0,
                _ => 2.0,
            }
        }
        _ => {
            // likely desktop
            scale_from_dpi(dpi, 120, shorter_res)
        }
    }
}

pub fn scale_from_dpi(dpi: f64, step_size: u32, shorter_px: u16) -> f64 {
    let scale = (dpi / step_size as f64) * 100.0;

    // snap to 50%
    let scale = ((scale.round() as u32).next_multiple_of(50) as f64) / 100.0;

    // max values
    let max = match shorter_px {
        px if px <= 1200 => 1.0, // 125% is usually a worse experience than 100% atm
        // for 1440p and weird variants we let the algorithm take over, but limit the max, this is inspired by what windows does
        px if px <= 1600 => 1.5,
        _ => 2.0, // never go higher than 200% by default though (Note for the future: Tablets? Phones?)
    };

    let min = match shorter_px {
        px if px >= 2000 => 2.0, // 4k should default to 200%, as we prefer integer scales
        _ => 1.0,                // never go lower than 100%
    };

    scale.min(max).max(min)
}

#[cfg(test)]
mod test {
    use super::calculate_scale;

    #[test]
    fn test_scale() {
        // `embedded` mirrors the compositor's
        // `interface == connector::Interface::EmbeddedDisplayPort` check. LVDS
        // laptop panels report as non-embedded and currently fall through to
        // the desktop heuristic, which happens to yield the same result for
        // these inputs — so the laptop cases are asserted for both flag values.
        fn scale(embedded: bool, diag_in: f64, w_px: u16, h_px: u16) -> i32 {
            let aspect = (w_px as f64) / (h_px as f64);
            let diag_mm = diag_in * 25.4;
            let h_mm = diag_mm / (aspect.powf(2.0) + 1.0).sqrt();
            let w_mm = h_mm * aspect;
            (calculate_scale(embedded, (w_mm as u32, h_mm as u32), (w_px, h_px)) * 100.0) as i32
        }

        // Laptops (eDP -> embedded; LVDS -> non-embedded)
        for &embedded in &[true, false] {
            // 14 inch 3:2
            assert_eq!(scale(embedded, 14.0, 3000, 2000), 200);

            // Various sizes 16:9
            for &diag_in in &[14.0, 15.6, 17.3] {
                assert_eq!(scale(embedded, diag_in, 1920, 1080), 100);
                assert_eq!(scale(embedded, diag_in, 2560, 1440), 150);
                assert_eq!(scale(embedded, diag_in, 3840, 2160), 200);
            }

            // Various sizes 16:10
            for &diag_in in &[14.0, 16.0] {
                assert_eq!(scale(embedded, diag_in, 1920, 1200), 100);
                assert_eq!(scale(embedded, diag_in, 2560, 1600), 150);
                assert_eq!(scale(embedded, diag_in, 3840, 2400), 200);
            }
        }

        // Desktops (non-embedded)
        {
            // 24 inch 16:9
            assert_eq!(scale(false, 24.0, 1920, 1080), 100);
            assert_eq!(scale(false, 24.0, 2560, 1440), 150);
            assert_eq!(scale(false, 24.0, 3840, 2160), 200);

            // Larger sizes 16:9
            for &diag_in in &[27.0, 32.0, 38.0] {
                assert_eq!(scale(false, diag_in, 1920, 1080), 100);
                assert_eq!(scale(false, diag_in, 2560, 1440), 100);
                assert_eq!(scale(false, diag_in, 3840, 2160), 200);
            }

            // Smaller sizes 21:9
            for &diag_in in &[26.0, 29.0] {
                assert_eq!(scale(false, diag_in, 2560, 1080), 100);
                assert_eq!(scale(false, diag_in, 3440, 1440), 150);
                assert_eq!(scale(false, diag_in, 5120, 2160), 200);
            }

            // Larger sizes 21:9
            for &diag_in in &[34.0, 45.0] {
                assert_eq!(scale(false, diag_in, 2560, 1080), 100);
                assert_eq!(scale(false, diag_in, 3440, 1440), 100);
                assert_eq!(scale(false, diag_in, 5120, 2160), 200);
            }

            // Various sizes 32:9
            for &diag_in in &[45.0, 49.0, 57.0] {
                assert_eq!(scale(false, diag_in, 3840, 1080), 100);
                assert_eq!(scale(false, diag_in, 5120, 1440), 100);
                assert_eq!(scale(false, diag_in, 7680, 2160), 200);
            }
        }

        // TVs (non-embedded)
        {
            // Various sizes 16:9
            for &diag_in in &[40.0, 42.0, 48.0, 55.0, 65.0, 77.0, 83.0] {
                assert_eq!(scale(false, diag_in, 1920, 1080), 100);
                assert_eq!(scale(false, diag_in, 2560, 1440), 200);
                assert_eq!(scale(false, diag_in, 3840, 2160), 200);
            }
        }

        // Zero sized displays (projectors, invalid EDID)
        {
            assert_eq!(scale(false, 0.0, 1920, 1080), 100);
            assert_eq!(scale(false, 0.0, 2560, 1440), 100);
            assert_eq!(scale(false, 0.0, 3840, 2160), 200);
        }
    }
}
