//! One focus-gain clock shared by the Halo and its window outline.

use iced_core::time::{Duration, Instant};

use crate::backend::render::animations::motion::cubic_bezier_cp;

#[derive(Clone, Copy, Debug)]
pub struct FocusOutline {
    pub focused: bool,
    pub animate: bool,
    pub duration: Duration,
    pub curve: [f32; 4],
}

#[derive(Clone, Copy, Debug)]
pub struct FocusOutlineFrame {
    pub progress: f32,
    pub bounds: iced_core::Rectangle,
    pub radii: [u8; 4],
}

#[derive(Clone, Debug, Default)]
pub(super) struct FocusAnimation {
    focused: bool,
    pending: bool,
    started: Option<Instant>,
    duration: Duration,
    curve: [f32; 4],
    pub progress: f32,
}

impl FocusAnimation {
    /// Observe focus changes without spending animation time in input/layout.
    /// Only a compositor draw may start or advance the sweep.
    pub fn update(&mut self, settings: FocusOutline) {
        if !settings.focused {
            *self = Self::default();
            return;
        }
        self.duration = settings.duration;
        self.curve = settings.curve;
        if !settings.animate || settings.duration.is_zero() {
            self.pending = false;
            self.started = None;
            self.progress = 1.0;
        } else if !self.focused {
            self.pending = true;
            self.started = None;
            self.progress = 0.0;
        }
        self.focused = true;
    }

    pub fn frame(&mut self, now: Instant) -> f32 {
        if self.pending {
            self.pending = false;
            self.started = Some(now);
        }
        if let Some(started) = self.started {
            let elapsed =
                now.saturating_duration_since(started).as_secs_f32() / self.duration.as_secs_f32();
            self.progress = cubic_bezier_cp(elapsed.clamp(0.0, 1.0), self.curve);
            if elapsed >= 1.0 {
                self.progress = 1.0;
                self.started = None;
            }
        }
        self.progress
    }

    pub fn is_animating(&self) -> bool {
        self.pending || self.started.is_some()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn halo_focus_matches_the_prototype_timing_not_the_icetron_namesake() {
        use crate::{comp_theme::CompTheme, shell::element::header_bar::halo_focus_outline};
        let theme = CompTheme::default();
        let settings = halo_focus_outline(&theme, true, false);
        assert_eq!(settings.duration, Duration::from_millis(420));
        assert_eq!(settings.curve, [0.4, 0.0, 0.2, 1.0]);
        let mut animation = FocusAnimation::default();
        let now = Instant::now();
        animation.update(settings);
        animation.frame(now);
        animation.frame(now + settings.duration / 4);
        assert!((animation.progress - 0.23658736).abs() < 0.001);
        animation.update(halo_focus_outline(&theme, true, true));
        assert_eq!(animation.progress, 1.0);
        animation.update(settings);
        animation.frame(now + settings.duration / 2);
        assert_eq!(
            animation.progress, 1.0,
            "leaving fullscreen must not replay focus gain"
        );
        let mut tokens = icetron_themes::dynamic::DEFAULT_THEME_PAIR.load(true);
        tokens.duration_slower = 0.0;
        let theme = CompTheme::new(std::sync::Arc::new(tokens), true);
        assert!(!halo_focus_outline(&theme, true, false).animate);
    }

    #[test]
    fn focus_gain_runs_once_and_loss_cancels_immediately() {
        let now = Instant::now();
        let mut animation = FocusAnimation::default();
        let mut settings = FocusOutline {
            focused: true,
            animate: true,
            duration: Duration::from_millis(420),
            curve: [0.0, 0.0, 1.0, 1.0],
        };
        animation.update(settings);
        animation.frame(now);
        assert_eq!(animation.progress, 0.0);
        assert!(animation.is_animating());
        animation.frame(now + settings.duration / 2);
        assert!((animation.progress - 0.5).abs() < 0.01);
        settings.focused = false;
        animation.update(settings);
        assert_eq!(animation.progress, 0.0);
        assert!(!animation.is_animating());
        settings.focused = true;
        animation.update(settings);
        animation.frame(now + settings.duration);
        assert_eq!(animation.progress, 0.0);
        animation.frame(now + settings.duration * 2);
        assert_eq!(animation.progress, 1.0);
        assert!(!animation.is_animating());
        animation.update(settings);
        animation.frame(now + settings.duration * 3);
        assert_eq!(animation.progress, 1.0);
        assert!(!animation.is_animating());
    }

    #[test]
    fn fullscreen_and_zero_duration_skip_the_sweep() {
        for (animate, duration) in [(false, Duration::from_millis(420)), (true, Duration::ZERO)] {
            let mut animation = FocusAnimation::default();
            animation.update(FocusOutline {
                focused: true,
                animate,
                duration,
                curve: [0.0, 0.0, 1.0, 1.0],
            });
            assert_eq!(animation.progress, 1.0);
            assert!(!animation.is_animating());
        }
    }

    #[test]
    fn late_first_draw_does_not_consume_the_start_of_the_sweep() {
        let settings = crate::shell::element::header_bar::halo_focus_outline(
            &crate::comp_theme::CompTheme::default(),
            true,
            false,
        );
        let mut animation = FocusAnimation::default();
        let focused_at = Instant::now();
        animation.update(settings);
        // Repeated input/layout updates before a (very delayed) first draw
        // must leave the animation armed, not silently finish it offscreen.
        animation.update(settings);
        animation.update(settings);
        let first_draw = focused_at + Duration::from_secs(2);
        assert_eq!(animation.frame(first_draw), 0.0);
        assert!(animation.is_animating());
        animation.update(settings);
        let next = animation.frame(first_draw + Duration::from_millis(16));
        assert!(
            next > 0.0 && next < 0.01,
            "first frame must reveal just the tips, got {next}"
        );
        assert_eq!(animation.frame(first_draw + settings.duration), 1.0);
        assert!(!animation.is_animating());
    }
}
