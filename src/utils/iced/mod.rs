//! Iced 0.15 compositor UI element using `UserInterface` + `Cache`.
//!
//! Renders iced widget trees into smithay buffers via `iced_tiny_skia`,
//! without any libcosmic dependency. Uses `CompTheme` for theming.

use std::{
    any::Any,
    collections::{HashMap, HashSet},
    fmt,
    hash::{Hash, Hasher},
    mem::ManuallyDrop,
    sync::{
        Arc, Mutex, OnceLock,
        atomic::{AtomicBool, Ordering},
        mpsc::Receiver,
    },
    thread::ThreadId,
    time::Instant,
};

use super::iced_profiler::{ICED_PROFILER, UpdateRecord, UpdateSource, iced_perf_logging_enabled};

mod elastic_row;
mod focus;
pub mod pulse;
mod tooltip;
mod visibility;
pub use elastic_row::ElasticRow;
use focus::FocusAnimation;
pub use focus::{FocusOutline, FocusOutlineFrame};
pub use visibility::Visibility;
use visibility::{VisibilityAnimation, VisibilityFrame};

// iced 0.15 direct imports (no libcosmic re-exports)
use iced_core::widget::Operation as WidgetOperation;
use iced_core::{
    Color, Element, Font, Length, Pixels, Point as IcedPoint, Size as IcedSize,
    event::Event,
    keyboard::{Event as KeyboardEvent, Modifiers as IcedModifiers},
    layout::Limits,
    mouse::{
        Button as MouseButton, Cursor, Event as MouseEvent, Interaction as MouseInteraction,
        ScrollDelta,
    },
    renderer::Style as RendererStyle,
    time::Instant as IcedInstant,
    touch::{Event as TouchEvent, Finger},
    widget::Tree,
    window::{self, Event as WindowEvent},
};
use iced_graphics::damage;
use iced_graphics::{Viewport, text::font_system};
use iced_runtime::{
    Action, Task,
    task::into_stream,
    user_interface::{self, UserInterface},
};
use iced_tiny_skia::Layer;

use super::iced_keymap;
use futures_util::{FutureExt, StreamExt};
use ordered_float::OrderedFloat;
use smithay::{
    backend::{
        allocator::Fourcc,
        input::{ButtonState, KeyState},
        renderer::{
            ImportMem,
            element::{
                Id, Kind,
                memory::{MemoryRenderBuffer, MemoryRenderBufferRenderElement},
                utils::RescaleRenderElement,
            },
        },
    },
    desktop::space::{RenderZindex, SpaceElement},
    input::{
        Seat,
        keyboard::{KeyboardTarget, KeysymHandle, ModifiersState},
        pointer::{
            AxisFrame, ButtonEvent, GestureHoldBeginEvent, GestureHoldEndEvent,
            GesturePinchBeginEvent, GesturePinchEndEvent, GesturePinchUpdateEvent,
            GestureSwipeBeginEvent, GestureSwipeEndEvent, GestureSwipeUpdateEvent, MotionEvent,
            PointerTarget, RelativeMotionEvent,
        },
        touch::{
            DownEvent, FrameMarker, MotionEvent as TouchMotionEvent, OrientationEvent, ShapeEvent,
            TouchTarget, UpEvent,
        },
    },
    output::Output,
    reexports::calloop::{self, LoopHandle, RegistrationToken, futures::Scheduler},
    render_elements,
    utils::{
        Buffer as BufferCoords, IsAlive, Logical, Physical, Point, Rectangle, Scale, Serial, Size,
        Transform,
    },
};

// MERGE: upstream also imports `utils::iced::state::State`; the fork replaced that module
// with `UserInterface` + `Cache` driven directly (iced 0.15, no libcosmic), so it is dropped.
use crate::backend::render::{
    IndicatorShader, OutlineElement, OutlineFocus,
    element::AsGlowRenderer,
    wayland::blur_effect::{BlurElement, BlurState, configured_blur_strength},
};

// --- Theme ---
pub use crate::comp_theme::CompTheme;

/// Type alias for iced elements rendered in the compositor.
/// Uses `iced_core::Theme` so standard iced widgets and icetron components
/// can be used without custom Catalog impls.
pub type CompElement<'a, Message> =
    Element<'a, Message, iced_core::Theme, iced_tiny_skia::Renderer>;

#[derive(Default)]
struct PendingRedraw(AtomicBool);

fn request_redraw(output: &Output) {
    output
        .user_data()
        .insert_if_missing_threadsafe(PendingRedraw::default);
    output
        .user_data()
        .get::<PendingRedraw>()
        .unwrap()
        .0
        .store(true, Ordering::Release);
}

/// Consume a frame requested by compositor Iced widgets on this output.
pub(crate) fn take_redraw_request(output: &Output) -> bool {
    output
        .user_data()
        .get::<PendingRedraw>()
        .is_some_and(|pending| pending.0.swap(false, Ordering::AcqRel))
}

// --- Public API (unchanged interface) ---

pub struct IcedElement<P: Program + Send + 'static>(pub(crate) Arc<Mutex<IcedElementInternal<P>>>);

impl<P: Program + Send + 'static> fmt::Debug for IcedElement<P> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}

// SAFETY: `IcedElementInternal` is force-`Send` so window decorations can be
// cached by KMS surface render threads (see `push_render_elements`). Two of its
// fields are bound to the main event loop and are *not* thread-safe:
// calloop's `LoopHandle` and `Scheduler` are both `Rc`-based, with `RefCell`
// interiors, so dereferencing or dropping them off the loop thread races the
// main loop's own borrows and refcount updates.
//
// Both are therefore kept behind guards that uphold the real invariant:
//   * `handle` is a [`ProgramLoop`], whose only off-thread-reachable operation
//     (`insert_idle`) parks the callback for the main loop instead of touching
//     calloop directly.
//   * `scheduler` is only reached through `schedule_task`, which refuses to
//     touch it off the event-loop thread.
//   * both are `ManuallyDrop` and are *moved* (never dereferenced) into
//     [`PENDING_LOOP_TEARDOWN`] by `Drop`, so the refcount decrements happen on
//     the main thread in [`drain_deferred_loop_work`].
// A move does not touch a refcount, so every `Rc` mutation stays on one thread.
unsafe impl<P: Program + Send + 'static> Send for IcedElementInternal<P> {}

/// The thread running the compositor's event loop.
static MAIN_THREAD: OnceLock<ThreadId> = OnceLock::new();

/// Record the calling thread as the event-loop thread. Called once from
/// `crate::run` before any element exists, so [`on_main_thread`] is accurate for
/// the whole life of the process (including element construction during
/// startup, which happens before the first `refresh()`).
pub fn mark_main_thread() {
    let _ = MAIN_THREAD.set(std::thread::current().id());
}

/// Whether the caller is on the compositor's event-loop thread. Conservatively
/// `false` if the thread was never marked, so anything racing startup is
/// deferred rather than run inline.
fn on_main_thread() -> bool {
    MAIN_THREAD.get() == Some(&std::thread::current().id())
}

/// Idle callbacks parked by [`ProgramLoop::insert_idle`] when it was called off
/// the event-loop thread. Drained by [`drain_deferred_loop_work`].
#[allow(clippy::type_complexity)]
static PENDING_PROGRAM_IDLES: Mutex<Vec<Box<dyn FnOnce(&mut crate::state::State) + Send>>> =
    Mutex::new(Vec::new());

/// Thread-safe façade over the compositor's event-loop handle, handed to iced
/// [`Program`]s.
///
/// A `Program`'s `update()` runs on the main loop *and* on KMS surface render
/// threads, because [`IcedElement::push_render_elements`] drives animation frames
/// from wherever the surface is being composited. calloop's `LoopHandle` is
/// `Rc`/`RefCell`-based, so calling `insert_idle` on it from a render thread
/// borrows a `RefCell` the main loop also borrows while dispatching — the
/// corruption that surfaces as a "RefCell already borrowed" panic or, when the
/// racing writes interleave differently, a silently wedged event loop.
///
/// This wrapper keeps main-thread behaviour byte-for-byte identical (the
/// callback is handed straight to calloop) and only changes the off-thread case,
/// where the callback is parked for the main loop to pick up in `refresh()`.
pub struct ProgramLoop {
    handle: LoopHandle<'static, crate::state::State>,
}

impl ProgramLoop {
    fn new(handle: LoopHandle<'static, crate::state::State>) -> Self {
        ProgramLoop { handle }
    }

    /// Run `callback` on the main loop with access to the compositor state.
    ///
    /// Dispatches inline when already on the event-loop thread; otherwise parks
    /// it for the next [`drain_deferred_loop_work`].
    pub fn insert_idle<F>(&self, callback: F)
    where
        F: FnOnce(&mut crate::state::State) + Send + 'static,
    {
        if on_main_thread() {
            self.handle.insert_idle(callback);
        } else {
            // Poison-tolerant: the queue is only ever pushed/taken, so a panic
            // elsewhere leaves it perfectly usable, and this path is reachable
            // from `Drop` where a panic would abort.
            PENDING_PROGRAM_IDLES
                .lock()
                .unwrap_or_else(|err| err.into_inner())
                .push(Box::new(callback));
        }
    }

    /// A handle over a throwaway event loop, for driving a `Program::update`
    /// directly in a test. Idle callbacks parked on it are never run.
    #[cfg(test)]
    pub(crate) fn test_handle() -> Self {
        // Leaked so the handle outlives the loop it came from, which is all a
        // test needs: nothing dispatches it.
        let event_loop = Box::leak(Box::new(
            calloop::EventLoop::<crate::state::State>::try_new().expect("test event loop"),
        ));
        Self::new(event_loop.handle())
    }

    /// The raw loop handle. Only sound on the event-loop thread — internal use
    /// only (element construction and cloning, both main-thread operations).
    pub(crate) fn raw(&self) -> &LoopHandle<'static, crate::state::State> {
        &self.handle
    }
}

impl fmt::Debug for ProgramLoop {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("ProgramLoop { .. }")
    }
}

impl<P: Program + Send + 'static> Clone for IcedElement<P> {
    fn clone(&self) -> Self {
        IcedElement(self.0.clone())
    }
}

impl<P: Program + Send + 'static> PartialEq for IcedElement<P> {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}
impl<P: Program + Send + 'static> Eq for IcedElement<P> {}

impl<P: Program + Send + 'static> Hash for IcedElement<P> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (Arc::as_ptr(&self.0) as usize).hash(state)
    }
}

// --- Program trait (same interface, different Element type) ---

pub trait Program {
    // `'static` so a dropped element's loop-bound state can be type-erased into
    // the main-thread teardown queue (see `PENDING_LOOP_TEARDOWN`).
    type Message: std::fmt::Debug + Send + 'static;

    fn update(
        &mut self,
        message: Self::Message,
        loop_handle: &ProgramLoop,
        last_seat: Option<&(Seat<crate::state::State>, Serial)>,
    ) -> Task<Self::Message> {
        let _ = (message, loop_handle, last_seat);
        Task::none()
    }

    /// Returns the view element.
    /// `theme` provides design tokens for pre-baking styles into closures.
    fn view<'a>(&'a self, theme: &'a CompTheme) -> CompElement<'a, Self::Message>;

    /// Program name for profiling logs. Defaults to the type name.
    fn program_name() -> &'static str {
        std::any::type_name::<Self>()
    }

    fn background_color(&self, _theme: &CompTheme) -> Color {
        Color::TRANSPARENT
    }

    /// Animate the complete UI buffer and its backdrop together when specified.
    /// The view stays fully visible; this owns its fade and translation.
    fn visibility(&self, _theme: &CompTheme) -> Option<Visibility> {
        None
    }

    /// Opt in to a compositor-drawn Halo outline and its shared focus clock.
    fn focus_outline(&self, _theme: &CompTheme) -> Option<FocusOutline> {
        None
    }

    /// Bounds and corner radii in Smithay's BR, TR, BL, TL order.
    fn backdrop_blur(
        &self,
        theme: &CompTheme,
        size: Size<i32, Logical>,
        _layers: &[Layer],
        radii: [u8; 4],
    ) -> Option<(iced_core::Rectangle, [u8; 4])> {
        theme.header_backdrop_blur().then(|| {
            (
                iced_core::Rectangle::with_size(IcedSize::new(size.w as f32, size.h as f32)),
                radii,
            )
        })
    }

    fn foreground(
        &self,
        pixels: &mut tiny_skia::PixmapMut<'_>,
        damage: &[Rectangle<i32, BufferCoords>],
        scale: f32,
        theme: &CompTheme,
    ) {
        let _ = (pixels, damage, scale, theme);
    }
}

// --- Internal state ---

pub(crate) struct IcedElementInternal<P: Program + Send + 'static> {
    // draw buffer
    additional_scale: f64,
    outputs: HashSet<Output>,
    buffers: HashMap<OrderedFloat<f64>, (MemoryRenderBuffer, Option<(Vec<Layer>, Color)>)>,
    pending_realloc: bool,
    blur: BlurState,
    tooltip: tooltip::Surface,

    // state
    size: Size<i32, Logical>,
    last_seat: Arc<Mutex<Option<(Seat<crate::state::State>, Serial)>>>,
    cursor_pos: Option<Point<f64, Logical>>,
    touch_map: HashMap<Finger, IcedPoint>,
    last_touch_frame: Option<FrameMarker>,
    last_touch_serial: Option<Serial>,

    // iced 0.15: UserInterface cache + manual event queue (replaces State<ProgramWrapper>)
    theme: CompTheme,
    iced_theme: iced_core::Theme,
    renderer: iced_tiny_skia::Renderer,
    cache: user_interface::Cache,
    event_queue: Vec<Event>,
    mouse_interaction: MouseInteraction,
    needs_redraw: bool,
    /// A dismissed surface keeps its last widget paint for the compositor exit.
    render_only: bool,
    visibility: Option<VisibilityAnimation>,
    visibility_frame: VisibilityFrame,
    focus: FocusAnimation,
    outline_id: Id,

    // the actual program
    program: P,

    // futures
    //
    // `handle` and `scheduler` are loop-bound (`Rc`-based) and must only ever be
    // dropped on the event-loop thread; `Drop` moves them out of these guards
    // into `PENDING_LOOP_TEARDOWN` for the main loop to reap. See the
    // `unsafe impl Send` above for the full invariant.
    handle: ManuallyDrop<ProgramLoop>,
    scheduler: ManuallyDrop<Scheduler<Option<<P as Program>::Message>>>,
    executor_token: Option<RegistrationToken>,
    rx: Receiver<Option<<P as Program>::Message>>,
    /// Widget operations queued since the last build (a focus request, say),
    /// applied to the next tree ahead of its events. The task executor keeps
    /// only a Task's messages, so this is how a host reaches into the widgets.
    pending_operations: Vec<Box<dyn WidgetOperation<()>>>,
}

impl<P: Program + Send + Clone + 'static> Clone for IcedElementInternal<P> {
    fn clone(&self) -> Self {
        // Cloning registers a new executor source, so this is a main-thread-only
        // operation (it is reached from `IcedElement::deep_clone`).
        let handle = self.handle.raw().clone();
        let (executor, scheduler) = calloop::futures::executor().expect("Out of file descriptors");
        let (tx, rx) = std::sync::mpsc::channel();
        let executor_token = handle
            .insert_source(executor, move |message, _, _| {
                let _ = tx.send(message);
            })
            .ok();

        let renderer = iced_tiny_skia::Renderer::new(Font::DEFAULT, Pixels(16.0));

        IcedElementInternal {
            additional_scale: self.additional_scale,
            outputs: self.outputs.clone(),
            buffers: self.buffers.clone(),
            pending_realloc: self.pending_realloc,
            blur: BlurState::default(),
            tooltip: tooltip::Surface::default(),
            size: self.size,
            last_seat: self.last_seat.clone(),
            cursor_pos: self.cursor_pos,
            touch_map: self.touch_map.clone(),
            last_touch_frame: None,
            last_touch_serial: None,
            theme: self.theme.clone(),
            iced_theme: self.theme.to_iced_theme(),
            renderer,
            cache: user_interface::Cache::default(),
            event_queue: Vec::new(),
            mouse_interaction: MouseInteraction::default(),
            needs_redraw: false,
            render_only: self.render_only,
            visibility: self.visibility.clone(),
            visibility_frame: self.visibility_frame,
            focus: self.focus.clone(),
            outline_id: Id::new(),
            program: self.program.clone(),
            handle: ManuallyDrop::new(ProgramLoop::new(handle)),
            scheduler: ManuallyDrop::new(scheduler),
            executor_token,
            pending_operations: Vec::new(),
            rx,
        }
    }
}

impl<P: Program + Send + 'static> fmt::Debug for IcedElementInternal<P> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("IcedElementInternal")
            .field("additional_scale", &self.additional_scale)
            .field(
                "outputs",
                &self.outputs.iter().map(|o| o.name()).collect::<Vec<_>>(),
            )
            .field("buffers", &"...")
            .field("size", &self.size)
            .field("pending_realloc", &self.pending_realloc)
            .field("last_seat", &self.last_seat)
            .field("cursor_pos", &self.cursor_pos)
            .field("touch_map", &self.touch_map)
            .field("last_touch_frame", &self.last_touch_frame)
            .field("last_touch_serial", &self.last_touch_serial)
            .field("theme", &"...")
            .field("renderer", &"...")
            .field("cache", &"...")
            .field("event_queue_len", &self.event_queue.len())
            .field("mouse_interaction", &self.mouse_interaction)
            .field("handle", &self.handle)
            // Elided like `buffers`/`renderer`/`cache` above: Scheduler's Debug
            // walks calloop's Rc<State<T>> and borrows its RefCell, which is
            // only sound on the loop thread. Nothing formats an element off it
            // today, but printing it must not be the thing that breaks that.
            .field("scheduler", &"...")
            .field("executor_token", &self.executor_token)
            .field("rx", &self.rx)
            .finish()
    }
}

/// The loop-bound remains of an `IcedElementInternal` that has been dropped,
/// waiting to be torn down on the event-loop thread.
///
/// An element's `Drop` can run anywhere: a KMS surface render thread routinely
/// holds the last `Arc` clone of a window's decoration in its cached frame, and
/// releases it when the window is fullscreened or unmapped (game-mode entry does
/// this in bulk). Everything in here is `Rc`-based, so releasing it on that
/// thread races the main loop's own refcount updates and `RefCell` borrows —
/// undefined behaviour that shows up either as a "RefCell already borrowed"
/// panic or as a silently wedged event loop.
struct LoopTeardown {
    /// The element's executor source. `LoopHandle::remove` borrows a `RefCell`
    /// the main loop also borrows while dispatching, so it must run at a
    /// post-dispatch point on the loop thread.
    token: Option<RegistrationToken>,
    /// The element's `ProgramLoop` and `Scheduler`, type-erased so one queue can
    /// serve every `P`. Only ever dropped — never dereferenced.
    loop_bound: Box<dyn Any>,
}

// SAFETY: a `LoopTeardown` is only ever *moved* between threads, which does not
// touch the refcounts inside it, and is only ever dropped by
// `drain_deferred_loop_work` on the event-loop thread. Nothing dereferences
// `loop_bound` off that thread.
unsafe impl Send for LoopTeardown {}

static PENDING_LOOP_TEARDOWN: Mutex<Vec<LoopTeardown>> = Mutex::new(Vec::new());

/// Run the work that `IcedElement`s deferred to the event-loop thread: idle
/// callbacks parked by [`ProgramLoop::insert_idle`], and the teardown of
/// elements dropped on a render thread.
///
/// Must be called from the event loop at a post-dispatch point (see
/// `crate::refresh`), where `LoopHandle::remove`/`insert_idle` are safe.
pub fn drain_deferred_loop_work(handle: &LoopHandle<'static, crate::state::State>) {
    // Only ever reached from `crate::refresh`, so this is the loop thread —
    // belt and braces in case `mark_main_thread` was ever missed.
    mark_main_thread();

    // Take before running: anything queued *by* this drain lands in a fresh Vec
    // and is picked up next time, rather than being iterated mid-drain.
    let idles = std::mem::take(
        &mut *PENDING_PROGRAM_IDLES
            .lock()
            .unwrap_or_else(|err| err.into_inner()),
    );
    for callback in idles {
        handle.insert_idle(callback);
    }

    let teardowns = std::mem::take(
        &mut *PENDING_LOOP_TEARDOWN
            .lock()
            .unwrap_or_else(|err| err.into_inner()),
    );
    for teardown in teardowns {
        if let Some(token) = teardown.token {
            handle.remove(token);
        }
        // Explicit for emphasis: *this* is the refcount decrement that must
        // happen here rather than on whichever thread dropped the element.
        drop(teardown.loop_bound);
    }
}

impl<P: Program + Send + 'static> Drop for IcedElementInternal<P> {
    fn drop(&mut self) {
        // May run on any thread (typically a surface render thread that held the
        // last Arc to this element), so nothing here may dereference or release
        // the loop-bound fields — move them out intact and let the main loop do
        // it. See `LoopTeardown`.
        //
        // SAFETY: `drop` runs exactly once and neither field is read afterwards.
        let handle = unsafe { ManuallyDrop::take(&mut self.handle) };
        let scheduler = unsafe { ManuallyDrop::take(&mut self.scheduler) };

        // Poison-tolerant: a panic in `drop` would abort the process.
        PENDING_LOOP_TEARDOWN
            .lock()
            .unwrap_or_else(|err| err.into_inner())
            .push(LoopTeardown {
                token: self.executor_token.take(),
                loop_bound: Box::new((handle, scheduler)),
            });
    }
}

// --- Public IcedElement methods ---

impl<P: Program + Send + 'static> IcedElement<P> {
    pub fn new(
        program: P,
        size: impl Into<Size<i32, Logical>>,
        handle: LoopHandle<'static, crate::state::State>,
        theme: CompTheme,
    ) -> IcedElement<P> {
        let size = size.into();
        let last_seat = Arc::new(Mutex::new(None));
        let renderer = iced_tiny_skia::Renderer::new(Font::DEFAULT, Pixels(16.0));

        // Load icetron fonts into the global font system so themed text renders correctly.
        // Guarded by Once to avoid redundant write-lock + iteration on every IcedElement.
        static FONTS_LOADED: std::sync::Once = std::sync::Once::new();
        FONTS_LOADED.call_once(|| {
            let mut fs = font_system().write().expect("Write font system");
            for font_data in icetron_themes::fonts::ALL {
                fs.load_font(std::borrow::Cow::Borrowed(*font_data));
            }
        });

        let (executor, scheduler) = calloop::futures::executor().expect("Out of file descriptors");
        let (tx, rx) = std::sync::mpsc::channel();
        let executor_token = handle
            .insert_source(executor, move |message, _, _| {
                let _ = tx.send(message);
            })
            .ok();

        let mut internal = IcedElementInternal {
            additional_scale: 1.0,
            outputs: HashSet::new(),
            buffers: HashMap::new(),
            pending_realloc: false,
            blur: BlurState::default(),
            tooltip: tooltip::Surface::default(),
            size,
            cursor_pos: None,
            last_seat,
            touch_map: HashMap::new(),
            iced_theme: theme.to_iced_theme(),
            last_touch_frame: None,
            last_touch_serial: None,
            theme,
            renderer,
            cache: user_interface::Cache::new(),
            event_queue: Vec::new(),
            mouse_interaction: MouseInteraction::default(),
            needs_redraw: false,
            render_only: false,
            visibility: None,
            visibility_frame: VisibilityFrame::VISIBLE,
            focus: FocusAnimation::default(),
            outline_id: Id::new(),
            program,
            handle: ManuallyDrop::new(ProgramLoop::new(handle)),
            scheduler: ManuallyDrop::new(scheduler),
            executor_token,
            pending_operations: Vec::new(),
            rx,
        };
        internal.update(UpdateSource::Forced);

        IcedElement(Arc::new(Mutex::new(internal)))
    }

    pub fn with_program<R>(&self, func: impl FnOnce(&P) -> R) -> R {
        let internal = self.0.lock().unwrap();
        func(&internal.program)
    }

    /// The painted backdrop body, excluding render-only shadow padding.
    pub(crate) fn backdrop_bounds(&self) -> Option<iced_core::Rectangle> {
        let mut guard = self.0.lock().unwrap();
        let IcedElementInternal {
            program,
            theme,
            size,
            renderer,
            visibility_frame,
            ..
        } = &mut *guard;
        program
            .backdrop_blur(theme, *size, renderer.layers(), [0; 4])
            .map(|(bounds, _)| visibility_frame.bounds(bounds))
    }

    /// Hit-test the painted body (not its shadow gutter), including surface
    /// animation and application/zoom scale, in element-relative logical units.
    pub(crate) fn backdrop_input_bounds(&self) -> Option<Rectangle<f64, Logical>> {
        let mut guard = self.0.lock().unwrap();
        let IcedElementInternal {
            program,
            theme,
            size,
            renderer,
            visibility_frame,
            additional_scale,
            ..
        } = &mut *guard;
        program
            .backdrop_blur(theme, *size, renderer.layers(), [0; 4])
            .map(|(bounds, _)| {
                let bounds = visibility_frame.bounds(bounds);
                Rectangle::new(
                    (bounds.x as f64, bounds.y as f64).into(),
                    (bounds.width as f64, bounds.height as f64).into(),
                )
                .upscale(*additional_scale)
            })
    }

    pub fn minimum_size(&self) -> Size<i32, Logical> {
        let internal = self.0.lock().unwrap();
        let mut element = internal.program.view(&internal.theme);
        let tree = &mut Tree::new(element.as_widget());
        let node = element
            .as_widget_mut()
            .layout(
                tree,
                &internal.renderer,
                &Limits::new(IcedSize::ZERO, IcedSize::INFINITE)
                    .width(Length::Shrink)
                    .height(Length::Shrink),
            )
            .size();
        Size::from((node.width.ceil() as i32, node.height.ceil() as i32))
    }

    /// The raw event-loop handle.
    ///
    /// Cloning an `Rc`-based handle is only sound on the event-loop thread, so
    /// callers must already be there — which every current caller is (element
    /// construction and input/grab handling). Program code reached from
    /// `update()` must use the [`ProgramLoop`] it is handed instead, since that
    /// can also run on a render thread.
    pub fn loop_handle(&self) -> LoopHandle<'static, crate::state::State> {
        debug_assert!(
            on_main_thread(),
            "IcedElement::loop_handle() off the event-loop thread races calloop's Rc"
        );
        self.0.lock().unwrap().handle.raw().clone()
    }

    pub fn resize(&self, size: Size<i32, Logical>) {
        let mut internal = self.0.lock().unwrap();
        let internal_ref = &mut *internal;
        if internal_ref.size == size {
            return;
        }

        internal_ref.size = size;
        internal_ref.pending_realloc = true;
        internal_ref.update(UpdateSource::Forced);
    }

    pub fn set_additional_scale(&self, scale: f64) {
        {
            let mut internal = self.0.lock().unwrap();
            let internal_ref = &mut *internal;
            if internal_ref.additional_scale == scale {
                return;
            }

            internal_ref.additional_scale = scale;
        }
        self.refresh();
    }

    pub fn force_update(&self) {
        self.0.lock().unwrap().update(UpdateSource::Forced);
    }

    /// The program must already request hidden visibility. Release queued
    /// input and retain its last paint, like a hidden layer-shell surface:
    /// only the compositor transform/alpha and live backdrop keep updating.
    pub(crate) fn animate_exit(&self) {
        self.0.lock().unwrap().animate_exit(IcedInstant::now());
    }

    /// A dismissed compositor surface may be retained until its exit ends.
    /// Query the clock, not the last sampled frame: an output may have stopped
    /// rendering while the close was in flight.
    pub(crate) fn is_fully_hidden(&self) -> bool {
        self.is_fully_hidden_at(IcedInstant::now())
    }

    pub(crate) fn is_fully_hidden_at(&self, now: IcedInstant) -> bool {
        self.0
            .lock()
            .unwrap()
            .visibility
            .as_ref()
            .is_some_and(|animation| animation.is_fully_hidden(now))
    }

    /// Read the element's theme. Ported from upstream (which hands out a `cosmic::Theme`)
    /// onto the fork's [`CompTheme`].
    pub fn with_theme<R: 'static>(&self, f: impl FnOnce(&CompTheme) -> R) -> R {
        let guard = self.0.lock().unwrap();
        f(&guard.theme)
    }

    pub fn set_theme(&self, theme: CompTheme) {
        let mut guard = self.0.lock().unwrap();
        guard.iced_theme = theme.to_iced_theme();
        guard.theme = theme;
        guard.tooltip.invalidate();
        guard.update(UpdateSource::Forced);
    }

    pub fn force_redraw(&self) {
        let mut internal = self.0.lock().unwrap();
        for (_buffer, old_primitives) in internal.buffers.values_mut() {
            *old_primitives = None;
        }
    }

    pub fn current_size(&self) -> Size<i32, Logical> {
        let internal = self.0.lock().unwrap();
        internal
            .size
            .to_f64()
            .upscale(internal.additional_scale)
            .to_i32_round()
    }

    pub fn queue_message(&self, msg: P::Message) {
        // In iced 0.15, we process messages immediately by calling program.update
        // and scheduling any resulting tasks. We need to trigger a UI rebuild.
        let mut internal = self.0.lock().unwrap();
        let internal = &mut *internal; // Allow split-borrowing of fields
        let task = internal.program.update(
            msg,
            &internal.handle,
            internal.last_seat.lock().unwrap().as_ref(),
        );
        internal.schedule_task(task);
    }

    /// Queue a widget operation (a focus request, say) for the next build,
    /// and rebuild now so it lands before the next input arrives.
    pub fn queue_operation(&self, operation: impl WidgetOperation<()> + 'static) {
        self.0
            .lock()
            .unwrap()
            .pending_operations
            .push(Box::new(operation));
        self.force_update();
    }

    /// Returns the current mouse interaction state from the last UI update.
    pub fn mouse_interaction(&self) -> MouseInteraction {
        self.0.lock().unwrap().mouse_interaction
    }
}

impl<P: Program + Send + 'static + Clone> IcedElement<P> {
    pub fn deep_clone(&self) -> Self {
        let internal = self.0.lock().unwrap();
        IcedElement(Arc::new(Mutex::new(internal.clone())))
    }
}

// --- Core update cycle (rewritten for iced 0.15) ---

impl<P: Program + Send + 'static> IcedElementInternal<P> {
    fn advance_exit(&mut self, now: IcedInstant) {
        if !self.render_only {
            return;
        }
        self.sync_visibility(now);
        if self
            .visibility
            .as_ref()
            .is_some_and(|animation| animation.is_animating(now))
        {
            for output in &self.outputs {
                request_redraw(output);
            }
        }
        self.needs_redraw = false;
    }

    fn animate_exit(&mut self, now: IcedInstant) {
        self.render_only = true;
        self.event_queue.clear();
        self.cursor_pos = None;
        self.touch_map.clear();
        self.needs_redraw = false;
        self.sync_visibility(now);
        self.tooltip.report = None;
        self.tooltip.sync(&self.theme, now);
        // Even an instant close needs a frame to erase the previous image.
        for output in &self.outputs {
            request_redraw(output);
        }
    }

    fn start_visibility_frame(&mut self, now: IcedInstant) {
        if self
            .visibility
            .as_mut()
            .is_some_and(|animation| animation.start_on_draw(now))
        {
            self.sync_visibility(now);
        }
    }

    fn focus_outline_frame(
        &mut self,
        radii: [u8; 4],
        now: IcedInstant,
    ) -> Option<FocusOutlineFrame> {
        let outline = self.program.focus_outline(&self.theme)?;
        let (bounds, radii) =
            self.program
                .backdrop_blur(&self.theme, self.size, self.renderer.layers(), radii)?;
        let progress = self.focus.frame(now);
        if self.focus.is_animating() {
            for output in &self.outputs {
                request_redraw(output);
            }
        }
        Some(FocusOutlineFrame {
            progress,
            bounds,
            radii,
            bottom_border: outline.bottom_border,
        })
    }

    fn sync_visibility(&mut self, now: IcedInstant) {
        self.visibility = self.program.visibility(&self.theme).map(|settings| {
            let mut animation = self
                .visibility
                .take()
                .unwrap_or_else(|| VisibilityAnimation::new(settings));
            animation.update(settings, now);
            animation
        });
        self.visibility_frame = self
            .visibility
            .as_ref()
            .map_or(VisibilityFrame::VISIBLE, |animation| animation.frame(now))
            .around(IcedPoint::new(
                self.size.w as f32 * 0.5,
                self.size.h as f32 * 0.5,
            ));
        if let Some(settings) = self.program.focus_outline(&self.theme) {
            self.focus.update(settings);
        } else {
            self.focus = FocusAnimation::default();
        }
    }

    /// Input positions are stored in the stationary buffer's coordinate space.
    /// Reproject them every frame, including frames with no pointer motion.
    fn local_position(&self, position: IcedPoint) -> IcedPoint {
        self.visibility_frame.unproject(position)
    }

    /// Schedule a Task returned by program.update() onto the calloop executor.
    fn schedule_task(&self, task: Task<P::Message>) {
        if let Some(stream) = into_stream(task) {
            // `Scheduler::schedule` borrows `active_tasks`, a `RefCell` shared
            // with the executor source running on the event loop — sound only on
            // that thread. `update()` also runs on KMS surface render threads
            // (see `push_render_elements`), so refuse rather than race.
            //
            // Unreachable today: every `Program::update` returns `Task::none()`,
            // so `into_stream` yields `None`. This exists so that stops being a
            // silent trap the moment a program returns a real `Task`.
            if !on_main_thread() {
                tracing::error!(
                    program = P::program_name(),
                    "iced Task returned off the event-loop thread; dropping it \
                     rather than racing calloop's executor. Defer the work with \
                     ProgramLoop::insert_idle instead."
                );
                return;
            }

            let _ = self
                .scheduler
                .schedule(stream.into_future().map(|f| match f.0 {
                    Some(Action::Output(msg)) => Some(msg),
                    _ => None,
                }));
        }
    }

    #[profiling::function]
    fn update(&mut self, source: UpdateSource) {
        let update_start = Instant::now();
        let force = matches!(source, UpdateSource::Forced);

        // Drain async task results and process them through the program
        while let Ok(Some(message)) = self.rx.try_recv() {
            let task = self.program.update(
                message,
                &self.handle,
                self.last_seat.lock().unwrap().as_ref(),
            );
            self.schedule_task(task);
        }

        if self.event_queue.is_empty() && !force {
            // Record skipped update
            if iced_perf_logging_enabled()
                && let Ok(mut profiler) = ICED_PROFILER.try_lock()
            {
                profiler.record_update(UpdateRecord {
                    program_name: P::program_name(),
                    source,
                    view_duration: std::time::Duration::ZERO,
                    build_duration: std::time::Duration::ZERO,
                    ui_update_duration: std::time::Duration::ZERO,
                    draw_duration: std::time::Duration::ZERO,
                    message_loop_duration: std::time::Duration::ZERO,
                    total_duration: update_start.elapsed(),
                    had_messages: false,
                    skipped: true,
                });
                profiler.maybe_report();
            }
            return;
        }

        // Rebuilt widgets finalize paint state on RedrawRequested, including
        // button status. Every draw must receive it after the queued input.
        if !matches!(
            self.event_queue.last(),
            Some(Event::Window(WindowEvent::RedrawRequested(_)))
        ) {
            self.event_queue
                .push(Event::Window(WindowEvent::RedrawRequested(
                    IcedInstant::now(),
                )));
        }

        let now = match self.event_queue.last() {
            Some(Event::Window(WindowEvent::RedrawRequested(now))) => *now,
            _ => unreachable!("every update ends with RedrawRequested"),
        };
        self.sync_visibility(now);
        let transform = self.visibility_frame;
        for event in &mut self.event_queue {
            match event {
                Event::Mouse(MouseEvent::CursorMoved { position })
                | Event::Touch(TouchEvent::FingerPressed { position, .. })
                | Event::Touch(TouchEvent::FingerMoved { position, .. })
                | Event::Touch(TouchEvent::FingerLifted { position, .. })
                | Event::Touch(TouchEvent::FingerLost { position, .. }) => {
                    *position = transform.unproject(*position)
                }
                _ => {}
            }
        }
        let cursor = self
            .cursor_pos
            .map(|p| self.local_position(IcedPoint::new(p.x as f32, p.y as f32)))
            .map(Cursor::Available)
            .unwrap_or(Cursor::Unavailable);

        // Phase 1: view() — build the element tree
        let view_start = Instant::now();
        let element = self.program.view(&self.theme);
        let view_duration = view_start.elapsed();

        // Phase 2: build — create UserInterface from element tree
        let build_start = Instant::now();
        let bounds = IcedSize::new(self.size.w as f32, self.size.h as f32);
        let cache = std::mem::take(&mut self.cache);
        let mut interface = UserInterface::build(element, bounds, cache, &mut self.renderer);
        let build_duration = build_start.elapsed();
        // Operations a host queued act on the fresh tree, ahead of the events
        // that assume they already have.
        for mut operation in self.pending_operations.drain(..) {
            interface.operate(&self.renderer, operation.as_mut());
        }

        // Phase 3: ui_update — process queued events, collecting messages
        let ui_update_start = Instant::now();
        let mut messages = Vec::new();
        let (state, _statuses) =
            interface.update(&self.event_queue, cursor, &mut self.renderer, &mut messages);
        self.event_queue.clear();
        let ui_update_duration = ui_update_start.elapsed();

        // Store the mouse interaction and check for animation redraw requests.
        match state {
            iced_runtime::user_interface::State::Updated {
                mouse_interaction,
                redraw_request,
                ..
            } => {
                self.mouse_interaction = mouse_interaction;
                // If widgets requested a redraw (e.g. animations in progress),
                // flag this element so the next compositor frame drives another update.
                let wants_redraw = redraw_request != window::RedrawRequest::Wait;
                self.needs_redraw = wants_redraw;
            }
            iced_runtime::user_interface::State::Outdated { .. } => {
                self.needs_redraw = true;
            }
        }

        // Phase 4: draw — populate renderer layers for rasterization
        let draw_start = Instant::now();
        let style = RendererStyle {
            text_color: self.theme.on_bg_color(),
        };
        interface.draw(&mut self.renderer, &self.iced_theme, &style, cursor);
        self.tooltip.report = tooltip::collect(&mut interface, &self.renderer);
        let draw_duration = draw_start.elapsed();

        // Preserve widget tree state for next frame
        self.cache = interface.into_cache();

        // Phase 5: message loop — process widget messages and rebuild if needed
        let msg_start = Instant::now();
        let had_messages = !messages.is_empty();
        if had_messages {
            for msg in messages {
                let task =
                    self.program
                        .update(msg, &self.handle, self.last_seat.lock().unwrap().as_ref());
                self.schedule_task(task);
            }
            // State changed from widget messages — rebuild the view and re-draw
            // so the UI reflects the new state immediately (e.g. trigger highlight
            // on dropdown open) without waiting for the next input event.
            let element = self.program.view(&self.theme);
            let cache = std::mem::take(&mut self.cache);
            let mut interface = UserInterface::build(element, bounds, cache, &mut self.renderer);
            let mut redraw_messages = Vec::new();
            let (state, _) = interface.update(
                &[Event::Window(WindowEvent::RedrawRequested(
                    IcedInstant::now(),
                ))],
                cursor,
                &mut self.renderer,
                &mut redraw_messages,
            );
            match state {
                user_interface::State::Updated {
                    redraw_request,
                    mouse_interaction,
                    ..
                } => {
                    self.needs_redraw |= redraw_request != window::RedrawRequest::Wait;
                    self.mouse_interaction = mouse_interaction;
                }
                user_interface::State::Outdated { .. } => self.needs_redraw = true,
            }
            interface.draw(&mut self.renderer, &self.iced_theme, &style, cursor);
            self.tooltip.report = tooltip::collect(&mut interface, &self.renderer);
            self.cache = interface.into_cache();
            // Do not discard messages emitted by redraw-aware widgets. Apply
            // them now and rebuild next frame, avoiding an unbounded rebuild loop.
            self.needs_redraw |= !redraw_messages.is_empty();
            for msg in redraw_messages {
                let task =
                    self.program
                        .update(msg, &self.handle, self.last_seat.lock().unwrap().as_ref());
                self.schedule_task(task);
            }
        }
        let message_loop_duration = msg_start.elapsed();

        self.sync_visibility(now);
        // A hidden owner must not create a new tooltip (e.g. a keyboard-focused
        // control on hidden fullscreen chrome). Existing tips still fade out.
        if self
            .program
            .visibility(&self.theme)
            .is_some_and(|visibility| !visibility.visible)
        {
            self.tooltip.report = None;
        }
        self.tooltip.sync(&self.theme, now);
        self.needs_redraw |= self
            .visibility
            .as_ref()
            .is_some_and(|animation| animation.is_animating(now));
        // Focus changes only GPU uniforms. Request an output frame without
        // treating it as an Iced widget redraw (layout/text/tooltip rebuild).
        if self.needs_redraw || self.focus.is_animating() || self.tooltip.is_animating(now) {
            for output in &self.outputs {
                request_redraw(output);
            }
        }

        let total_duration = update_start.elapsed();

        // Record to profiler
        if iced_perf_logging_enabled()
            && let Ok(mut profiler) = ICED_PROFILER.try_lock()
        {
            profiler.record_update(UpdateRecord {
                program_name: P::program_name(),
                source,
                view_duration,
                build_duration,
                ui_update_duration,
                draw_duration,
                message_loop_duration,
                total_duration,
                had_messages,
                skipped: false,
            });
            profiler.maybe_report();
        }
    }
}

// --- Input handling (unchanged interface, updated internals) ---

impl<P: Program + Send + 'static> PointerTarget<crate::state::State> for IcedElement<P> {
    fn enter(
        &self,
        seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
        event: &MotionEvent,
    ) {
        let mut internal = self.0.lock().unwrap();
        internal
            .event_queue
            .push(Event::Mouse(MouseEvent::CursorEntered));
        let event_location = event.location.downscale(internal.additional_scale);
        let position = IcedPoint::new(event_location.x as f32, event_location.y as f32);
        internal
            .event_queue
            .push(Event::Mouse(MouseEvent::CursorMoved { position }));
        internal.cursor_pos = Some(event_location);
        *internal.last_seat.lock().unwrap() = Some((seat.clone(), event.serial));
        internal.update(UpdateSource::Input);
    }

    fn motion(
        &self,
        seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
        event: &MotionEvent,
    ) {
        let mut internal = self.0.lock().unwrap();
        let event_location = event.location.downscale(internal.additional_scale);
        let position = IcedPoint::new(event_location.x as f32, event_location.y as f32);
        internal
            .event_queue
            .push(Event::Mouse(MouseEvent::CursorMoved { position }));
        internal.cursor_pos = Some(event_location);
        *internal.last_seat.lock().unwrap() = Some((seat.clone(), event.serial));
        internal.update(UpdateSource::Input);
    }

    fn relative_motion(
        &self,
        _seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
        _event: &RelativeMotionEvent,
    ) {
    }

    fn button(
        &self,
        seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
        event: &ButtonEvent,
    ) {
        let mut internal = self.0.lock().unwrap();
        let button = match event.button {
            0x110 => MouseButton::Left,
            0x111 => MouseButton::Right,
            0x112 => MouseButton::Middle,
            x => MouseButton::Other(x as u16),
        };
        internal.event_queue.push(Event::Mouse(match event.state {
            ButtonState::Pressed => MouseEvent::ButtonPressed(button),
            ButtonState::Released => MouseEvent::ButtonReleased(button),
        }));
        *internal.last_seat.lock().unwrap() = Some((seat.clone(), event.serial));
        internal.update(UpdateSource::Input);
    }

    fn axis(
        &self,
        _seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
        frame: AxisFrame,
    ) {
        let mut internal = self.0.lock().unwrap();
        internal
            .event_queue
            .push(Event::Mouse(MouseEvent::WheelScrolled {
                delta: if let Some(discrete) = frame.v120 {
                    ScrollDelta::Lines {
                        x: discrete.0 as f32 / 120.,
                        y: discrete.1 as f32 / 120.,
                    }
                } else {
                    ScrollDelta::Pixels {
                        x: frame.axis.0 as f32,
                        y: frame.axis.1 as f32,
                    }
                },
            }));
        internal.update(UpdateSource::Input);
    }

    fn frame(&self, _seat: &Seat<crate::state::State>, _data: &mut crate::state::State) {}

    fn leave(
        &self,
        _seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
        _serial: Serial,
        _time: u32,
    ) {
        let mut internal = self.0.lock().unwrap();
        internal.cursor_pos = None;
        internal
            .event_queue
            .push(Event::Mouse(MouseEvent::CursorLeft));
        internal.update(UpdateSource::Input);
    }

    fn gesture_swipe_begin(
        &self,
        _: &Seat<crate::state::State>,
        _: &mut crate::state::State,
        _: &GestureSwipeBeginEvent,
    ) {
    }
    fn gesture_swipe_update(
        &self,
        _: &Seat<crate::state::State>,
        _: &mut crate::state::State,
        _: &GestureSwipeUpdateEvent,
    ) {
    }
    fn gesture_swipe_end(
        &self,
        _: &Seat<crate::state::State>,
        _: &mut crate::state::State,
        _: &GestureSwipeEndEvent,
    ) {
    }
    fn gesture_pinch_begin(
        &self,
        _: &Seat<crate::state::State>,
        _: &mut crate::state::State,
        _: &GesturePinchBeginEvent,
    ) {
    }
    fn gesture_pinch_update(
        &self,
        _: &Seat<crate::state::State>,
        _: &mut crate::state::State,
        _: &GesturePinchUpdateEvent,
    ) {
    }
    fn gesture_pinch_end(
        &self,
        _: &Seat<crate::state::State>,
        _: &mut crate::state::State,
        _: &GesturePinchEndEvent,
    ) {
    }
    fn gesture_hold_begin(
        &self,
        _: &Seat<crate::state::State>,
        _: &mut crate::state::State,
        _: &GestureHoldBeginEvent,
    ) {
    }
    fn gesture_hold_end(
        &self,
        _: &Seat<crate::state::State>,
        _: &mut crate::state::State,
        _: &GestureHoldEndEvent,
    ) {
    }
}

impl<P: Program + Send + 'static> TouchTarget<crate::state::State> for IcedElement<P> {
    fn down(
        &self,
        seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
        event: &DownEvent,
    ) {
        let mut internal = self.0.lock().unwrap();
        let id = Finger(i32::from(event.slot) as u64);
        let event_location = event.location.downscale(internal.additional_scale);
        let position = IcedPoint::new(event_location.x as f32, event_location.y as f32);
        internal
            .event_queue
            .push(Event::Touch(TouchEvent::FingerPressed { id, position }));
        internal.touch_map.insert(id, position);
        internal.cursor_pos = Some(event_location);
        internal.last_touch_serial = Some(event.serial);
        *internal.last_seat.lock().unwrap() = Some((seat.clone(), event.serial));
        internal.update(UpdateSource::Input);
    }

    fn up(
        &self,
        seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
        event: &UpEvent,
    ) {
        let mut internal = self.0.lock().unwrap();
        let id = Finger(i32::from(event.slot) as u64);
        if let Some(position) = internal.touch_map.remove(&id) {
            *internal.last_seat.lock().unwrap() =
                Some((seat.clone(), internal.last_touch_serial.unwrap()));
            internal
                .event_queue
                .push(Event::Touch(TouchEvent::FingerLifted { id, position }));
            internal.update(UpdateSource::Input);
        }
    }

    fn motion(
        &self,
        seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
        event: &TouchMotionEvent,
    ) {
        let mut internal = self.0.lock().unwrap();
        let id = Finger(i32::from(event.slot) as u64);
        let event_location = event.location.downscale(internal.additional_scale);
        let position = IcedPoint::new(event_location.x as f32, event_location.y as f32);
        *internal.last_seat.lock().unwrap() =
            Some((seat.clone(), internal.last_touch_serial.unwrap()));
        internal
            .event_queue
            .push(Event::Touch(TouchEvent::FingerMoved { id, position }));
        internal.touch_map.insert(id, position);
        internal.cursor_pos = Some(event_location);
        internal.update(UpdateSource::Input);
    }

    fn frame(
        &self,
        _seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
        frame: FrameMarker,
    ) {
        self.0.lock().unwrap().last_touch_frame = Some(frame);
    }

    fn cancel(
        &self,
        _seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
        frame: FrameMarker,
    ) {
        let mut internal = self.0.lock().unwrap();
        internal.last_touch_frame = Some(frame);
        for (id, position) in std::mem::take(&mut internal.touch_map) {
            internal
                .event_queue
                .push(Event::Touch(TouchEvent::FingerLost { id, position }));
        }
        internal.update(UpdateSource::Input);
    }

    fn shape(
        &self,
        _seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
        _event: &ShapeEvent,
    ) {
    }

    fn orientation(
        &self,
        _seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
        _event: &OrientationEvent,
    ) {
    }

    fn last_frame(
        &self,
        _seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
    ) -> Option<FrameMarker> {
        self.0.lock().unwrap().last_touch_frame
    }
}

impl<P: Program + Send + 'static> KeyboardTarget<crate::state::State> for IcedElement<P> {
    fn enter(
        &self,
        _seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
        keys: Vec<KeysymHandle<'_>>,
        _serial: Serial,
    ) {
        let mut internal = self.0.lock().unwrap();
        for key in &keys {
            let keysym = key.modified_sym();
            let iced_key = iced_keymap::keysym_to_iced_key(keysym);
            let modified_key = iced_key.clone();
            let physical_key = iced_keymap::keycode_to_physical(key.raw_code());
            let location = iced_keymap::keysym_to_location(keysym);
            internal
                .event_queue
                .push(Event::Keyboard(KeyboardEvent::KeyPressed {
                    key: iced_key,
                    modified_key,
                    physical_key,
                    location,
                    modifiers: IcedModifiers::empty(),
                    text: keysym.key_char().filter(|c| !c.is_control()).map(|c| {
                        let mut buf = [0u8; 4];
                        iced_core::SmolStr::new(c.encode_utf8(&mut buf))
                    }),
                    repeat: false,
                }));
        }
        internal.update(UpdateSource::Input);
    }

    fn leave(
        &self,
        _seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
        _serial: Serial,
    ) {
        // Iced doesn't track held keys internally, so nothing to release.
    }

    fn key(
        &self,
        seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
        key: KeysymHandle<'_>,
        state: KeyState,
        serial: Serial,
        _time: u32,
    ) {
        let mut internal = self.0.lock().unwrap();
        let keysym = key.modified_sym();
        let iced_key = iced_keymap::keysym_to_iced_key(keysym);
        let modified_key = iced_key.clone();
        let physical_key = iced_keymap::keycode_to_physical(key.raw_code());
        let location = iced_keymap::keysym_to_location(keysym);
        let modifiers = IcedModifiers::empty(); // modifiers() is called separately by smithay

        let event = match state {
            KeyState::Pressed => KeyboardEvent::KeyPressed {
                key: iced_key,
                modified_key,
                physical_key,
                location,
                modifiers,
                text: keysym.key_char().filter(|c| !c.is_control()).map(|c| {
                    let mut buf = [0u8; 4];
                    iced_core::SmolStr::new(c.encode_utf8(&mut buf))
                }),
                repeat: false,
            },
            KeyState::Released => KeyboardEvent::KeyReleased {
                key: iced_key,
                modified_key,
                physical_key,
                location,
                modifiers,
            },
        };
        internal.event_queue.push(Event::Keyboard(event));
        *internal.last_seat.lock().unwrap() = Some((seat.clone(), serial));
        internal.update(UpdateSource::Input);
    }

    fn modifiers(
        &self,
        _seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
        modifiers: ModifiersState,
        _serial: Serial,
    ) {
        let mut internal = self.0.lock().unwrap();
        let mut mods = IcedModifiers::empty();
        if modifiers.shift {
            mods.insert(IcedModifiers::SHIFT);
        }
        if modifiers.alt {
            mods.insert(IcedModifiers::ALT);
        }
        if modifiers.ctrl {
            mods.insert(IcedModifiers::CTRL);
        }
        if modifiers.logo {
            mods.insert(IcedModifiers::LOGO);
        }
        internal
            .event_queue
            .push(Event::Keyboard(KeyboardEvent::ModifiersChanged(mods)));
        internal.update(UpdateSource::Input);
    }
}

impl<P: Program + Send + 'static> IsAlive for IcedElement<P> {
    fn alive(&self) -> bool {
        true
    }
}

impl<P: Program + Send + 'static> SpaceElement for IcedElement<P> {
    fn bbox(&self) -> Rectangle<i32, Logical> {
        let internal = self.0.lock().unwrap();
        Rectangle::from_size(
            internal
                .size
                .to_f64()
                .upscale(internal.additional_scale)
                .to_i32_round(),
        )
    }

    fn is_in_input_region(&self, _point: &Point<f64, Logical>) -> bool {
        true
    }

    fn set_activate(&self, activated: bool) {
        let mut internal = self.0.lock().unwrap();
        internal.event_queue.push(Event::Window(if activated {
            WindowEvent::Focused
        } else {
            WindowEvent::Unfocused
        }));
        internal.update(UpdateSource::Input);
    }

    fn output_enter(&self, output: &Output, _overlap: Rectangle<i32, Logical>) {
        let mut internal = self.0.lock().unwrap();
        let scale = output.current_scale().fractional_scale() * internal.additional_scale;

        let internal_size = internal.size;
        internal.buffers.entry(OrderedFloat(scale)).or_insert({
            let buffer_size = internal_size
                .to_f64()
                .to_buffer(scale, Transform::Normal)
                .to_i32_round();

            (
                MemoryRenderBuffer::new(Fourcc::Argb8888, buffer_size, 1, Transform::Normal, None),
                None,
            )
        });

        internal.outputs.insert(output.clone());
        std::mem::drop(internal);
        self.refresh();
    }

    fn output_leave(&self, output: &Output) {
        self.0.lock().unwrap().outputs.remove(output);
        self.refresh();
    }

    fn z_index(&self) -> u8 {
        RenderZindex::Shell as u8
    }

    #[profiling::function]
    fn refresh(&self) {
        let mut internal = self.0.lock().unwrap();
        let internal_ref = &mut *internal;
        internal_ref.buffers.retain(|scale, _| {
            internal_ref.outputs.iter().any(|o| {
                o.current_scale().fractional_scale() * internal_ref.additional_scale == **scale
            })
        });
        for scale in internal_ref
            .outputs
            .iter()
            .map(|o| {
                OrderedFloat(o.current_scale().fractional_scale() * internal_ref.additional_scale)
            })
            .filter(|scale| !internal_ref.buffers.contains_key(scale))
            .collect::<Vec<_>>()
            .into_iter()
        {
            let buffer_size = internal_ref
                .size
                .to_f64()
                .to_buffer(*scale, Transform::Normal)
                .to_i32_round();
            internal_ref.buffers.insert(
                scale,
                (
                    MemoryRenderBuffer::new(
                        Fourcc::Argb8888,
                        buffer_size,
                        1,
                        Transform::Normal,
                        None,
                    ),
                    None,
                ),
            );
        }
        internal.update(UpdateSource::Refresh);
    }
}

// --- Render elements (rasterization via iced_tiny_skia) ---
//
// MERGE: upstream replaced the `AsRenderElements` impl with this push-based API so the
// frosted-glass backdrop can be ordered below the rasterized UI buffer.

impl<P: Program + Send + 'static> IcedElement<P> {
    pub fn push_render_elements<R>(
        &self,
        renderer: &mut R,
        location: Point<i32, Physical>,
        scale: Scale<f64>,
        alpha: f32,
        radii: [u8; 4],
        push_above: &mut dyn FnMut(IcedRenderElement<R>),
        push_below: Option<&mut dyn FnMut(IcedRenderElement<R>)>,
    ) where
        R: AsGlowRenderer + ImportMem,
        R::TextureId: Send + Clone + 'static,
    {
        self.push_render_elements_with_focus(
            renderer, location, scale, alpha, radii, push_above, push_below,
        );
    }

    /// Return the exact frame used for the Halo, so its window cannot sample
    /// a different instant (or reacquire state after another output advances it).
    pub fn push_render_elements_with_focus<R>(
        &self,
        renderer: &mut R,
        location: Point<i32, Physical>,
        mut scale: Scale<f64>,
        alpha: f32,
        radii: [u8; 4],
        push_above: &mut dyn FnMut(IcedRenderElement<R>),
        push_below: Option<&mut dyn FnMut(IcedRenderElement<R>)>,
    ) -> Option<FocusOutlineFrame>
    where
        R: AsGlowRenderer + ImportMem,
        R::TextureId: Send + Clone + 'static,
    {
        let mut internal = self.0.lock().unwrap();
        let internal_ref = &mut *internal;

        // Drive animation frames: if a previous update requested a redraw,
        // inject a RedrawRequested event so animation widgets can advance.
        let element_id = Arc::as_ptr(&self.0) as usize;
        internal_ref.advance_exit(IcedInstant::now());
        if internal_ref.needs_redraw {
            internal_ref.needs_redraw = false;
            internal_ref
                .event_queue
                .push(Event::Window(WindowEvent::RedrawRequested(
                    IcedInstant::now(),
                )));
            internal_ref.update(UpdateSource::AnimRedraw);

            // Track animation burst
            if iced_perf_logging_enabled() {
                if let Ok(mut profiler) = ICED_PROFILER.try_lock() {
                    profiler.animation_burst_frame(element_id);
                }

                // Check if animation ended (no further redraw requested)
                if !internal_ref.needs_redraw
                    && let Ok(mut profiler) = ICED_PROFILER.try_lock()
                {
                    profiler.animation_burst_end(element_id);
                }
            }
        } else {
            // No animation active — end any tracked burst
            if iced_perf_logging_enabled()
                && let Ok(mut profiler) = ICED_PROFILER.try_lock()
            {
                profiler.animation_burst_end(element_id);
            }
        }

        // Track animation burst starts (needs_redraw was set by update above)
        if iced_perf_logging_enabled()
            && internal_ref.needs_redraw
            && let Ok(mut profiler) = ICED_PROFILER.try_lock()
        {
            profiler.animation_burst_start(element_id);
        }
        let frame_time = IcedInstant::now();
        internal_ref.start_visibility_frame(frame_time);
        let focus_frame = internal_ref.focus_outline_frame(radii, frame_time);
        internal_ref.tooltip.advance(frame_time);
        if internal_ref.tooltip.is_animating(frame_time) {
            for output in &internal_ref.outputs {
                request_redraw(output);
            }
        }
        // Tooltips have their own fade lifetime. Let an outgoing chip finish
        // even as the Halo fades/slides away; still inherit window-level alpha.
        internal_ref.tooltip.push(
            renderer,
            &internal_ref.theme,
            location.to_f64(),
            scale * internal_ref.additional_scale,
            internal_ref.additional_scale,
            alpha,
            push_above,
        );
        let alpha = internal_ref.visibility_frame.alpha(alpha);
        if alpha <= 0.0 {
            return focus_frame;
        }
        if std::mem::replace(&mut internal_ref.pending_realloc, false) {
            for (scale, (buffer, old_primitives)) in internal_ref.buffers.iter_mut() {
                let buffer_size = internal_ref
                    .size
                    .to_f64()
                    .to_buffer(**scale, Transform::Normal)
                    .to_i32_round();
                buffer.render().resize(buffer_size);
                *old_primitives = None;
            }
        }

        let output_scale = scale;
        let surface_scale = internal_ref.visibility_frame.scale as f64;
        scale = scale * internal_ref.additional_scale;
        // Preserve subpixel motion and use exactly the same origin for the
        // texture (including its shadow) and the framebuffer blur capture.
        let location = internal_ref.visibility_frame.location(location, scale);
        // The tooltip was emitted above. The outline shares the header fill's
        // own fade/slide; neither is part of the tooltip's independent lifetime.
        if let Some(frame) = focus_frame {
            let theme = &internal_ref.theme;
            let zoom = internal_ref.additional_scale;
            let bounds = frame.bounds;
            let geometry = Rectangle::new(
                (
                    location.x / output_scale.x + bounds.x as f64 * zoom,
                    location.y / output_scale.y + bounds.y as f64 * zoom,
                )
                    .into(),
                (bounds.width as f64 * zoom, bounds.height as f64 * zoom).into(),
            );
            push_above(IcedRenderElement::Outline(OutlineElement(
                IndicatorShader::animated_outline_with_bottom_border(
                    renderer,
                    internal_ref.outline_id.clone(),
                    geometry,
                    theme.halo_style().border_width * zoom as f32,
                    frame.radii.map(|r| r as f32 * zoom as f32),
                    alpha,
                    output_scale.x,
                    theme.focused_window_border(true),
                    theme.window_border_width() * zoom as f32,
                    theme
                        .focused_window_ring(true)
                        .unwrap_or(Color::TRANSPARENT),
                    Some(OutlineFocus {
                        progress: frame.progress,
                        tip: 0.0,
                        halo: true,
                        neutral: theme.window_border_color(),
                    }),
                    frame.bottom_border,
                ),
            )));
        }
        if let Some((buffer, old_layers)) = internal_ref.buffers.get_mut(&OrderedFloat(scale.x)) {
            let size: Size<i32, BufferCoords> = internal_ref
                .size
                .to_f64()
                .to_buffer(scale.x, Transform::Normal)
                .to_i32_round();
            if size.w > 0 && size.h > 0 {
                let mut clip_mask = tiny_skia::Mask::new(size.w as u32, size.h as u32).unwrap();
                let theme = &internal_ref.theme;

                _ = buffer.render().draw(|buf| {
                    let mut pixels =
                        tiny_skia::PixmapMut::from_bytes(buf, size.w as u32, size.h as u32)
                            .expect("Failed to create pixel map");

                    let background_color = internal_ref.program.background_color(theme);
                    let bounds = IcedSize::new(size.w as u32, size.h as u32);
                    let viewport = Viewport::with_physical_size(bounds, scale.x as f32);
                    let scale_x = scale.x as f32;

                    // Get current layers from the renderer (populated by last draw() call)
                    let current_layers = internal_ref.renderer.layers();

                    let mut damage_rects: Vec<_> = old_layers
                        .as_ref()
                        .and_then(|(last_primitives, last_color)| {
                            (last_color == &background_color).then(|| {
                                damage::diff(
                                    last_primitives,
                                    current_layers,
                                    |layer| vec![layer.bounds],
                                    Layer::damage,
                                )
                                .into_iter()
                                .filter(|d| {
                                    let width = d.width as u32;
                                    let height = d.height as u32;
                                    width > 1 && height > 1
                                })
                                .collect()
                            })
                        })
                        .unwrap_or_else(|| {
                            vec![iced_core::Rectangle::with_size(viewport.logical_size())]
                        });

                    damage_rects = damage::group(
                        damage_rects,
                        iced_core::Rectangle::with_size(viewport.logical_size()),
                    );

                    if !damage_rects.is_empty() {
                        *old_layers = Some((current_layers.to_vec(), background_color));

                        // iced 0.15: no overlay parameter
                        internal_ref.renderer.draw(
                            &mut pixels,
                            &mut clip_mask,
                            &viewport,
                            &damage_rects,
                            background_color,
                        );
                    }

                    let damage_output = damage_rects
                        .into_iter()
                        .map(|d| d * scale_x)
                        .filter_map(|x| x.snap())
                        .map(|damage_rect| {
                            Rectangle::new(
                                (damage_rect.x as i32, damage_rect.y as i32).into(),
                                (damage_rect.width as i32, damage_rect.height as i32).into(),
                            )
                        })
                        .collect::<Vec<_>>();

                    internal_ref.program.foreground(
                        &mut pixels,
                        &damage_output,
                        scale.x as f32,
                        theme,
                    );

                    Result::<_, ()>::Ok(damage_output)
                });

                // Trim the cosmic-text shape cache
                {
                    let mut font_system = font_system().write().unwrap();
                    let _ = font_system.raw(); // cache trimming not available in this cosmic-text version
                }
            }

            match MemoryRenderBufferRenderElement::from_buffer(
                renderer,
                location,
                buffer,
                Some(alpha),
                Some(VisibilityFrame::texture_source(location, size)),
                Some(
                    internal_ref
                        .size
                        .to_f64()
                        .upscale(internal_ref.additional_scale)
                        .to_i32_round(),
                ),
                Kind::Unspecified,
            ) {
                Ok(buffer) => {
                    if surface_scale == 1.0 {
                        push_above(buffer.into());
                    } else {
                        // Scale the existing texture on the GPU, never resize
                        // and rerasterize the menu's buffer on every frame.
                        push_above(IcedRenderElement::ScaledUI(
                            RescaleRenderElement::from_element(
                                buffer,
                                location.to_i32_round(),
                                surface_scale,
                            ),
                        ));
                    }
                }
                Err(err) => tracing::warn!("What? {:?}", err),
            }

            // MERGE: upstream gates this on `cosmic::Theme::transparent`; the
            // local Program decides which compositor chrome needs a backdrop.
            let backdrop_blur = {
                let IcedElementInternal {
                    renderer,
                    program,
                    theme,
                    size,
                    ..
                } = internal_ref;
                program.backdrop_blur(theme, *size, renderer.layers(), radii)
            };
            if let Some((bounds, mut blur_radii)) = backdrop_blur {
                for radius in blur_radii.iter_mut() {
                    *radius = ((*radius as f64) * internal_ref.additional_scale).round() as u8;
                }

                let local_bounds = Rectangle::<f64, Logical>::new(
                    (bounds.x as f64, bounds.y as f64).into(),
                    (bounds.width as f64, bounds.height as f64).into(),
                )
                .upscale(internal_ref.additional_scale);
                let element_origin = location
                    .to_logical(scale)
                    .upscale(internal_ref.additional_scale);

                match BlurElement::from_state_with_appearance(
                    renderer,
                    &mut internal_ref.blur,
                    Rectangle::new(
                        element_origin + local_bounds.loc,
                        local_bounds.size.to_i32_round(),
                    ),
                    scale.x,
                    blur_radii,
                    configured_blur_strength(true),
                    alpha,
                    [internal_ref.theme.backdrop_saturate_popover(), 0.0, 0.0],
                ) {
                    Ok(Some(elem)) => {
                        // Use the same GPU transform as the UI. Resizing the
                        // blur itself would reallocate capture textures and
                        // round its corner radii on every animation frame.
                        let elem = if surface_scale == 1.0 {
                            IcedRenderElement::Blur(elem)
                        } else {
                            IcedRenderElement::ScaledBlur(RescaleRenderElement::from_element(
                                elem,
                                location.to_i32_round(),
                                surface_scale,
                            ))
                        };
                        if let Some(push_below) = push_below {
                            push_below(elem)
                        } else {
                            push_above(elem)
                        }
                    }
                    Ok(None) => {}
                    Err(err) => tracing::warn!("Blur elem error: {:?}", err),
                }
            }
        }
        focus_frame
    }
}

render_elements! {
    pub IcedRenderElement<R> where R: ImportMem + AsGlowRenderer, R::TextureId: Send;
    UI=MemoryRenderBufferRenderElement<R>,
    ScaledUI=RescaleRenderElement<MemoryRenderBufferRenderElement<R>>,
    Blur=BlurElement,
    ScaledBlur=RescaleRenderElement<BlurElement>,
    Outline=OutlineElement,
}

#[cfg(test)]
mod tests;
