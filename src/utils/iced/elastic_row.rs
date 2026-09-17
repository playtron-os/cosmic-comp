//! A row that shares a width shortfall between its flexible children.
//!
//! Iced's flex pass hands every `Shrink` child the whole remaining width, in
//! order, so the first one that wants more than fits swallows everything and
//! leaves its siblings nothing at all. In the Halo pill that turns a long
//! window title into a header with no app name — and, before the controls were
//! pinned to a fixed width, no buttons either.
//!
//! This row measures what every child would like, and only when they do not
//! all fit does it hand out the width by max-min fairness: the smallest
//! appetite is served first, so a short label keeps all of its text and the
//! long one absorbs the shortfall. Children marked rigid (an icon, a control)
//! are never asked to give anything up.

use iced_core::{
    Element, Event, Layout, Length, Point, Rectangle, Shell, Size, Vector, layout, mouse, overlay,
    renderer,
    widget::{Operation, Tree, Widget},
};

/// See the module documentation.
pub struct ElasticRow<'a, Message, Theme, Renderer> {
    children: Vec<Element<'a, Message, Theme, Renderer>>,
    /// Parallel to `children`: whether the child may be squeezed.
    elastic: Vec<bool>,
    spacing: f32,
}

impl<Message, Theme, Renderer> Default for ElasticRow<'_, Message, Theme, Renderer> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a, Message, Theme, Renderer> ElasticRow<'a, Message, Theme, Renderer> {
    pub fn new() -> Self {
        Self {
            children: Vec::new(),
            elastic: Vec::new(),
            spacing: 0.0,
        }
    }

    /// Add a child that always gets the width it asks for.
    #[must_use]
    pub fn push_rigid(mut self, child: impl Into<Element<'a, Message, Theme, Renderer>>) -> Self {
        self.children.push(child.into());
        self.elastic.push(false);
        self
    }

    /// Add a child that gives way when the row runs out of width.
    #[must_use]
    pub fn push_elastic(mut self, child: impl Into<Element<'a, Message, Theme, Renderer>>) -> Self {
        self.children.push(child.into());
        self.elastic.push(true);
        self
    }

    #[must_use]
    pub fn spacing(mut self, spacing: f32) -> Self {
        self.spacing = spacing;
        self
    }

    /// Unwrap a row of exactly one child: with nothing to share it against,
    /// the row would only add a layer that lays out identically. Any other
    /// row is handed back unchanged.
    pub fn into_single(mut self) -> Result<Element<'a, Message, Theme, Renderer>, Self> {
        if self.children.len() == 1 {
            Ok(self.children.remove(0))
        } else {
            Err(self)
        }
    }
}

/// How wide each child may be, given `budget` to share between them.
///
/// Returns `None` when everything fits, so the caller can keep the layout it
/// already has instead of measuring a second time.
fn shares(natural: &[f32], elastic: &[bool], budget: f32) -> Option<Vec<f32>> {
    if natural.iter().sum::<f32>() <= budget {
        return None;
    }
    let rigid: f32 = natural
        .iter()
        .zip(elastic)
        .filter_map(|(width, elastic)| (!elastic).then_some(*width))
        .sum();
    let mut left = (budget - rigid).max(0.0);
    let mut order: Vec<usize> = (0..natural.len()).filter(|index| elastic[*index]).collect();
    order.sort_by(|a, b| natural[*a].total_cmp(&natural[*b]));

    let mut shares = natural.to_vec();
    let mut remaining = order.len();
    for index in order {
        // `remaining` is never zero: the loop runs exactly `order.len()` times.
        let even = left / remaining as f32;
        let share = natural[index].min(even);
        shares[index] = share;
        left -= share;
        remaining -= 1;
    }
    Some(shares)
}

impl<Message, Theme, Renderer> Widget<Message, Theme, Renderer>
    for ElasticRow<'_, Message, Theme, Renderer>
where
    Renderer: iced_core::Renderer,
{
    fn children(&self) -> Vec<Tree> {
        self.children.iter().map(Tree::new).collect()
    }

    fn diff(&self, tree: &mut Tree) {
        tree.diff_children(&self.children);
    }

    fn size(&self) -> Size<Length> {
        Size {
            width: Length::Shrink,
            height: Length::Shrink,
        }
    }

    fn layout(
        &mut self,
        tree: &mut Tree,
        renderer: &Renderer,
        limits: &layout::Limits,
    ) -> layout::Node {
        let limits = limits.width(Length::Shrink).height(Length::Shrink);
        let max = limits.max();
        let count = self.children.len();
        if count == 0 {
            return layout::Node::new(limits.resolve(Length::Shrink, Length::Shrink, Size::ZERO));
        }

        let total_spacing = self.spacing * (count - 1) as f32;
        let budget = (max.width - total_spacing).max(0.0);

        // What every child would take if it were alone with the whole budget.
        let loose = layout::Limits::new(Size::ZERO, Size::new(budget, max.height));
        let mut nodes: Vec<layout::Node> = (0..count)
            .map(|index| {
                self.children[index].as_widget_mut().layout(
                    &mut tree.children[index],
                    renderer,
                    &loose,
                )
            })
            .collect();
        let natural: Vec<f32> = nodes.iter().map(|node| node.size().width).collect();

        if let Some(shares) = shares(&natural, &self.elastic, budget) {
            for index in 0..count {
                if shares[index] >= natural[index] {
                    continue;
                }
                let limits =
                    layout::Limits::new(Size::ZERO, Size::new(shares[index].max(0.0), max.height));
                nodes[index] = self.children[index].as_widget_mut().layout(
                    &mut tree.children[index],
                    renderer,
                    &limits,
                );
            }
        }

        let height = nodes
            .iter()
            .map(|node| node.size().height)
            .fold(0.0_f32, f32::max);
        let mut x = 0.0_f32;
        for node in &mut nodes {
            let size = node.size();
            node.move_to_mut(Point::new(x, (height - size.height) / 2.0));
            x += size.width + self.spacing;
        }
        let width = (x - self.spacing).max(0.0);

        layout::Node::with_children(
            limits.resolve(Length::Shrink, Length::Shrink, Size::new(width, height)),
            nodes,
        )
    }

    fn operate(
        &mut self,
        tree: &mut Tree,
        layout: Layout<'_>,
        renderer: &Renderer,
        operation: &mut dyn Operation,
    ) {
        operation.container(None, layout.bounds());
        operation.traverse(&mut |operation| {
            self.children
                .iter_mut()
                .zip(&mut tree.children)
                .zip(layout.children())
                .for_each(|((child, state), layout)| {
                    child
                        .as_widget_mut()
                        .operate(state, layout, renderer, operation);
                });
        });
    }

    fn update(
        &mut self,
        tree: &mut Tree,
        event: &Event,
        layout: Layout<'_>,
        cursor: mouse::Cursor,
        renderer: &Renderer,
        shell: &mut Shell<'_, Message>,
        viewport: &Rectangle,
    ) {
        for ((child, tree), layout) in self
            .children
            .iter_mut()
            .zip(&mut tree.children)
            .zip(layout.children())
        {
            child
                .as_widget_mut()
                .update(tree, event, layout, cursor, renderer, shell, viewport);
        }
    }

    fn mouse_interaction(
        &self,
        tree: &Tree,
        layout: Layout<'_>,
        cursor: mouse::Cursor,
        viewport: &Rectangle,
        renderer: &Renderer,
    ) -> mouse::Interaction {
        self.children
            .iter()
            .zip(&tree.children)
            .zip(layout.children())
            .map(|((child, tree), layout)| {
                child
                    .as_widget()
                    .mouse_interaction(tree, layout, cursor, viewport, renderer)
            })
            .max()
            .unwrap_or_default()
    }

    fn draw(
        &self,
        tree: &Tree,
        renderer: &mut Renderer,
        theme: &Theme,
        style: &renderer::Style,
        layout: Layout<'_>,
        cursor: mouse::Cursor,
        viewport: &Rectangle,
    ) {
        for ((child, tree), layout) in self
            .children
            .iter()
            .zip(&tree.children)
            .zip(layout.children())
            .filter(|(_, layout)| layout.bounds().intersects(viewport))
        {
            child
                .as_widget()
                .draw(tree, renderer, theme, style, layout, cursor, viewport);
        }
    }

    fn overlay<'b>(
        &'b mut self,
        tree: &'b mut Tree,
        layout: Layout<'b>,
        renderer: &Renderer,
        viewport: &Rectangle,
        translation: Vector,
    ) -> Option<overlay::Element<'b, Message, Theme, Renderer>> {
        overlay::from_children(
            &mut self.children,
            tree,
            layout,
            renderer,
            viewport,
            translation,
        )
    }
}

impl<'a, Message, Theme, Renderer> From<ElasticRow<'a, Message, Theme, Renderer>>
    for Element<'a, Message, Theme, Renderer>
where
    Message: 'a,
    Theme: 'a,
    Renderer: iced_core::Renderer + 'a,
{
    fn from(row: ElasticRow<'a, Message, Theme, Renderer>) -> Self {
        Self::new(row)
    }
}

#[cfg(test)]
mod tests {
    use super::shares;

    #[test]
    fn everything_that_fits_is_left_alone() {
        assert!(shares(&[10.0, 20.0], &[true, true], 100.0).is_none());
        assert!(shares(&[50.0, 50.0], &[true, true], 100.0).is_none());
    }

    #[test]
    fn a_short_label_keeps_its_text_while_the_long_one_gives_way() {
        // A 500px title beside a 20px app name in 200px: the app name is cheap
        // enough to serve in full, so only the title is cut.
        let shares = shares(&[500.0, 20.0], &[true, true], 200.0).unwrap();
        assert_eq!(shares, vec![180.0, 20.0]);
    }

    #[test]
    fn two_greedy_children_split_the_width_evenly() {
        let shares = shares(&[500.0, 400.0], &[true, true], 200.0).unwrap();
        assert_eq!(shares, vec![100.0, 100.0]);
    }

    #[test]
    fn a_rigid_child_is_never_squeezed() {
        // The icon is rigid: it keeps 20px and the texts share the other 80.
        let shares = shares(&[20.0, 500.0, 400.0], &[false, true, true], 100.0).unwrap();
        assert_eq!(shares, vec![20.0, 40.0, 40.0]);
    }

    #[test]
    fn a_budget_swallowed_by_rigid_children_never_goes_negative() {
        let shares = shares(&[300.0, 50.0], &[false, true], 100.0).unwrap();
        assert_eq!(shares, vec![300.0, 0.0]);
        assert!(shares.iter().all(|share| *share >= 0.0));
    }

    #[test]
    fn no_elastic_child_leaves_every_width_alone() {
        let shares = shares(&[300.0, 50.0], &[false, false], 100.0).unwrap();
        assert_eq!(shares, vec![300.0, 50.0]);
    }
}
