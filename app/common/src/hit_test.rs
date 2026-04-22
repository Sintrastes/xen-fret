//! Click/tap hit-testing for rendered fretboard diagrams.
//!
//! Given a [`DiagramLayout`] (returned by `fretboard_diagrams::render_board_with_layout`)
//! and the rendered element's pixel dimensions, map a click in element-local pixels
//! to a specific note on the fretboard. Applies the standard SVG
//! `preserveAspectRatio="xMidYMid meet"` transform (SVG default) — the same
//! letterbox scaling the browser applies to inline SVGs and that GTK's
//! `ContentFit::Contain` and Compose's `ContentScale.Fit` apply to rendered
//! bitmaps.

use fretboard_diagrams::DiagramLayout;

/// A note the user hit with a click or tap.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HitTarget {
    pub string_idx: usize,
    pub fret: i32,
    pub scale_degree: usize,
    pub absolute_step: i32,
}

/// Hit-test a click at `(click_x, click_y)` within an element of size
/// `(elem_w, elem_h)`. Coordinates are in the element's local pixel space
/// (top-left origin). Returns the closest note dot that contains the click,
/// or `None` if the click is outside every dot.
pub fn hit_test(
    layout: &DiagramLayout,
    elem_w: f64,
    elem_h: f64,
    click_x: f64,
    click_y: f64,
) -> Option<HitTarget> {
    if elem_w <= 0.0 || elem_h <= 0.0 {
        return None;
    }
    let (vx, vy, vw, vh) = layout.view_box;
    if vw <= 0.0 || vh <= 0.0 {
        return None;
    }
    // "meet" = uniform scale, letterbox to fit.
    let scale = (elem_w / vw).min(elem_h / vh);
    let ox = (elem_w - vw * scale) / 2.0 - vx * scale;
    let oy = (elem_h - vh * scale) / 2.0 - vy * scale;
    let bx = (click_x - ox) / scale;
    let by = (click_y - oy) / scale;

    layout
        .notes
        .iter()
        .filter(|n| {
            let dx = bx - n.cx;
            let dy = by - n.cy;
            dx * dx + dy * dy <= n.radius * n.radius
        })
        .min_by(|a, b| {
            let da = (bx - a.cx).hypot(by - a.cy);
            let db = (bx - b.cx).hypot(by - b.cy);
            da.partial_cmp(&db).unwrap_or(std::cmp::Ordering::Equal)
        })
        .map(|n| HitTarget {
            string_idx: n.string_idx,
            fret: n.fret,
            scale_degree: n.scale_degree,
            absolute_step: n.absolute_step,
        })
}

#[cfg(test)]
mod tests {
    use super::*;
    use fretboard_diagrams::NotePosition;

    fn layout() -> DiagramLayout {
        DiagramLayout {
            view_box: (0.0, 0.0, 10.0, 5.0),
            notes: vec![
                NotePosition {
                    string_idx: 0,
                    fret: 0,
                    scale_degree: 0,
                    absolute_step: 0,
                    cx: 1.0,
                    cy: 1.0,
                    radius: 0.5,
                },
                NotePosition {
                    string_idx: 1,
                    fret: 3,
                    scale_degree: 2,
                    absolute_step: 4,
                    cx: 5.0,
                    cy: 3.0,
                    radius: 0.5,
                },
            ],
        }
    }

    #[test]
    fn unscaled_hit_matches() {
        // Element matches viewBox exactly (10x5 px). Click at (5,3) hits note 1.
        let hit = hit_test(&layout(), 10.0, 5.0, 5.0, 3.0).unwrap();
        assert_eq!(hit.string_idx, 1);
        assert_eq!(hit.fret, 3);
    }

    #[test]
    fn scaled_hit_matches() {
        // 2x uniform scale: 20x10 px element, viewBox 10x5 → click at (10, 6) = (5,3) in vb.
        let hit = hit_test(&layout(), 20.0, 10.0, 10.0, 6.0).unwrap();
        assert_eq!(hit.string_idx, 1);
    }

    #[test]
    fn letterboxed_hit_matches() {
        // 40x10 element, viewBox 10x5 → scale = min(4, 2) = 2, x-offset = (40-20)/2 = 10.
        // Click at (10 + 10, 6) → viewBox (5, 3) → note 1.
        let hit = hit_test(&layout(), 40.0, 10.0, 20.0, 6.0).unwrap();
        assert_eq!(hit.string_idx, 1);
    }

    #[test]
    fn miss_returns_none() {
        assert!(hit_test(&layout(), 10.0, 5.0, 9.0, 4.5).is_none());
    }
}
