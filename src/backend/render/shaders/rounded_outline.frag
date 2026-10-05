precision highp float;
uniform float alpha;
#if defined(DEBUG_FLAGS)
uniform float tint;
#endif
varying vec2 v_coords;

uniform vec4 color;
uniform vec4 ring_color;
uniform vec4 neutral_color;
uniform float focus_mode;
uniform float focus_progress;
uniform float focus_tip;
uniform float thickness;
uniform float bottom_border;
uniform float ring_width;
uniform vec4 radius;
uniform float scale;
uniform vec2 draw_size;
uniform vec2 shape_origin;
uniform vec2 shape_size;
uniform float dash;

float coverage(vec2 p, vec2 extent, vec4 corners) {
    if (extent.x <= 0.0 || extent.y <= 0.0) return 0.0;
    vec2 half_size = extent * 0.5;
    vec2 centered = p - half_size;
    float r = centered.x < 0.0
        ? (centered.y < 0.0 ? corners.x : corners.w)
        : (centered.y < 0.0 ? corners.y : corners.z);
    r = clamp(r, 0.0, min(half_size.x, half_size.y));
    vec2 q = abs(centered) - half_size + vec2(r);
    float distance = length(max(q, vec2(0.0))) + min(max(q.x, q.y), 0.0) - r;
    // Linear pixel coverage preserves stroke weight as an edge crosses pixels.
    return clamp(0.5 - distance * scale, 0.0, 1.0);
}

// Arc length along a rounded rect's edge, clockwise from the top edge's left end.
float perimeter_at(vec2 p, vec2 size, float r) {
    float top = max(size.x - 2.0 * r, 0.0);
    float side = max(size.y - 2.0 * r, 0.0);
    float q = 1.57079632679 * r;
    vec2 far = size - vec2(r);
    if (p.x >= far.x && p.y <= r) return top + r * atan(p.x - far.x, r - p.y);
    if (p.x >= far.x && p.y >= far.y) return top + q + side + r * atan(p.y - far.y, p.x - far.x);
    if (p.x <= r && p.y >= far.y) return 2.0 * top + 2.0 * q + side + r * atan(r - p.x, p.y - far.y);
    if (p.x <= r && p.y <= r) return 2.0 * top + 3.0 * q + 2.0 * side + r * atan(r - p.y, r - p.x);
    float edge = min(min(p.y, size.x - p.x), min(size.y - p.y, p.x));
    if (edge == p.y) return p.x - r;
    if (edge == size.x - p.x) return top + q + p.y - r;
    if (edge == size.y - p.y) return top + 2.0 * q + side + far.x - p.x;
    return 2.0 * top + 3.0 * q + side + far.y - p.y;
}

// CSS `dashed`: dashes and gaps of `dash`, stretched so a whole number fit.
float dash_mask(vec2 location) {
    float inset = thickness * 0.5;
    vec2 size = shape_size - vec2(thickness);
    float r = clamp(radius.x - inset, 0.0, min(size.x, size.y) * 0.5);
    float length = 2.0 * (size.x + size.y) - (8.0 - 6.28318530718) * r;
    float period = length / max(floor(length / (2.0 * dash) + 0.5), 1.0);
    float m = mod(perimeter_at(location - vec2(inset), size, r), period);
    return clamp(min(m, period * 0.5 - m) * scale + 0.5, 0.0, 1.0);
}

// Normalized SVG stroke reveal from WindowFrame: mirrored paths run from
// the Halo tips around the window and meet at bottom center. The short top
// bridges independently meet under the Halo. Insets follow each stroke's
// centerline, so the border and outside ring share the same endpoint clock.
float focus_mask(vec2 p, float inset) {
    if (focus_mode == 0.0 || focus_progress >= 1.0) return 1.0;
    if (focus_progress <= 0.0) return 0.0;
    if (focus_mode == 2.0) {
        float from_tip = min(p.x, shape_size.x - p.x);
        return clamp((focus_progress * shape_size.x * 0.5 - from_tip) * scale + 0.5, 0.0, 1.0);
    }

    vec2 extent = shape_size - vec2(2.0 * inset);
    p -= vec2(inset);
    bool left = p.x <= extent.x * 0.5;
    float rt = left ? radius.x : radius.y;
    float rb = left ? radius.w : radius.z;
    rt = clamp(rt - inset, 0.0, min(extent.x, extent.y) * 0.5);
    rb = clamp(rb - inset, 0.0, min(extent.x, extent.y) * 0.5);
    if (!left) p.x = extent.x - p.x;
    float mid = extent.x * 0.5;
    float tip = clamp(focus_tip - inset, rt, mid);
    float quarter = 1.57079632679;
    float top_length = tip - rt;
    float side_length = max(extent.y - rt - rb, 0.0);
    float path_length = top_length + quarter * (rt + rb) + side_length + mid - rb;
    float distance;
    if (p.x < rt && p.y < rt) {
        distance = top_length + rt * atan(max(rt - p.x, 0.0), max(rt - p.y, 0.0));
    } else if (p.x < rb && p.y > extent.y - rb) {
        distance = top_length + quarter * rt + side_length
            + rb * atan(max(p.y - (extent.y - rb), 0.0), max(rb - p.x, 0.0));
    } else if (p.y <= p.x && p.y <= extent.y - p.y) {
        if (p.x > tip) {
            distance = p.x - tip;
            path_length = mid - tip;
        } else {
            distance = tip - p.x;
        }
    } else if (extent.y - p.y <= p.x) {
        distance = top_length + quarter * (rt + rb) + side_length + p.x - rb;
    } else {
        distance = top_length + quarter * rt + p.y - rt;
    }
    return clamp((focus_progress * path_length - distance) * scale + 0.5, 0.0, 1.0);
}

void main() {
    vec2 location = v_coords * draw_size - shape_origin;
    float body = coverage(location, shape_size, radius);
    float inner = 0.0;

    if (thickness > 0.0) {
        vec2 inner_size = shape_size - vec2(2.0 * thickness);
        if (bottom_border == 0.0) {
            // Extend the hole through the join, retaining both vertical sides.
            inner_size.y += thickness + ring_width + 1.0 / scale;
        }
        inner = coverage(location - vec2(thickness), inner_size,
            max(radius - vec4(thickness), vec4(0.0)));
    }

    float outer = body;
    if (ring_width > 0.0) {
        vec2 outer_size = shape_size + vec2(2.0 * ring_width);
        vec4 outer_radius = radius + vec4(ring_width);
        if (bottom_border == 0.0) {
            outer_radius.zw = vec2(0.0);
        }
        outer = coverage(location + vec2(ring_width), outer_size, outer_radius);
        if (bottom_border == 0.0) {
            // Clip at the join without shortening the backing rectangle: that
            // would clamp the top arc radius and change its stroke weight.
            outer = min(outer, clamp(0.5 + (shape_size.y - location.y) * scale, 0.0, 1.0));
        }
    }
    // These regions are disjoint parts of the same pixel. Add their premultiplied
    // contributions; separate over-blends would darken the shared antialiased edge.
    float border_coverage = max(body - inner, 0.0);
    if (dash > 0.0 && border_coverage > 0.0) border_coverage *= dash_mask(location);
    float ring_coverage = max(outer - body, 0.0);
    // Skip path math for the transparent interior and for settled outlines.
    float border_reveal = border_coverage > 0.0 ? focus_mask(location, thickness * 0.5) : 0.0;
    float ring_reveal = ring_coverage > 0.0 ? focus_mask(location, -ring_width * 0.5) : 0.0;
    vec4 mix_color = (mix(neutral_color, color, border_reveal) * border_coverage
        + ring_color * ring_reveal * ring_coverage) * alpha;

    #if defined(DEBUG_FLAGS)
    if (tint == 1.0)
        mix_color = vec4(0.0, 0.3, 0.0, 0.2) + mix_color * 0.8;
    #endif

    gl_FragColor = mix_color;
}
