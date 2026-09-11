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
uniform float ring_width;
uniform vec4 radius;
uniform float scale;
uniform vec2 draw_size;
uniform vec2 shape_origin;
uniform vec2 shape_size;

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
        inner = coverage(location - vec2(thickness), shape_size - vec2(2.0 * thickness),
            max(radius - vec4(thickness), vec4(0.0)));
    }

    float outer = body;
    if (ring_width > 0.0) {
        outer = coverage(location + vec2(ring_width), shape_size + vec2(2.0 * ring_width),
            radius + vec4(ring_width));
    }
    // These regions are disjoint parts of the same pixel. Add their premultiplied
    // contributions; separate over-blends would darken the shared antialiased edge.
    float border_coverage = max(body - inner, 0.0);
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
