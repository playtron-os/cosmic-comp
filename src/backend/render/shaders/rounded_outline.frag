precision highp float;
uniform float alpha;
#if defined(DEBUG_FLAGS)
uniform float tint;
#endif
varying vec2 v_coords;

uniform vec4 color;
uniform vec4 ring_color;
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
    vec4 mix_color = (color * max(body - inner, 0.0)
        + ring_color * max(outer - body, 0.0)) * alpha;

    #if defined(DEBUG_FLAGS)
    if (tint == 1.0)
        mix_color = vec4(0.0, 0.3, 0.0, 0.2) + mix_color * 0.8;
    #endif

    gl_FragColor = mix_color;
}
