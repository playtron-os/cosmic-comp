precision highp float;
uniform float alpha;
#if defined(DEBUG_FLAGS)
uniform float tint;
#endif
uniform vec2 size;
varying vec2 v_coords;

// The destination's colour, fading out from the edge the switch came from:
// CSS `linear-gradient(to top, colour, transparent <reach>)`, premultiplied.
uniform vec3 color;
uniform float from_bottom;
uniform float reach;

void main() {
    float from_edge = from_bottom > 0.5 ? 1.0 - v_coords.y : v_coords.y;
    float a = alpha * clamp(1.0 - from_edge / reach, 0.0, 1.0);
    vec4 mix_color = vec4(color * a, a);

#if defined(DEBUG_FLAGS)
    if (tint == 1.0)
        mix_color = vec4(0.0, 0.3, 0.0, 0.2) + mix_color * 0.8;
#endif

    gl_FragColor = mix_color;
}
