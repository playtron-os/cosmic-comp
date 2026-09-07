// The airlock wash: the workspace you are entering announces itself in its own
// colour, from the edge you came from.
//
// It carries DIRECTION, which is the whole point — enter from below and the
// colour rises from the bottom edge. That reinforces the model (workspaces are
// the vertical axis) without a word of copy, which is why this is a gradient
// and not a flat tint.
//
// `color` arrives PREMULTIPLIED by `alpha` (see BackdropShader's call site,
// which this follows), so the output stays premultiplied by construction.

precision highp float;
uniform float alpha;
#if defined(DEBUG_FLAGS)
uniform float tint;
#endif
uniform vec2 size;
varying vec2 v_coords;

uniform vec3 color;

// Which edge the colour enters from: 1.0 = top, 0.0 = bottom.
uniform float from_top;

// How far across the surface the colour has faded to nothing, 0..1. The design
// fades out by 55%, so the far half of the screen stays untouched and the
// workspace under it is never obscured.
uniform float falloff;

void main() {
    // Distance from the entering edge, 0 at that edge and 1 at the far one.
    // v_coords.y runs top(0) -> bottom(1).
    float d = mix(1.0 - v_coords.y, v_coords.y, from_top);

    // smoothstep rather than a linear ramp: a hard gradient edge reads as a
    // band sliding across the screen instead of a glow receding from it.
    float t = 1.0 - smoothstep(0.0, falloff, d);

    vec4 mix_color = vec4(color, alpha) * t;

    #if defined(DEBUG_FLAGS)
    if (tint == 1.0)
        mix_color = vec4(0.0, 0.3, 0.0, 0.2) + mix_color * 0.8;
    #endif

    gl_FragColor = mix_color;
}
