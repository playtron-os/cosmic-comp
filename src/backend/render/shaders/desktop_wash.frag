#version 100

//_DEFINES_

#if defined(EXTERNAL)
#extension GL_OES_EGL_image_external : require
#endif

precision highp float;
#if defined(EXTERNAL)
uniform samplerExternalOES tex;
#else
uniform sampler2D tex;
#endif

uniform float alpha;
varying vec2 v_coords;

#if defined(DEBUG_FLAGS)
uniform float tint;
#endif

// The workspace's colour, and how much of it is blended over the wallpaper with
// CSS `mix-blend-mode: soft-light`, which reads the wallpaper under every pixel.
uniform vec3 wash_color;
uniform float wash_amount;

vec3 soft_light(vec3 b, vec3 s) {
    vec3 d = mix(sqrt(b), ((16.0 * b - 12.0) * b + 4.0) * b, step(b, vec3(0.25)));
    vec3 darken = b - (1.0 - 2.0 * s) * b * (1.0 - b);
    vec3 lighten = b + (2.0 * s - 1.0) * (d - b);
    return mix(lighten, darken, step(s, vec3(0.5)));
}

void main() {
    vec4 color = texture2D(tex, v_coords);
#if defined(NO_ALPHA)
    color = vec4(color.rgb, 1.0);
#endif
    if (color.a > 0.0) {
        vec3 base = color.rgb / color.a;
        color.rgb = mix(base, soft_light(base, wash_color), wash_amount) * color.a;
    }
    color = color * alpha;

#if defined(DEBUG_FLAGS)
    if (tint == 1.0)
        color = vec4(0.0, 0.2, 0.0, 0.2) + color * 0.8;
#endif

    gl_FragColor = color;
}
