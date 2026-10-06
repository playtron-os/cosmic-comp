// SPDX-License-Identifier: MIT
// Copyright (c) 2021 Advanced Micro Devices, Inc. All rights reserved.
// See LICENSE-FSR for the permission notice.

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
uniform vec2 src_size;
uniform vec2 dst_size;
varying vec2 v_coords;

#if defined(DEBUG_FLAGS)
uniform float tint;
#endif

float luma(vec3 c) {
    return c.g + (c.r + c.b) * 0.5;
}

void edge(inout vec2 dir, inout float len, float weight,
          float a, float b, float c, float d, float e) {
    vec2 gradient = vec2(d - b, e - a);
    vec2 contrast = vec2(max(abs(d - c), abs(c - b)),
                         max(abs(e - c), abs(c - a)));
    vec2 strength = clamp(abs(gradient) / max(contrast, vec2(1e-8)), 0.0, 1.0);
    dir += gradient * weight;
    len += dot(strength, strength) * weight;
}

void tap(inout vec3 color, inout float weight_sum, vec2 offset,
         vec2 dir, vec2 len, float lobe, float clip, vec3 sample_color) {
    vec2 v = vec2(dot(offset, dir), dot(offset, vec2(-dir.y, dir.x))) * len;
    float d2 = min(dot(v, v), clip);
    float base = 0.4 * d2 - 1.0;
    float window = lobe * d2 - 1.0;
    float weight = (1.5625 * base * base - 0.5625) * window * window;
    color += sample_color * weight;
    weight_sum += weight;
}

void main() {
    vec2 pos = v_coords * src_size - 0.5;
    vec2 base = floor(pos) + 0.5;
    vec2 pp = fract(pos);
    vec2 inv_src = 1.0 / src_size;

    // Explicit texel-center samples keep the 12-tap kernel available on GLES 2.
    vec3 b = texture2D(tex, (base + vec2( 0.0, -1.0)) * inv_src).rgb;
    vec3 c = texture2D(tex, (base + vec2( 1.0, -1.0)) * inv_src).rgb;
    vec3 e = texture2D(tex, (base + vec2(-1.0,  0.0)) * inv_src).rgb;
    vec3 f = texture2D(tex, (base + vec2( 0.0,  0.0)) * inv_src).rgb;
    vec3 g = texture2D(tex, (base + vec2( 1.0,  0.0)) * inv_src).rgb;
    vec3 h = texture2D(tex, (base + vec2( 2.0,  0.0)) * inv_src).rgb;
    vec3 i = texture2D(tex, (base + vec2(-1.0,  1.0)) * inv_src).rgb;
    vec3 j = texture2D(tex, (base + vec2( 0.0,  1.0)) * inv_src).rgb;
    vec3 k = texture2D(tex, (base + vec2( 1.0,  1.0)) * inv_src).rgb;
    vec3 l = texture2D(tex, (base + vec2( 2.0,  1.0)) * inv_src).rgb;
    vec3 n = texture2D(tex, (base + vec2( 0.0,  2.0)) * inv_src).rgb;
    vec3 o = texture2D(tex, (base + vec2( 1.0,  2.0)) * inv_src).rgb;

    vec2 dir = vec2(0.0);
    float len = 0.0;
    edge(dir, len, (1.0 - pp.x) * (1.0 - pp.y), luma(b), luma(e), luma(f), luma(g), luma(j));
    edge(dir, len, pp.x * (1.0 - pp.y), luma(c), luma(f), luma(g), luma(h), luma(k));
    edge(dir, len, (1.0 - pp.x) * pp.y, luma(f), luma(i), luma(j), luma(k), luma(n));
    edge(dir, len, pp.x * pp.y, luma(g), luma(j), luma(k), luma(l), luma(o));

    float dir2 = dot(dir, dir);
    dir = dir2 < 1.0 / 32768.0 ? vec2(1.0, 0.0) : dir * inversesqrt(dir2);
    len = 0.25 * len * len;
    float stretch = dot(dir, dir) / max(abs(dir.x), abs(dir.y));
    vec2 lengths = vec2(1.0 + (stretch - 1.0) * len, 1.0 - 0.5 * len);
    float lobe = 0.5 - 0.29 * len;
    float clip = 1.0 / lobe;

    vec3 color = vec3(0.0);
    float weight_sum = 0.0;
    tap(color, weight_sum, vec2( 0.0, -1.0) - pp, dir, lengths, lobe, clip, b);
    tap(color, weight_sum, vec2( 1.0, -1.0) - pp, dir, lengths, lobe, clip, c);
    tap(color, weight_sum, vec2(-1.0,  1.0) - pp, dir, lengths, lobe, clip, i);
    tap(color, weight_sum, vec2( 0.0,  1.0) - pp, dir, lengths, lobe, clip, j);
    tap(color, weight_sum, vec2( 0.0,  0.0) - pp, dir, lengths, lobe, clip, f);
    tap(color, weight_sum, vec2(-1.0,  0.0) - pp, dir, lengths, lobe, clip, e);
    tap(color, weight_sum, vec2( 1.0,  1.0) - pp, dir, lengths, lobe, clip, k);
    tap(color, weight_sum, vec2( 2.0,  1.0) - pp, dir, lengths, lobe, clip, l);
    tap(color, weight_sum, vec2( 2.0,  0.0) - pp, dir, lengths, lobe, clip, h);
    tap(color, weight_sum, vec2( 1.0,  0.0) - pp, dir, lengths, lobe, clip, g);
    tap(color, weight_sum, vec2( 1.0,  2.0) - pp, dir, lengths, lobe, clip, o);
    tap(color, weight_sum, vec2( 0.0,  2.0) - pp, dir, lengths, lobe, clip, n);

    color = clamp(color / weight_sum, min(min(f, g), min(j, k)), max(max(f, g), max(j, k)));
    vec4 result = vec4(color, 1.0) * alpha;
    #if defined(DEBUG_FLAGS)
    if (tint == 1.0)
        result = vec4(0.4, 0.0, 0.0, 0.3) + result * 0.7;
    #endif
    gl_FragColor = result;
}
