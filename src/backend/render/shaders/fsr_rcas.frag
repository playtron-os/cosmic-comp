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
uniform vec2 inv_size;
uniform float sharpness;
varying vec2 v_coords;

#if defined(DEBUG_FLAGS)
uniform float tint;
#endif

void main() {
    vec3 b = texture2D(tex, v_coords + vec2(0.0, -inv_size.y)).rgb;
    vec3 d = texture2D(tex, v_coords + vec2(-inv_size.x, 0.0)).rgb;
    vec3 e = texture2D(tex, v_coords).rgb;
    vec3 f = texture2D(tex, v_coords + vec2(inv_size.x, 0.0)).rgb;
    vec3 h = texture2D(tex, v_coords + vec2(0.0, inv_size.y)).rgb;

    vec3 mn = min(min(b, d), min(f, h));
    vec3 mx = max(max(b, d), max(f, h));
    // Saturated black/white neighborhoods have zero headroom; avoid 0/0.
    vec3 hit_min = min(mn, e) / max(4.0 * mx, vec3(1e-8));
    vec3 hit_max = (1.0 - max(mx, e)) / min(4.0 * mn - 4.0, vec3(-1e-8));
    vec3 lobes = max(-hit_min, hit_max);
    float lobe = max(-0.1875, min(max(lobes.r, max(lobes.g, lobes.b)), 0.0)) * sharpness;

    vec3 color = (lobe * (b + d + f + h) + e) / (4.0 * lobe + 1.0);
    vec4 result = vec4(color, 1.0) * alpha;
    #if defined(DEBUG_FLAGS)
    if (tint == 1.0)
        result = vec4(0.4, 0.0, 0.0, 0.3) + result * 0.7;
    #endif
    gl_FragColor = result;
}
