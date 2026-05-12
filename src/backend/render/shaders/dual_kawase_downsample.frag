// Dual Kawase Downsample Shader
// Based on KWin's implementation and "Bandwidth-Efficient Rendering" (SIGGRAPH 2015)
//
// Combines blur + downsample in a single pass:
// Samples the center texel plus 4 diagonal offsets at half-pixel positions.
// Output is rendered at half the input resolution, so the GPU's bilinear
// filtering effectively gives us a 2x2 area sample per texel fetch.
//
// Each downsample level halves the texture: full → 1/2 → 1/4 → 1/8 → 1/16
// With 4 levels, we go from e.g. 1440×900 → 720×450 → 360×225 → 180×112 → 90×56

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

// Uniforms
uniform vec2 half_texel;  // 0.5 / texture_size (half-pixel offset in UV space)
uniform float offset;     // Blur spread multiplier (1.0 = standard, higher = stronger)

#if defined(DEBUG_FLAGS)
uniform float tint;
#endif

void main() {
    // Sample center + 4 diagonal offsets (the Dual Kawase downsample kernel)
    // The half_texel offset means we sample between texels, leveraging
    // the GPU's bilinear filtering for free 2x2 averaging
    vec2 uv = v_coords;

    vec4 sum = texture2D(tex, uv) * 4.0;
    sum += texture2D(tex, uv - half_texel * offset);
    sum += texture2D(tex, uv + half_texel * offset);
    sum += texture2D(tex, uv + vec2(half_texel.x, -half_texel.y) * offset);
    sum += texture2D(tex, uv + vec2(-half_texel.x, half_texel.y) * offset);

    vec4 result = sum / 8.0;
    result.a *= alpha;

    #if defined(DEBUG_FLAGS)
    if (tint == 1.0)
        result = vec4(0.0, 0.2, 0.0, 0.3) + result * 0.7;
    #endif

    gl_FragColor = result;
}
