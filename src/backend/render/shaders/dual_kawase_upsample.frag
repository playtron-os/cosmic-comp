// Dual Kawase Upsample Shader
// Based on KWin's implementation and "Bandwidth-Efficient Rendering" (SIGGRAPH 2015)
//
// Combines blur + upsample in a single pass:
// Samples 8 surrounding texels (4 diagonal + 4 cardinal at half-pixel offsets)
// with appropriate weights. Output is rendered at double the input resolution.
//
// The upsample kernel spreads the blurred result back up to full resolution,
// applying additional blur at each level for smooth accumulation.
// Each level doubles: 1/16 → 1/8 → 1/4 → 1/2 → full

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
    vec2 uv = v_coords;

    // 8-tap upsample kernel: 4 diagonal (weight 1) + 4 cardinal (weight 2)
    // Cardinal samples are weighted double because they're closer to center
    vec4 sum = vec4(0.0);

    // Diagonal samples (weight = 1 each, total = 4)
    sum += texture2D(tex, uv + vec2(-half_texel.x * 2.0, 0.0) * offset);
    sum += texture2D(tex, uv + vec2(half_texel.x * 2.0, 0.0) * offset);
    sum += texture2D(tex, uv + vec2(0.0, half_texel.y * 2.0) * offset);
    sum += texture2D(tex, uv + vec2(0.0, -half_texel.y * 2.0) * offset);

    // Cardinal samples (weight = 2 each, total = 8)
    sum += texture2D(tex, uv + vec2(-half_texel.x, half_texel.y) * offset) * 2.0;
    sum += texture2D(tex, uv + vec2(half_texel.x, half_texel.y) * offset) * 2.0;
    sum += texture2D(tex, uv + vec2(-half_texel.x, -half_texel.y) * offset) * 2.0;
    sum += texture2D(tex, uv + vec2(half_texel.x, -half_texel.y) * offset) * 2.0;

    // Total weight: 4×1 + 4×2 = 12
    vec4 result = sum / 12.0;
    result.a *= alpha;

    #if defined(DEBUG_FLAGS)
    if (tint == 1.0)
        result = vec4(0.0, 0.0, 0.4, 0.3) + result * 0.7;
    #endif

    gl_FragColor = result;
}
