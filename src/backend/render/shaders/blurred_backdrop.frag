// Blurred backdrop shader - frosted glass effect
// Implements: background: rgba(255, 255, 255, 0.10); backdrop-filter: blur(50px);
//
// The texture has already been blurred and cropped to the window region via src_rect.
// This shader:
// 1. Samples the blurred texture
// 2. Applies a semi-transparent white overlay (frosted glass tint)
// 3. Masks with rounded corners (analytic SDF antialiasing)
// 4. Optionally draws a 1px border (for layer shells)

#version 100

// Enable derivatives extension for fwidth() - needed for analytic AA
#extension GL_OES_standard_derivatives : enable

// CRITICAL: Use highp for all SDF math - mediump lacks precision for sub-pixel edges
// On GLES 2.0 (Mali/Adreno/Intel iGPU), mediump is only ~10-16 bits total
// which causes stair-stepping on edges regardless of AA technique
precision highp float;

// Standard texture sampling uniforms (provided by TextureShaderElement)
uniform sampler2D tex;
varying vec2 v_coords;

// Custom uniforms
uniform float alpha;           // Overall opacity
uniform vec2 size;             // Element size in pixels
uniform vec2 screen_size;      // Full screen size for coordinate mapping
uniform vec2 element_pos;      // Element position on screen
uniform float corner_radius_tl;   // Top-left corner radius
uniform float corner_radius_tr;   // Top-right corner radius
uniform float corner_radius_br;   // Bottom-right corner radius
uniform float corner_radius_bl;   // Bottom-left corner radius
uniform vec3 tint_color;       // Tint overlay color (e.g., white = 1.0, 1.0, 1.0)
uniform float tint_strength;   // Tint opacity (0.10 for 10% white overlay)
uniform float border_enabled;  // 1.0 = draw border, 0.0 = no border

// Border configuration (hardcoded for now)
const float BORDER_WIDTH = 1.0;       // 1px border
const vec3 BORDER_COLOR = vec3(1.0, 1.0, 1.0);  // White
const float BORDER_ALPHA = 0.2;       // 20% opacity

// Signed distance field for a rounded box with per-corner radii
// p: position relative to box center
// b: box half-size
// r: corner radii (top_right, bottom_right, bottom_left, top_left)
float rounded_box(vec2 p, vec2 b, vec4 r) {
    r.xy = (p.x > 0.0) ? r.xy : r.zw;
    r.x = (p.y > 0.0) ? r.x : r.y;
    vec2 q = abs(p) - b + r.x;
    return min(max(q.x, q.y), 0.0) + length(max(q, 0.0)) - r.x;
}

void main() {
    // Sample the blurred texture
    vec4 blurred = texture2D(tex, v_coords);
    
    // Convert texture coords to local element pixel coordinates
    vec2 screen_pos = v_coords * screen_size;
    vec2 pixel_coords = screen_pos - element_pos;
    
    // Setup for SDF
    vec2 center = size / 2.0;
    vec2 half_size = size / 2.0;
    vec4 corners = vec4(corner_radius_tr, corner_radius_br, corner_radius_bl, corner_radius_tl);
    
    // Calculate signed distance
    float dist = rounded_box(pixel_coords - center, half_size, corners);
    
    // Analytic antialiasing using screen-space derivatives
    // fwidth(dist) gives the rate of change of distance per pixel
    // This is the canonical SDF AA solution - resolution independent and precise
    float aa = fwidth(dist);
    
    // Smooth shape mask: 1.0 inside, 0.0 outside, smooth gradient at edge
    // smoothstep(0.0, -aa, dist) = 1.0 when dist < -aa, 0.0 when dist > 0
    float mask = smoothstep(0.0, -aa, dist);
    
    // Early discard for pixels completely outside
    if (mask < 0.001) discard;
    
    // Apply tint overlay
    vec3 tinted = mix(blurred.rgb, tint_color, tint_strength);
    
    // Analytic border using two smoothsteps
    // Border is the region between dist=0 (outer edge) and dist=-BORDER_WIDTH (inner edge)
    float border_mask = 0.0;
    if (border_enabled > 0.5) {
        // Outer edge of border (at shape boundary, dist=0)
        float outer = smoothstep(0.0, -aa, dist);
        // Inner edge of border (BORDER_WIDTH pixels inside)
        float inner = smoothstep(-BORDER_WIDTH, -BORDER_WIDTH - aa, dist);
        // Border is the difference - creates a crisp but smooth 1px ring
        border_mask = outer - inner;
    }
    
    // Blend border color over tinted background
    vec3 final_color = mix(tinted, BORDER_COLOR, border_mask * BORDER_ALPHA);
    
    // Final output with premultiplied alpha
    // cosmic-comp uses premultiplied alpha throughout the pipeline
    // Both RGB and A must be multiplied by alpha for correct blending
    float final_alpha = blurred.a * alpha * mask;
    gl_FragColor = vec4(final_color * final_alpha, final_alpha);
}
