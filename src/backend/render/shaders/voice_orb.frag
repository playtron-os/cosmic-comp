// Voice Orb Fragment Shader
// Animated plasma-like orb with noise distortion and halo effects

precision highp float;

// Standard uniforms
uniform float alpha;
uniform vec2 size;
varying vec2 v_coords;

// Animation uniforms
uniform float time;           // Animation time in seconds
uniform float scale;          // Current scale of the orb (0.0 to 1.0 for grow animation)
uniform float pulse;          // Pulse intensity (0.0 to 1.0) for voice activity
uniform float attached;       // 1.0 if attached to window, 0.0 if floating

// Position uniforms (for window attachment)
uniform vec2 target_center;   // Target center position (normalized 0-1)
uniform float morph_progress; // Progress of morphing to fill window (0-1)

// Configuration constants
const float innerRadius = 0.45;    // Aperture
const float noiseScale = 0.65;     // Turbulence
const float speed = 1.2;           // Temporal Speed

// Colors (converted from RGB 0-255 to 0-1)
const vec3 color1 = vec3(0.349, 0.859, 0.702);      // Primary pulse: 89, 219, 179
const vec3 color2 = vec3(0.580, 0.980, 0.780);      // Secondary flow: 148, 250, 199
const vec3 color3 = vec3(1.0, 1.0, 0.941);          // Core depth: 255, 255, 240
const vec3 haloColor = vec3(0.859, 1.0, 1.0);       // Halo ascent: 219, 255, 255
const vec3 bgColor = vec3(1.0, 1.0, 1.0);           // Background A: 255, 255, 255
const vec3 bgColor2 = vec3(0.961, 1.0, 0.980);      // Background B: 245, 255, 250

const float PI = 3.14159265359;

// Hash function for noise
vec3 hash33(vec3 p3) {
    p3 = fract(p3 * vec3(0.1031, 0.11369, 0.13787));
    p3 += dot(p3, p3.yxz + 19.19);
    return -1.0 + 2.0 * fract(vec3(p3.x + p3.y, p3.x + p3.z, p3.y + p3.z) * p3.zyx);
}

// 3D Simplex noise
float snoise3(vec3 p) {
    const float K1 = 0.333333333;
    const float K2 = 0.166666667;
    
    vec3 i = floor(p + (p.x + p.y + p.z) * K1);
    vec3 d0 = p - (i - (i.x + i.y + i.z) * K2);
    
    vec3 e = step(vec3(0.0), d0 - d0.yzx);
    vec3 i1 = e * (1.0 - e.zxy);
    vec3 i2 = 1.0 - e.zxy * (1.0 - e);
    
    vec3 d1 = d0 - (i1 - K2);
    vec3 d2 = d0 - (i2 - K1);
    vec3 d3 = d0 - 0.5;
    
    vec4 h = max(0.6 - vec4(dot(d0, d0), dot(d1, d1), dot(d2, d2), dot(d3, d3)), 0.0);
    vec4 n = h * h * h * h * vec4(dot(d0, hash33(i)), dot(d1, hash33(i + i1)), dot(d2, hash33(i + i2)), dot(d3, hash33(i + 1.0)));
    
    return dot(vec4(31.316), n);
}

// Extract alpha from color (for premultiplied alpha)
vec4 extractAlpha(vec3 colorIn) {
    vec4 colorOut;
    float maxValue = min(max(max(colorIn.r, colorIn.g), colorIn.b), 1.0);
    if (maxValue > 1e-5) {
        colorOut.rgb = colorIn.rgb * (1.0 / maxValue);
        colorOut.a = maxValue;
    } else {
        colorOut = vec4(0.0);
    }
    return colorOut;
}

// Light falloff functions
float light1(float intensity, float attenuation, float dist) {
    return intensity / (1.0 + dist * attenuation);
}

float light2(float intensity, float attenuation, float dist) {
    return intensity / (1.0 + dist * dist * attenuation);
}

// Hue shift for color variation
vec3 hueShift(vec3 color, float amount) {
    const vec3 k = vec3(0.57735, 0.57735, 0.57735);
    float cosAngle = cos(amount);
    return color * cosAngle + cross(k, color) * sin(amount) + k * dot(k, color) * (1.0 - cosAngle);
}

void main() {
    // Exact coordinate transformation from original shader
    vec2 fragCoord = v_coords * size;
    vec2 uv = (fragCoord * 2.0 - size) / size.y;
    
    float animTime = time * speed;
    
    // Smooth the pulse value for cleaner animation (cubic smoothstep)
    float smoothPulse = pulse * pulse * (3.0 - 2.0 * pulse);
    
    // Audio reactivity - subtle expansion based on voice input
    float bassPulse = smoothPulse * 0.8;      // Radius expansion
    float trebleShimmer = smoothPulse * 0.5;  // Brightness/glow boost
    
    float ang = atan(uv.y, uv.x);
    float len = length(uv);
    
    // Dynamic parameters based on audio - subtle radius expansion when speaking
    float dynamicRadius = innerRadius + bassPulse * 0.12;
    float dynamicNoiseScale = noiseScale + trebleShimmer * 0.15;
    
    // Noise-based distortion
    float n0 = snoise3(vec3(uv * dynamicNoiseScale, animTime * 0.5)) * 0.5 + 0.5;
    float r0 = mix(mix(dynamicRadius, 1.0, 0.4), mix(dynamicRadius, 1.0, 0.6), n0);
    float d0 = distance(uv, r0 / len * uv);
    
    // Core glow - brighter when speaking
    float v0 = light1(1.0 + bassPulse * 2.0, 15.0 - bassPulse * 3.0, d0);
    v0 *= smoothstep(r0 * 1.05, r0, len);
    float cl = cos(ang + animTime * 2.0 + bassPulse * 1.5) * 0.5 + 0.5;
    
    // Rotating light point
    float a = animTime * -1.0;
    vec2 pos = vec2(cos(a), sin(a)) * r0;
    float d = distance(uv, pos);
    float v1 = light2(2.0 + bassPulse * 1.5, 5.0, d);
    v1 *= light1(1.0, 50.0 - bassPulse * 10.0, d0);
    
    // Outer halo ring - more visible when speaking
    float haloR = r0 * (1.18 + trebleShimmer * 0.08);
    float dHalo = abs(len - haloR);
    float haloIntensity = 0.15 + trebleShimmer * 0.4;  // Base glow + voice boost
    float v4 = light1(haloIntensity, 60.0, dHalo);
    v4 *= smoothstep(0.12, 0.0, dHalo);
    
    // Edge masks
    float v2 = smoothstep(1.0 + bassPulse * 0.05, mix(dynamicRadius, 1.0, n0 * 0.5), len);
    float v3 = smoothstep(dynamicRadius, mix(dynamicRadius, 1.0, 0.5), len);
    
    // Subtle hue shift based on voice activity
    vec3 baseCol1 = hueShift(color1, bassPulse * 0.3);
    vec3 baseCol2 = hueShift(color2, -bassPulse * 0.15);
    vec3 baseCol3 = hueShift(color3, bassPulse * 0.1);
    vec3 haloColFinal = hueShift(haloColor, trebleShimmer * 0.8);
    
    // Combine colors
    vec3 col = mix(baseCol1, baseCol2, cl);
    col = mix(baseCol3, col, v0);
    col += haloColFinal * v4 * 3.5;
    col *= (1.0 - v4 * 0.2);
    
    // Subtle brightness boost when speaking
    col += (baseCol1 + baseCol2) * bassPulse * 0.08;
    
    col = (col + v1) * v2 * v3;
    col = clamp(col, 0.0, 1.0);
    
    // Extract alpha from the computed color
    vec4 portal = extractAlpha(col);
    
    // Background gradient (only visible inside orb area)
    float bgGrad = clamp(len * 0.5 + uv.y * 0.2, 0.0, 1.0);
    vec3 currentBG = mix(bgColor, bgColor2, bgGrad);
    currentBG += bgColor2 * bassPulse * 0.05;
    
    // Mix portal with background
    vec3 finalRGB = mix(currentBG, portal.rgb, portal.a);
    
    // Create circular mask for the orb
    float orbEdge = r0 * 1.15;  // Slight buffer for halo
    float orbMask = 1.0 - smoothstep(orbEdge - 0.05, orbEdge + 0.05, len);
    
    // Final alpha (scale is handled by geometry size, not shader)
    float finalAlpha = orbMask * alpha;
    
    // Output with premultiplied alpha
    gl_FragColor = vec4(finalRGB * finalAlpha, finalAlpha);
}
