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

uniform float progress;
uniform float aspect;
uniform float direction;
uniform float seed;

const float PI = 3.14159265359;
const float TAU = 6.28318530718;

float hash(float value) {
    return fract(sin(value * 127.1 + seed * 311.7) * 43758.5453);
}

vec2 rotate2(vec2 value, float angle) {
    float c = cos(angle);
    float s = sin(angle);
    return mat2(c, -s, s, c) * value;
}

// Concentric, staggered polar cells make irregular glass-sized polygons. Their
// boundaries are the cracks, and every cell gets a stable transform from its id.
float ring_coordinate(vec2 point) {
    float radius = length(point);
    float angle = atan(point.y, point.x);
    float sector = (angle + PI) / TAU * 16.0;
    float local_angle = (fract(sector) - 0.5) * TAU / 16.0;
    float irregularity = (hash(floor(sector) + 83.0) - 0.5) * 0.32;
    return radius * cos(local_angle) * 8.5 + irregularity;
}

float sector_coordinate(vec2 point, float ring) {
    float angle = atan(point.y, point.x);
    return (angle + PI) / TAU * 16.0
        + sin(angle * 3.0 + seed * 4.0) * 0.30
        + sin(angle * 7.0 - seed * 5.0) * 0.08
        + mod(ring, 2.0) * 0.5;
}

vec3 shard_at(vec2 point) {
    float ring_coord = ring_coordinate(point);
    float ring = floor(ring_coord);
    float sector = floor(sector_coordinate(point, ring));
    return vec3(ring, sector, ring * 37.0 + sector * 11.0);
}

vec2 shard_center(vec3 shard) {
    float radius = (shard.x + 0.5) / 8.5;
    float angle = ((shard.y + 0.5 - mod(shard.x, 2.0) * 0.5) / 16.0) * TAU - PI;
    angle += (hash(shard.z + 3.0) - 0.5) * 0.09;
    return vec2(cos(angle), sin(angle)) * radius;
}

vec2 shard_translation(vec3 shard, vec2 center, float fly) {
    float random = hash(shard.z + 17.0);
    vec2 radial = normalize(center + vec2(0.0001));
    vec2 tangent = vec2(-radial.y, radial.x);
    vec2 motion = radial * (0.08 + random * 0.32);
    motion += tangent * (hash(shard.z + 29.0) - 0.5) * 0.12;
    motion.y += fly * (0.04 + hash(shard.z + 41.0) * 0.08);
    return motion * fly;
}

float shard_rotation(vec3 shard, float fly) {
    return (hash(shard.z + 53.0) - 0.5) * 1.3 * fly;
}

// Map a displayed pixel back into the frozen image. Re-evaluating the shard a
// few times converges on the source cell after translation and rotation.
vec2 inverse_shard(vec2 output_point, float fly) {
    vec2 source = output_point;
    for (int i = 0; i < 3; i++) {
        vec3 shard = shard_at(source);
        vec2 center = shard_center(shard);
        vec2 translation = shard_translation(shard, center, fly);
        float rotation = shard_rotation(shard, fly);
        source = center + rotate2(output_point - center - translation, -rotation);
    }
    return source;
}

float crack_mask(vec2 point) {
    float radius = length(point);
    float ring_coord = ring_coordinate(point);
    float ring = floor(ring_coord);
    float sector_coord = sector_coordinate(point, ring);
    float ring_edge = min(fract(ring_coord), 1.0 - fract(ring_coord));
    float sector_edge = min(fract(sector_coord), 1.0 - fract(sector_coord));
    return min(ring_edge / 8.5, sector_edge * max(radius, 0.04) * TAU / 16.0);
}

void main() {
    vec2 impact = vec2(0.48, direction > 0.0 ? 0.60 : 0.40);
    vec2 output_point = v_coords - impact;
    output_point.x *= aspect;

    float crack_phase = smoothstep(0.02, 0.34, progress);
    float crack_reach = crack_phase * 1.35;
    float crack = crack_phase
        * (1.0 - smoothstep(crack_reach - 0.10, crack_reach, length(output_point)));
    float fly = smoothstep(0.30, 0.82, progress);
    fly *= fly;

    vec2 source_point = inverse_shard(output_point, fly);
    vec3 source_shard = shard_at(source_point);
    vec2 center = shard_center(source_shard);
    vec2 projected = center
        + rotate2(source_point - center, shard_rotation(source_shard, fly))
        + shard_translation(source_shard, center, fly);

    // Fixed-point inversion that landed on a different shard is a gap. The live
    // incoming workspace underneath shows through it.
    float mapped = 1.0 - step(0.004, length(projected - output_point));
    vec2 source_uv = source_point;
    source_uv.x /= aspect;
    source_uv += impact;
    mapped *= step(0.0, source_uv.x) * step(source_uv.x, 1.0);
    mapped *= step(0.0, source_uv.y) * step(source_uv.y, 1.0);

    vec4 color = texture2D(tex, clamp(source_uv, 0.0, 1.0));
#if defined(NO_ALPHA)
    color.a = 1.0;
#endif

    float edge = crack_mask(source_point);
    float shadow = (1.0 - smoothstep(0.0015, 0.0065, edge)) * crack;
    float glint = (1.0 - smoothstep(0.0003, 0.0015, edge)) * crack;
    color.rgb *= 1.0 - shadow * 0.42;
    color.rgb += vec3(color.a) * glint * 0.72;

    float fade = 1.0 - smoothstep(0.76, 1.0, progress);
    color *= mapped * fade * alpha;
    if (color.a <= 0.001)
        discard;

#if defined(DEBUG_FLAGS)
    if (tint == 1.0)
        color = vec4(0.0, 0.2, 0.0, 0.2) + color * 0.8;
#endif
    gl_FragColor = color;
}
