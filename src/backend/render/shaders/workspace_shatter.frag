// Procedural glass cracks and graphic shards over the outgoing workspace.

precision highp float;
uniform float alpha;
#if defined(DEBUG_FLAGS)
uniform float tint;
#endif
uniform vec2 size;
varying vec2 v_coords;

uniform vec3 color;
uniform float progress;
uniform float aspect;
uniform float direction;
uniform float seed;

const float PI = 3.14159265359;
const float TAU = 6.28318530718;

float hash(float value) {
    return fract(sin(value * 127.1 + seed * 311.7) * 43758.5453);
}

float stroke(float distance, float width) {
    return 1.0 - smoothstep(width, width * 1.8, distance);
}

float ray_distance(vec2 p, float radius, float angle, float index, float reach) {
    float ray = -PI + TAU * ((index + 0.5) / 13.0);
    ray += (hash(index + 2.0) - 0.5) * 0.34;
    float ahead = dot(p, vec2(cos(ray), sin(ray)));
    float bend = sin(radius * (13.0 + hash(index) * 9.0) + index) * 0.035;
    float distance = abs(sin(angle - ray - bend)) * radius;
    float ray_reach = reach * (0.72 + hash(index + 19.0) * 0.42);
    distance = max(distance, radius - ray_reach);
    return mix(10.0, distance, step(0.0, ahead));
}

float radial_distance(vec2 p, float reach) {
    float radius = length(p);
    float angle = atan(p.y, p.x);
    float ray_coord = (angle + PI) / TAU * 13.0 - 0.5;
    float index = mod(floor(ray_coord) + 13.0, 13.0);
    float distance = ray_distance(p, radius, angle, index, reach);
    distance = min(distance, ray_distance(p, radius, angle, mod(index + 12.0, 13.0), reach));
    return min(distance, ray_distance(p, radius, angle, mod(index + 1.0, 13.0), reach));
}

float ring_distance(vec2 p, float reach) {
    float radius = length(p);
    float angle = atan(p.y, p.x);
    float sector = floor((angle + PI) * 4.5);
    float distance = 10.0;

    for (int i = 1; i < 5; i++) {
        float index = float(i);
        float target = index * 0.12;
        target += sin(angle * (5.0 + index) + index * 2.7) * 0.018;
        float broken = step(0.28, hash(sector + index * 31.0));
        float reached = step(target, reach);
        float candidate = mix(10.0, abs(radius - target), broken * reached);
        distance = min(distance, candidate);
    }
    return distance;
}

void main() {
    vec2 impact = vec2(0.47, direction > 0.0 ? 0.62 : 0.38);
    vec2 p = v_coords - impact;
    p.x *= aspect;

    float radius = length(p);
    float angle = atan(p.y, p.x);
    float crack_phase = smoothstep(0.02, 0.42, progress);
    float reach = crack_phase * 1.35;

    float crack_distance = min(radial_distance(p, reach), ring_distance(p, reach));
    float crack_shadow = stroke(crack_distance, 0.0065);
    float crack_core = stroke(crack_distance, 0.0022);

    float impact_flash = (1.0 - smoothstep(0.0, 0.075, radius));
    impact_flash *= 1.0 - smoothstep(0.08, 0.34, progress);

    float break_phase = smoothstep(0.36, 0.56, progress);
    float first_ring = floor(radius * 8.0);
    float first_sector = floor((angle + PI) / TAU * 15.0 + first_ring * 0.73);
    float first_cell = hash(first_ring * 37.0 + first_sector * 11.0);

    // Evaluate the polar cells behind their outward trajectory so the shards fly.
    vec2 shard_p = p - normalize(p + vec2(0.0001))
        * break_phase * (0.025 + first_cell * 0.085);
    shard_p.y += break_phase * break_phase * (0.02 + first_cell * 0.04);
    float shard_radius = length(shard_p);
    float shard_angle = atan(shard_p.y, shard_p.x);
    float ring_id = floor(shard_radius * 8.0);
    float sector_coord = (shard_angle + PI) / TAU * 15.0 + ring_id * 0.73;
    float sector_id = floor(sector_coord);
    float cell = hash(ring_id * 37.0 + sector_id * 11.0);
    float shard_life = 1.0 - smoothstep(0.52 + cell * 0.16, 0.78 + cell * 0.18, progress);

    float ring_frac = fract(shard_radius * 8.0);
    float sector_frac = fract(sector_coord);
    float ring_edge = stroke(min(ring_frac, 1.0 - ring_frac), 0.035);
    float sector_edge = stroke(min(sector_frac, 1.0 - sector_frac), 0.035);
    float shard_edge = max(ring_edge, sector_edge) * break_phase * shard_life;
    float shard_fill = break_phase * shard_life * (0.12 + cell * 0.22);

    float crack_life = 1.0 - smoothstep(0.62, 0.94, progress);
    vec3 crimson = mix(vec3(0.95, 0.025, 0.07), color, 0.16);
    float shadow_alpha = crack_shadow * crack_life * 0.72;
    float core_alpha = crack_core * crack_life * 0.96;
    float shard_alpha = max(shard_fill, shard_edge * 0.72);
    float out_alpha = max(max(shadow_alpha, core_alpha), max(shard_alpha, impact_flash));

    vec3 rgb = crimson * (core_alpha + shard_edge * 0.44);
    rgb += vec3(1.0) * impact_flash;
    rgb *= 1.0 - shadow_alpha * (1.0 - crack_core);
    rgb += vec3(0.015) * shard_fill;
    rgb = min(rgb, vec3(out_alpha));

    vec4 result = vec4(rgb * alpha, out_alpha * alpha);
#if defined(DEBUG_FLAGS)
    if (tint == 1.0)
        result = vec4(0.3, 0.0, 0.0, 0.2) + result * 0.8;
#endif
    gl_FragColor = result;
}
