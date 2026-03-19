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

uniform float invert;
uniform float color_mode;
uniform float night_shift;

void main() {
    vec4 color = texture2D(tex, v_coords);

#if defined(NO_ALPHA)
    color = vec4(color.rgb, 1.0) * alpha;
#else
    color = color * alpha;
#endif

    // un-multiply
    color.rgb /= color.a;

    // First invert then filter

    if (invert == 1.0) {
        color.rgb = 1.0 - color.rgb;
    }

    if (color_mode == 1.0) {        // greyscale
        float value = (color.r + color.g + color.b) / 3.0;
        color = vec4(value, value, value, color.a);
    } else if (color_mode >= 2.0) {
        float L = (17.8824 * color.r) + (43.5161 * color.g) + (4.11935 * color.b);
	    float M = (3.45565 * color.r) + (27.1554 * color.g) + (3.86714 * color.b);
    	float S = (0.0299566 * color.r) + (0.184309 * color.g) + (1.46709 * color.b);

        float l, m, s;
        if (color_mode == 2.0) { // Protanopia
            l = 0.0 * L + 2.02344 * M + -2.52581 * S;
		    m = 0.0 * L + 1.0 * M + 0.0 * S;
		    s = 0.0 * L + 0.0 * M + 1.0 * S;
        } else if (color_mode == 3.0) { // Deuteranopia
            l = 1.0 * L + 0.0 * M + 0.0 * S;
            m = 0.494207 * L + 0.0 * M + 1.24827 * S;
            s = 0.0 * L + 0.0 * M + 1.0 * S; 
        } else if (color_mode == 4.0) { // Tritanopia
            l = 1.0 * L + 0.0 * M + 0.0 * S;
            m = 0.0 * L + 1.0 * M + 0.0 * S;
            s = -0.395913 * L + 0.801109 * M + 0.0 * S; 
        } else {
            // unknown
            l = L;
            m = M;
            s = S;
        }

        vec3 error;
        error.r = (0.0809444479 * l) + (-0.130504409 * m) + (0.116721066 * s);
        error.g = (-0.0102485335 * l) + (0.0540193266 * m) + (-0.113614708 * s);
        error.b = (-0.000365296938 * l) + (-0.00412161469 * m) + (0.693511405 * s);

        vec3 diff = color.rgb - error;
        vec3 correction;
        correction.r = 0.0;
        correction.g = (diff.r * 0.7) + (diff.g * 1.0);
        correction.b =  (diff.r * 0.7) + (diff.b * 1.0);

        color.rgb += correction;
    }

    // Night shift: warm color temperature via Planckian locus approximation.
    // night_shift carries the target temperature in Kelvin (e.g. 3500.0).
    // 6500 K ≈ daylight (no change); lower values are warmer.
    if (night_shift > 0.0 && night_shift < 6500.0) {
        float temp = night_shift / 100.0;
        float r_mult;
        float g_mult;
        float b_mult;

        // Red
        if (temp <= 66.0) {
            r_mult = 1.0;
        } else {
            float t = temp - 60.0;
            r_mult = 1.29293618606 * pow(t, -0.1332047592);
            r_mult = clamp(r_mult, 0.0, 1.0);
        }

        // Green
        if (temp <= 66.0) {
            g_mult = 0.39008157876 * log(temp) - 0.63184144378;
            g_mult = clamp(g_mult, 0.0, 1.0);
        } else {
            float t = temp - 60.0;
            g_mult = 1.12989086089 * pow(t, -0.0755148492);
            g_mult = clamp(g_mult, 0.0, 1.0);
        }

        // Blue
        if (temp >= 66.0) {
            b_mult = 1.0;
        } else if (temp <= 19.0) {
            b_mult = 0.0;
        } else {
            float t = temp - 10.0;
            b_mult = 0.54320678911 * log(t) - 1.19625408914;
            b_mult = clamp(b_mult, 0.0, 1.0);
        }

        // Normalize so that 6500 K maps to (1,1,1)
        float nr = 1.0;
        float ng = 0.39008157876 * log(65.0) - 0.63184144378;
        float nb = 0.54320678911 * log(55.0) - 1.19625408914;

        color.r *= r_mult / nr;
        color.g *= g_mult / ng;
        color.b *= b_mult / nb;
    }

    // re-multiply
    color.rgb *= color.a;

    gl_FragColor = color;
}