// Expectation table and independent host oracle for the Metal device gate.
// Values come from the mathematical definition, never from emitted source.
//
// Per-output expectation modes:
//   v compare with the double oracle    n expect NaN
//   p expect +inf                       m expect -inf
//   u expect -inf or NaN: a subnormal input may flush to zero before x*r.

namespace {

constexpr int kOutputs = 10;
constexpr int kGuardThreads = 8;
constexpr float kSentinel = 12345.678f;
constexpr double kTolerance = 1.0e-5;

struct Sample {
    float x;
    float dx;
    float seed;
    const char* mode;
};

// value refined square cube tangent adjoint wave extent root scaled
const Sample kTable[] = {
    {1.0f, -0.375f, 1.25f, "vvvvvvvvvv"},
    {2.0f, -0.375f, 1.25f, "vvvvvvvvvv"},
    {-3.5f, -0.375f, 1.25f, "vvvvvvvvnv"},
    {0.5f, -0.375f, 1.25f, "vvvvvvvvvv"},
    {123.456f, -0.375f, 1.25f, "vvvvvvvvvv"},
    {-0.125f, -0.375f, 1.25f, "vvvvvvvvnv"},
    {1.0e-3f, -0.375f, 1.25f, "vvvvvvvvvv"},
    {-999.75f, -0.375f, 1.25f, "vvvvvvvvnv"},
    {3.14159f, -0.375f, 1.25f, "vvvvvvvvvv"},
    {0.00123456f, -0.375f, 1.25f, "vvvvvvvvvv"},
    {0.0f, -0.375f, 1.25f, "pnpppmnvvp"},
    {-0.0f, -0.375f, 1.25f, "mnpmpmnvvp"},
    {FLT_MAX, -0.375f, 1.25f, "vvvvvvvvvv"},
    {-FLT_MAX, -0.375f, 1.25f, "vvvvvvvvnv"},
    {FLT_TRUE_MIN, -0.375f, 1.25f, "pupppmnvvp"},
    {1.0e-30f, -0.375f, 1.25f, "vvvvvvvvvv"},
    {-1.0e-30f, -0.375f, 1.25f, "vvvvvvvvnv"},
};

constexpr int kSamples = static_cast<int>(sizeof(kTable) / sizeof(kTable[0]));

constexpr const char* kNames[kOutputs] = {
    "value", "refined", "square", "cube", "tangent", "adjoint",
    "wave", "extent", "root", "scaled"};

// Oracle in double from x, dx and seed: r = 1/x, one Newton refinement
// r*(2-x*r), the power ladder, the JVP and VJP slopes, sine of the
// reciprocal, the ternary maximum, the square root and pi times the square.
void oracle(const Sample& s, double expected[kOutputs]) {
    const double x = s.x, dx = s.dx, seed = s.seed;
    const double r = 1.0 / x;
    const double pi = std::acos(-1.0);
    expected[0] = r;
    expected[1] = r * (2.0 - x * r);
    expected[2] = r * r;
    expected[3] = r * r * r;
    expected[4] = dx * (-(r * r));
    expected[5] = seed * (-(r * r));
    expected[6] = std::sin(r);
    expected[7] = std::fmax(x, std::fmax(dx, seed));
    expected[8] = std::pow(x, 0.5);
    expected[9] = pi * (r * r);
}

BOOL fail = NO;

// Metal specification 8.2/8.4: division rounds to nearest or toward zero;
// precise sin has at most 4 ulp error. Quantize its input independently,
// since sin(1/x) is ill-conditioned with respect to reciprocal rounding.
// https://developer.apple.com/metal/Metal-Shading-Language-Specification.pdf
BOOL check_wave(int i, float got, double* max_rel) {
    const double r = 1.0 / static_cast<double>(kTable[i].x);
    const float nearest = static_cast<float>(r);
    const float toward_zero = std::fabs(static_cast<double>(nearest)) > std::fabs(r)
        ? std::nextafter(nearest, 0.0f) : nearest;
    const float candidates[] = {nearest, toward_zero};
    double best = HUGE_VAL;
    for (float candidate : candidates) {
        const double expected = std::sin(static_cast<double>(candidate));
        const float rounded = static_cast<float>(expected);
        const double ulp = std::fmax(
            static_cast<double>(std::nextafter(rounded, INFINITY)) - rounded,
            static_cast<double>(rounded) - std::nextafter(rounded, -INFINITY));
        const double error = std::fabs(static_cast<double>(got) - expected);
        if (error < best) best = error;
        if (std::isfinite(got) && error <= 4.0 * ulp) {
            if (error > max_rel[6]) max_rel[6] = error;
            return YES;
        }
        // Metal 8.1 permits subnormal results to flush to zero.
        if (got == 0.0f && std::fabs(expected) < FLT_MIN) return YES;
    }
    std::fprintf(stderr, "sample %d wave: got %.9g, error %.3g from rounded-input "
                        "double oracle exceeds 4 ulp\n", i, got, best);
    return NO;
}

BOOL check(int i, int j, float got, double expected, char mode, double* max_rel) {
    if (mode == 'u') {
        if (std::isnan(got) || (std::isinf(got) && std::signbit(got))) return YES;
        std::fprintf(stderr, "sample %d %s: expected -inf or NaN after possible "
                            "subnormal flush, got %.9g\n", i, kNames[j], got);
        return NO;
    }
    if (mode == 'n') {
        if (std::isnan(got)) return YES;
        std::fprintf(stderr, "sample %d %s: expected NaN, got %.9g\n", i, kNames[j], got);
        return NO;
    }
    if (mode == 'p' || mode == 'm') {
        const BOOL want_positive = (mode == 'p');
        if (!std::isinf(got) || (got > 0) != want_positive) {
            std::fprintf(stderr, "sample %d %s: expected %sinf, got %.9g\n", i, kNames[j],
                        want_positive ? "+" : "-", got);
            return NO;
        }
        return YES;
    }
    if (j == 6) return check_wave(i, got, max_rel);
    if (std::isnan(expected)) {
        std::fprintf(stderr, "sample %d %s: oracle is NaN but mode asks for a value\n",
                    i, kNames[j]);
        return NO;
    }
    if (std::fabs(expected) > static_cast<double>(FLT_MAX)) {
        if (!std::isinf(got)) {
            std::fprintf(stderr, "sample %d %s: oracle %.17g exceeds binary32 range, "
                        "expected overflow, got %.9g\n", i, kNames[j], expected, got);
            return NO;
        }
        if (std::signbit(got) != std::signbit(expected)) {
            std::fprintf(stderr, "sample %d %s: overflow sign differs\n", i, kNames[j]);
            return NO;
        }
        return YES;
    }
    if (!std::isfinite(got)) {
        std::fprintf(stderr, "sample %d %s: expected finite, got %.9g\n", i, kNames[j], got);
        return NO;
    }
    const double rel = std::fabs(static_cast<double>(got) - expected) /
        std::fmax(1.0, std::fabs(expected));
    if (rel > max_rel[j]) max_rel[j] = rel;
    if (rel > kTolerance) {
        std::fprintf(stderr, "sample %d %s: got %.9g oracle %.17g relative %.3g\n",
                    i, kNames[j], got, expected, rel);
        return NO;
    }
    return YES;
}

}  // namespace
