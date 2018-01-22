#include <math.h>
#include <include/omega32.h>
#include <nmmintrin.h>
#include <smmintrin.h>
#include <tmmintrin.h>
#include <pmmintrin.h>
#include <emmintrin.h>
#include <xmmintrin.h>
#include <mmintrin.h>

void init_dft16() {
}

void dft16(float  *Y, float  *X) {
    __m128 s139, s140, s141, s142, s143, s144, s145, s146
    , s147, s148, s149, s150, s151, s152, s153, s154
    , s155, s156, s157, s158, s159, s160, s161, s162
    , s163, s164, s165, s166, s167, s168, s169, s170
    , s171, s172, s173, s174, s175, s176, s177, s178
    , s179, s180, s181, s182, s183, s184, t129, t130
    , t131, t132, t133, t134, t135, t136, t137, t138
    , t139, t140, t141, t142, t143, t144, t145, t146
    , t147, t148, t149, t150, t151, t152, t153, t154
    , t155, t156, t157, t158, t159, t160;
    __m128  *a45, *a46;
    a45 = ((__m128  *) X);
    s139 = *(a45);
    s140 = *((a45 + 1));
    s141 = _mm_shuffle_ps(s139, s140, _MM_SHUFFLE(2, 0, 2, 0));
    s142 = _mm_shuffle_ps(s139, s140, _MM_SHUFFLE(3, 1, 3, 1));
    s143 = *((a45 + 4));
    s144 = *((a45 + 5));
    s145 = _mm_shuffle_ps(s143, s144, _MM_SHUFFLE(2, 0, 2, 0));
    s146 = _mm_shuffle_ps(s143, s144, _MM_SHUFFLE(3, 1, 3, 1));
    t129 = _mm_add_ps(s141, s145);
    t130 = _mm_add_ps(s142, s146);
    t131 = _mm_sub_ps(s141, s145);
    t132 = _mm_sub_ps(s142, s146);
    s147 = *((a45 + 2));
    s148 = *((a45 + 3));
    s149 = _mm_shuffle_ps(s147, s148, _MM_SHUFFLE(2, 0, 2, 0));
    s150 = _mm_shuffle_ps(s147, s148, _MM_SHUFFLE(3, 1, 3, 1));
    s151 = *((a45 + 6));
    s152 = *((a45 + 7));
    s153 = _mm_shuffle_ps(s151, s152, _MM_SHUFFLE(2, 0, 2, 0));
    s154 = _mm_shuffle_ps(s151, s152, _MM_SHUFFLE(3, 1, 3, 1));
    t133 = _mm_add_ps(s149, s153);
    t134 = _mm_add_ps(s150, s154);
    t135 = _mm_sub_ps(s149, s153);
    t136 = _mm_sub_ps(s150, s154);
    t137 = _mm_add_ps(t129, t133);
    t138 = _mm_add_ps(t130, t134);
    t139 = _mm_sub_ps(t129, t133);
    t140 = _mm_sub_ps(t130, t134);
    t141 = _mm_sub_ps(t131, t136);
    t142 = _mm_add_ps(t132, t135);
    t143 = _mm_add_ps(t131, t136);
    t144 = _mm_sub_ps(t132, t135);
    s155 = _mm_unpacklo_ps(t137, t141);
    s156 = _mm_unpackhi_ps(t137, t141);
    s157 = _mm_unpacklo_ps(t138, t142);
    s158 = _mm_unpackhi_ps(t138, t142);
    s159 = _mm_unpacklo_ps(t139, t143);
    s160 = _mm_unpackhi_ps(t139, t143);
    s161 = _mm_unpacklo_ps(t140, t144);
    s162 = _mm_unpackhi_ps(t140, t144);
    s163 = _mm_shuffle_ps(s155, s159, _MM_SHUFFLE(1, 0, 1, 0));
    s164 = _mm_shuffle_ps(s155, s159, _MM_SHUFFLE(3, 2, 3, 2));
    s165 = _mm_shuffle_ps(s156, s160, _MM_SHUFFLE(1, 0, 1, 0));
    s166 = _mm_shuffle_ps(s156, s160, _MM_SHUFFLE(3, 2, 3, 2));
    s167 = _mm_shuffle_ps(s157, s161, _MM_SHUFFLE(1, 0, 1, 0));
    s168 = _mm_shuffle_ps(s157, s161, _MM_SHUFFLE(3, 2, 3, 2));
    s169 = _mm_shuffle_ps(s158, s162, _MM_SHUFFLE(1, 0, 1, 0));
    s170 = _mm_shuffle_ps(s158, s162, _MM_SHUFFLE(3, 2, 3, 2));
    s171 = _mm_sub_ps(_mm_mul_ps(_mm_set_ps((-0.70710678118654757f), 0.0f, 0.70710678118654757f, 1.0f), s165), _mm_mul_ps(_mm_set_ps(0.70710678118654757f, 1.0f, 0.70710678118654757f, 0.0f), s169));
    s172 = _mm_add_ps(_mm_mul_ps(_mm_set_ps(0.70710678118654757f, 1.0f, 0.70710678118654757f, 0.0f), s165), _mm_mul_ps(_mm_set_ps((-0.70710678118654757f), 0.0f, 0.70710678118654757f, 1.0f), s169));
    t145 = _mm_add_ps(s163, s171);
    t146 = _mm_add_ps(s167, s172);
    t147 = _mm_sub_ps(s163, s171);
    t148 = _mm_sub_ps(s167, s172);
    s173 = _mm_sub_ps(_mm_mul_ps(_mm_set_ps(0.38268343236508978f, 0.70710678118654757f, 0.92387953251128674f, 1.0f), s164), _mm_mul_ps(_mm_set_ps(0.92387953251128674f, 0.70710678118654757f, 0.38268343236508978f, 0.0f), s168));
    s174 = _mm_add_ps(_mm_mul_ps(_mm_set_ps(0.92387953251128674f, 0.70710678118654757f, 0.38268343236508978f, 0.0f), s164), _mm_mul_ps(_mm_set_ps(0.38268343236508978f, 0.70710678118654757f, 0.92387953251128674f, 1.0f), s168));
    s175 = _mm_sub_ps(_mm_mul_ps(_mm_set_ps((-0.92387953251128674f), (-0.70710678118654757f), 0.38268343236508978f, 1.0f), s166), _mm_mul_ps(_mm_set_ps((-0.38268343236508978f), 0.70710678118654757f, 0.92387953251128674f, 0.0f), s170));
    s176 = _mm_add_ps(_mm_mul_ps(_mm_set_ps((-0.38268343236508978f), 0.70710678118654757f, 0.92387953251128674f, 0.0f), s166), _mm_mul_ps(_mm_set_ps((-0.92387953251128674f), (-0.70710678118654757f), 0.38268343236508978f, 1.0f), s170));
    t149 = _mm_add_ps(s173, s175);
    t150 = _mm_add_ps(s174, s176);
    t151 = _mm_sub_ps(s173, s175);
    t152 = _mm_sub_ps(s174, s176);
    t153 = _mm_add_ps(t145, t149);
    t154 = _mm_add_ps(t146, t150);
    t155 = _mm_sub_ps(t145, t149);
    t156 = _mm_sub_ps(t146, t150);
    a46 = ((__m128  *) Y);
    s177 = _mm_unpacklo_ps(t153, t154);
    *(a46) = s177;
    s178 = _mm_unpackhi_ps(t153, t154);
    *((a46 + 1)) = s178;
    s179 = _mm_unpacklo_ps(t155, t156);
    *((a46 + 4)) = s179;
    s180 = _mm_unpackhi_ps(t155, t156);
    *((a46 + 5)) = s180;
    t157 = _mm_sub_ps(t147, t152);
    t158 = _mm_add_ps(t148, t151);
    t159 = _mm_add_ps(t147, t152);
    t160 = _mm_sub_ps(t148, t151);
    s181 = _mm_unpacklo_ps(t157, t158);
    *((a46 + 2)) = s181;
    s182 = _mm_unpackhi_ps(t157, t158);
    *((a46 + 3)) = s182;
    s183 = _mm_unpacklo_ps(t159, t160);
    *((a46 + 6)) = s183;
    s184 = _mm_unpackhi_ps(t159, t160);
    *((a46 + 7)) = s184;
}
