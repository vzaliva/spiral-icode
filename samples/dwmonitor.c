/*	Dynamic Window Approach Safety Monitor

	Version:
	C99, Intel C++, SSE 4.1

	inputs:
	D[0] = (A/b+1)*(A/2*eps^2+eps*V)
	D[1] = V/b + eps*(A/b+1)
	D[2] = 1/(2*b)

	X[0] = vr
	X[1..2] = pr.x, pr.y
	x[3..4] = po.x, po.y

	computes:
	(A/b+1)*(A/2*eps^2+eps*V) + (V/b + eps*(A/b+1))*vr + (1/(2*b))*vr^2 < || pr - po||_oo

	return:
	1 => true
	0 => false
	-1 => unknown

*/

#include "dwmonitor.h"
#include <smmintrin.h>
#include <float.h>

int dwmonitor(float  *X, double  *D) {
    __m128d u10, u11, u12, u13, u14, u15, u16, u9
            , x20, x21, x22, x23, x25, x26, x27, x28
            , x29, x32, x33, x36, x37, x38;
    int w3;
    {
        unsigned _xm = _mm_getcsr();
        _mm_setcsr(_xm & 0xffff0000 | 0x0000dfc0);
        u13 = _mm_set1_pd(0.0);
        u10 = _mm_cvtps_pd(_mm_addsub_ps(_mm_set1_ps(FLT_MIN), _mm_set1_ps(X[0])));
        u9 = _mm_set_pd(1.0, (-1.0));
        for(int i33 = 0; i33 <= 2; i33++) {
            x25 = _mm_addsub_pd(_mm_set1_pd((DBL_MIN + DBL_MIN)), _mm_loaddup_pd(&(D[i33])));
            x20 = _mm_addsub_pd(_mm_set1_pd(0.0), u9);
            x21 = _mm_mul_pd(x20, x25);
            x22 = _mm_mul_pd(_mm_shuffle_pd(x20, x20, _MM_SHUFFLE2(0, 1)), x25);
            x23 = _mm_sub_pd(_mm_set1_pd(0.0), _mm_min_pd(x21, x22));
            u11 = _mm_add_pd(max(_mm_shuffle_pd(x23, x23, _MM_SHUFFLE2(0, 1)),x21), _mm_set1_pd(DBL_MIN));
            u13 = _mm_add_pd(u13, u11);
            x26 = _mm_addsub_pd(_mm_set1_pd(0.0), u9);
            x27 = _mm_mul_pd(x26, u10);
            x28 = _mm_mul_pd(_mm_shuffle_pd(x26, x26, _MM_SHUFFLE2(0, 1)), u10);
            x29 = _mm_sub_pd(_mm_set1_pd(0.0), _mm_min_pd(x27, x28));
            u9 = _mm_add_pd(max(_mm_shuffle_pd(x29, x29, _MM_SHUFFLE2(0, 1)),x27), _mm_set1_pd(DBL_MIN));
        }
        u14 = _mm_set1_pd(0.0);
        for(int i31 = 0; i31 <= 1; i31++) {
            u16 = _mm_cvtps_pd(_mm_addsub_ps(_mm_set1_ps(FLT_MIN), _mm_set1_ps(X[(i31 + 1)])));
            u15 = _mm_cvtps_pd(_mm_addsub_ps(_mm_set1_ps(FLT_MIN), _mm_set1_ps(X[(3 + i31)])));
            x33 = _mm_add_pd(u16, _mm_shuffle_pd(u15, u15, _MM_SHUFFLE2(0, 1)));
            x32 = _mm_shuffle_pd(x33, x33, _MM_SHUFFLE2(0, 1));
            u12 = _mm_shuffle_pd(_mm_min_pd(x32, x33), max(x32,x33), _MM_SHUFFLE2(1, 0));
            u14 = _mm_shuffle_pd(_mm_min_pd(u12, u14), max(u12,u14), _MM_SHUFFLE2(1, 0));
        }
        x36 = _mm_addsub_pd(_mm_set1_pd(0.0), u14);
        x37 = _mm_addsub_pd(_mm_set1_pd(0.0), u13);
        x38 = _mm_cmpge_pd(x36, _mm_shuffle_pd(x37, x37, _MM_SHUFFLE2(0, 1)));
        w3 = (_mm_testc_si128(_mm_castpd_si128(x38), _mm_set_epi32(0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff)) - (_mm_testnzc_si128(_mm_castpd_si128(x38), _mm_set_epi32(0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff))));
        // BASIC BLOCK BARRIER
        if (_mm_getcsr() & 0x0d) {
            _mm_setcsr(_xm);
            return -1;
        }
        _mm_setcsr(_xm);
    }
    return w3;
}
