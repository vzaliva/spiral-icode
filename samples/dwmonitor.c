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
    __m128d u1, u2, u3, u4, u5, u6, u7, u8
            , x1, x10, x13, x14, x17, x18, x19, x2
            , x3, x4, x6, x7, x8, x9;
    int w1;
    {
        unsigned _xm = _mm_getcsr();
        _mm_setcsr(_xm & 0xffff0000 | 0x0000dfc0);
        u5 = _mm_set1_pd(0.0);
        u2 = _mm_cvtps_pd(_mm_addsub_ps(_mm_set1_ps(FLT_MIN), _mm_set1_ps(X[0])));
        u1 = _mm_set_pd(1.0, (-1.0));
        for(int i5 = 0; i5 <= 2; i5++) {
            x6 = _mm_addsub_pd(_mm_set1_pd((DBL_MIN + DBL_MIN)), _mm_loaddup_pd(&(D[i5])));
            x1 = _mm_addsub_pd(_mm_set1_pd(0.0), u1);
            x2 = _mm_mul_pd(x1, x6);
            x3 = _mm_mul_pd(_mm_shuffle_pd(x1, x1, _MM_SHUFFLE2(0, 1)), x6);
            x4 = _mm_sub_pd(_mm_set1_pd(0.0), _mm_min_pd(x3, x2));
            u3 = _mm_add_pd(_mm_max_pd(_mm_shuffle_pd(x4, x4, _MM_SHUFFLE2(0, 1)), _mm_max_pd(x3, x2)), _mm_set1_pd(DBL_MIN));
            u5 = _mm_add_pd(u5, u3);
            x7 = _mm_addsub_pd(_mm_set1_pd(0.0), u1);
            x8 = _mm_mul_pd(x7, u2);
            x9 = _mm_mul_pd(_mm_shuffle_pd(x7, x7, _MM_SHUFFLE2(0, 1)), u2);
            x10 = _mm_sub_pd(_mm_set1_pd(0.0), _mm_min_pd(x9, x8));
            u1 = _mm_add_pd(_mm_max_pd(_mm_shuffle_pd(x10, x10, _MM_SHUFFLE2(0, 1)), _mm_max_pd(x9, x8)), _mm_set1_pd(DBL_MIN));
        }
        u6 = _mm_set1_pd(0.0);
        for(int i3 = 0; i3 <= 1; i3++) {
            u8 = _mm_cvtps_pd(_mm_addsub_ps(_mm_set1_ps(FLT_MIN), _mm_set1_ps(X[(i3 + 1)])));
            u7 = _mm_cvtps_pd(_mm_addsub_ps(_mm_set1_ps(FLT_MIN), _mm_set1_ps(X[(3 + i3)])));
            x14 = _mm_add_pd(u8, _mm_shuffle_pd(u7, u7, _MM_SHUFFLE2(0, 1)));
            x13 = _mm_shuffle_pd(x14, x14, _MM_SHUFFLE2(0, 1));
            u4 = _mm_shuffle_pd(_mm_min_pd(x14, x13), _mm_max_pd(x14, x13), _MM_SHUFFLE2(1, 0));
            u6 = _mm_shuffle_pd(_mm_min_pd(u6, u4), _mm_max_pd(u6, u4), _MM_SHUFFLE2(1, 0));
        }
        x17 = _mm_addsub_pd(_mm_set1_pd(0.0), u6);
        x18 = _mm_addsub_pd(_mm_set1_pd(0.0), u5);
        x19 = _mm_cmpge_pd(x17, _mm_shuffle_pd(x18, x18, _MM_SHUFFLE2(0, 1)));
        w1 = (_mm_testc_si128(_mm_castpd_si128(x19), _mm_set_epi32(0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff)) - (_mm_testnzc_si128(_mm_castpd_si128(x19), _mm_set_epi32(0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff))));
        /* __asm nop; */
        if (_mm_getcsr() & 0x0d) {
            _mm_setcsr(_xm);
            return -1;
        }
        _mm_setcsr(_xm);
    }
    return w1;
}
