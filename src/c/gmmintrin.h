/***************************************************************
This code is part of Spiral 6.0 beta, www.spiral.net --
Copyright (c) 2008, Carnegie Mellon University
All rights reserved.
The code is distributed under a BSD style license
(see http://www.opensource.org/licenses/bsd-license.php)

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

* Redistributions of source code must retain the above copyright
  notice, reference to Spiral, this list of conditions and the
  following disclaimer.
* Redistributions in binary form must reproduce the above
  copyright notice, this list of conditions and the following
  disclaimer in the documentation and/or other materials provided
  with the distribution.
* Neither the name of Carnegie Mellon University nor the name of its
  contributors may be used to endorse or promote products derived from
  this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
*AS IS* AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
******************************************************************/


#ifndef __AVXEMU_H__
#define __AVXEMU_H__

#if defined(__ICL) || defined(_MSC_VER)
#define INLINE	__inline
#else
#define INLINE	__inline__
#endif

// SSE 4.1 not available before Penryn, but part of AVX
#ifdef HAVE_SSE41
#include <smmintrin.h>
#else
#include <pmmintrin.h>
int _mm_extract_ps(__m128 src, const int ndx)
{
	union {
		__m128 val128;
		__int32 val32[4];
	} val;

	val.val128 = src;
	return val.val32[ndx];
}

__m128i _mm_insert_epi32 (__m128i s2, int s, const int ndx) {
	union {
		__m128i val128;
		__int32 val32[4];
	} val;

	val.val128 = s2;
	val.val32[ndx] = s;

	return val.val128;
}
#endif


//	helper functions
#define PRINT_M256D(m256d)	printf("(%f, %f, %f, %f)", m256d.d[3], m256d.d[2], m256d.d[1], m256d.d[0]) 
#define PRINT_M256(m256)	printf("(%f, %f, %f, %f, %f, %f, %f, %f)", (double)m256.f[7], (double)m256.f[6], (double)m256.f[5], (double)m256.f[4], (double)m256.f[3], (double)m256.f[2], (double)m256.f[1], (double)m256.f[0])  

typedef struct {
	double d[4];
} __m256d;

typedef struct {
	float f[8];
} __m256;

typedef struct {
	unsigned __int8 i[32];
} __m256i;


//=======================================
INLINE void _mm256_zeroall(void) {
}

INLINE void _mm256_zeroupper(void) {
}

//=======================================
INLINE __m256 _mm256_setzero_ps(void) {
	__m256 val256;
	int i; for (i=0; i<8; i++)
		val256.f[i] = 0;
	return val256;
}

INLINE __m256 _mm256_set1_ps(float f) {
	__m256 val256;
	int i; for (i=0; i<8; i++)
		val256.f[i] = f;
	return val256;
}

INLINE __m256 _mm256_set_ps(float f0, float f1, float f2, float f3, float f4, float f5, float f6, float f7) {
	__m256 val256;
	val256.f[0] = f7;
	val256.f[1] = f6;
	val256.f[2] = f5;
	val256.f[3] = f4;
	val256.f[4] = f3;
	val256.f[5] = f2;
	val256.f[6] = f1;
	val256.f[7] = f0;
	return val256;
}

INLINE __m256 _mm256_xor_ps (__m256 a, __m256 b) {
	__m256 val256;
	__int32 *ap = (__int32 *)&a, 
		*bp = (__int32 *)&b,
		*vp = (__int32 *)&val256;

	int i; for (i=0; i<8; i++)
		vp[i] = ap[i] ^ bp[i];
	
	return val256;
}

INLINE __m256 _mm256_broadcast_ss(float * a) {
	__m256 val256;
	int i; for (i=0; i<8; i++)
		val256.f[i] = *a;
	return val256;
}

INLINE __m256 _mm256_load_ps(float * p) {
	__m256 val256;
	int i; for (i=0; i<8; i++)
		val256.f[i] = p[i];
	return val256;
}

INLINE void _mm256_store_ps(float * p, __m256 a) {
	int i; for (i=0; i<8; i++)
		p[i] = a.f[i];
}

INLINE __m256 _mm256_add_ps (__m256 a, __m256 b) {
	__m256 val256;
	int i; for (i=0; i<8; i++)
		val256.f[i] = a.f[i] + b.f[i];
	return val256;
}

INLINE __m256 _mm256_sub_ps (__m256 a, __m256 b) {
	__m256 val256;
	int i; for (i=0; i<8; i++)
		val256.f[i] = a.f[i] - b.f[i];
	return val256;
}

INLINE __m256 _mm256_mul_ps (__m256 a, __m256 b) {
	__m256 val256;
	int i; for (i=0; i<8; i++)
		val256.f[i] = a.f[i] * b.f[i];
	return val256;
}

INLINE __m256 _mm256_unpacklo_ps(__m256 a, __m256 b) {
	__m256 val256;
	val256.f[0] = a.f[0];
	val256.f[1] = b.f[0];
	val256.f[2] = a.f[1];
	val256.f[3] = b.f[1];
	val256.f[4] = a.f[4];
	val256.f[5] = b.f[4];
	val256.f[6] = a.f[5];
	val256.f[7] = b.f[5];
	return val256;
}

INLINE __m256 _mm256_unpackhi_ps(__m256 a, __m256 b) {
	__m256 val256;
	val256.f[0] = a.f[2];
	val256.f[1] = b.f[2];
	val256.f[2] = a.f[3];
	val256.f[3] = b.f[3];
	val256.f[4] = a.f[6];
	val256.f[5] = b.f[6];
	val256.f[6] = a.f[7];
	val256.f[7] = b.f[7];
	return val256;
}

INLINE __m256 _mm256_permute2f128_ps (__m256 a, __m256 b, int control) {
	__m256 val256;

	switch (control & 0x03) {
		case 0x00: 
			val256.f[0] = a.f[0]; 
			val256.f[1] = a.f[1];
			val256.f[2] = a.f[2]; 
			val256.f[3] = a.f[3];
			break;
		case 0x01: 
			val256.f[0] = a.f[4]; 
			val256.f[1] = a.f[5];
			val256.f[2] = a.f[6]; 
			val256.f[3] = a.f[7];
			break;
		case 0x02: 
			val256.f[0] = b.f[0]; 
			val256.f[1] = b.f[1];
			val256.f[2] = b.f[2]; 
			val256.f[3] = b.f[3];
			break;
		case 0x03: 
			val256.f[0] = b.f[4]; 
			val256.f[1] = b.f[5];
			val256.f[2] = b.f[6]; 
			val256.f[3] = b.f[7];
			break;
	}

	switch (control & 0x30) {
		case 0x00: 
			val256.f[4] = a.f[0]; 
			val256.f[5] = a.f[1];
			val256.f[6] = a.f[2]; 
			val256.f[7] = a.f[3];
			break;
		case 0x10: 
			val256.f[4] = a.f[4]; 
			val256.f[5] = a.f[5];
			val256.f[6] = a.f[6]; 
			val256.f[7] = a.f[7];
			break;
		case 0x20: 
			val256.f[4] = b.f[0]; 
			val256.f[5] = b.f[1];
			val256.f[6] = b.f[2]; 
			val256.f[7] = b.f[3];
			break;
		case 0x30: 
			val256.f[4] = b.f[4]; 
			val256.f[5] = b.f[5];
			val256.f[6] = b.f[6]; 
			val256.f[7] = b.f[7];
			break;
	}
	
	if (control & 0x08) {
		val256.f[0] = 0.0; 
		val256.f[1] = 0.0;
		val256.f[2] = 0.0; 
		val256.f[3] = 0.0;
	}

	if (control & 0x80) {
		val256.f[4] = 0.0; 
		val256.f[5] = 0.0;
		val256.f[6] = 0.0; 
		val256.f[7] = 0.0;
	}

	return val256;
}

INLINE __m256 _mm256_permute2_ps(__m256 a, __m256 b, __m256i control, int imm) {
	__m256 val256;
	int c0, c1, c2, c3, c4, c5, c6, c7, zero_match;

	c0 = control.i[0];
	c1 = control.i[4];
	c2 = control.i[8];
	c3 = control.i[12];
	c4 = control.i[16];
	c5 = control.i[20];
	c6 = control.i[24];
	c7 = control.i[28];
	zero_match = imm & 3;

	if (!(c0 & 0x04))
		val256.f[0] = a.f[c0 & 0x03];
	else
		val256.f[0] = b.f[c0 & 0x03];

	if ((zero_match == 2) && ((c0 & 0x08) == 8)) 
		val256.f[0] = 0.0;
	if ((zero_match == 3) && ((c0 & 0x08) == 0)) 
		val256.f[0] = 0.0;

	if (!(c1 & 0x04))
		val256.f[1] = a.f[c1 & 0x03];
	else
		val256.f[1] = b.f[c1 & 0x03];

	if ((zero_match == 2) && ((c1 & 0x08) == 8)) 
		val256.f[1] = 0.0;
	if ((zero_match == 3) && ((c1 & 0x08) == 0)) 
		val256.f[1] = 0.0;

	if (!(c2 & 0x04))
		val256.f[2] = a.f[c2 & 0x03];
	else
		val256.f[2] = b.f[c2 & 0x03];

	if ((zero_match == 2) && ((c2 & 0x08) == 8)) 
		val256.f[2] = 0.0;
	if ((zero_match == 3) && ((c2 & 0x08) == 0)) 
		val256.f[2] = 0.0;

	if (!(c3 & 0x04))
		val256.f[3] = a.f[c3 & 0x03];
	else
		val256.f[3] = b.f[c3 & 0x03];

	if ((zero_match == 2) && ((c3 & 0x08) == 8)) 
		val256.f[3] = 0.0;
	if ((zero_match == 3) && ((c3 & 0x08) == 0)) 
		val256.f[3] = 0.0;


	if (!(c4 & 0x04))
		val256.f[4] = a.f[(c4 & 0x03) + 4];
	else
		val256.f[4] = b.f[(c4 & 0x03) + 4];

	if ((zero_match == 2) && ((c4 & 0x08) == 8)) 
		val256.f[4] = 0.0;
	if ((zero_match == 3) && ((c4 & 0x08) == 0)) 
		val256.f[4] = 0.0;

	if (!(c5 & 0x04))
		val256.f[5] = a.f[(c5 & 0x03) + 4];
	else
		val256.f[5] = b.f[(c5 & 0x03) + 4];

	if ((zero_match == 2) && ((c5 & 0x08) == 8)) 
		val256.f[5] = 0.0;
	if ((zero_match == 3) && ((c5 & 0x08) == 0)) 
		val256.f[5] = 0.0;

	if (!(c6 & 0x04))
		val256.f[6] = a.f[(c6 & 0x03) + 4];
	else
		val256.f[6] = b.f[(c6 & 0x03) + 4];

	if ((zero_match == 2) && ((c6 & 0x08) == 8)) 
		val256.f[6] = 0.0;
	if ((zero_match == 3) && ((c6 & 0x08) == 0)) 
		val256.f[6] = 0.0;

	if (!(c7 & 0x04))
		val256.f[7] = a.f[(c7 & 0x03) + 4];
	else
		val256.f[7] = b.f[(c7 & 0x03) + 4];

	if ((zero_match == 2) && ((c7 & 0x08) == 8)) 
		val256.f[7] = 0.0;
	if ((zero_match == 3) && ((c7 & 0x08) == 0)) 
		val256.f[7] = 0.0;
	
	return val256;
}

INLINE __m256 _mm256_permute_ps(__m256 a, int control) {
	__m256 val256;

	val256.f[0] = a.f[control & 0x03]; 
	val256.f[1] = a.f[(control >> 2) & 0x03]; 
	val256.f[2] = a.f[(control >> 4) & 0x03]; 
	val256.f[3] = a.f[(control >> 6) & 0x03]; 

	val256.f[4] = a.f[(control & 0x03) + 4]; 
	val256.f[5] = a.f[((control >> 2) & 0x03) + 4]; 
	val256.f[6] = a.f[((control >> 4) & 0x03) + 4]; 
	val256.f[7] = a.f[((control >> 6) & 0x03) + 4]; 

	return val256;
}

INLINE __m256 _mm256_permutevar_ps(__m256 a, __m256i control) {
	__m256 val256;

	val256.f[0] = a.f[control.i[0] & 0x03]; 
	val256.f[1] = a.f[control.i[4] & 0x03]; 
	val256.f[2] = a.f[control.i[8] & 0x03]; 
	val256.f[3] = a.f[control.i[12] & 0x03]; 

	val256.f[4] = a.f[(control.i[16] & 0x03) + 4]; 
	val256.f[5] = a.f[(control.i[20] & 0x03) + 4]; 
	val256.f[6] = a.f[(control.i[24] & 0x03) + 4]; 
	val256.f[7] = a.f[(control.i[28] & 0x03) + 4]; 

	return val256;
}

INLINE __m256 _mm256_shuffle_ps(__m256 a, __m256 b, const int select) {
	__m256 val256;

	val256.f[0] = a.f[select & 0x03]; 
	val256.f[1] = a.f[(select >> 2) & 0x03]; 
	val256.f[2] = b.f[(select >> 4) & 0x03]; 
	val256.f[3] = b.f[(select >> 6) & 0x03]; 

	val256.f[4] = a.f[(select & 0x03) + 4]; 
	val256.f[5] = a.f[((select >> 2) & 0x03) + 4]; 
	val256.f[6] = b.f[((select >> 4) & 0x03) + 4]; 
	val256.f[7] = b.f[((select >> 6) & 0x03) + 4]; 

	return val256;
}

INLINE __m256 _mm256_addsub_ps(__m256 a, __m256 b) {
	__m256 val256;
	val256.f[0] = a.f[0] - b.f[0];
	val256.f[1] = a.f[1] + b.f[1];
	val256.f[2] = a.f[2] - b.f[2];
	val256.f[3] = a.f[3] + b.f[3];
	val256.f[4] = a.f[4] - b.f[4];
	val256.f[5] = a.f[5] + b.f[5];
	val256.f[6] = a.f[6] - b.f[6];
	val256.f[7] = a.f[7] + b.f[7];
	return val256;
}

INLINE __m256 _mm256_insertf128_ps(__m256 a, __m128 b, int offset) {
	union {
		__m256 val256;
		__m128 val128[2];
	} val1, val2;
	
	val1.val256 = a;
	if (offset == 0) {
		val2.val128[0] = b;
		val2.val128[1] = val1.val128[1];
	}
	else {
		val2.val128[0] = val1.val128[0];
		val2.val128[1] = b;
	}
	return val2.val256;
}

INLINE __m128 _mm256_extractf128_ps(__m256 a, int offset) {
	union {
		__m256 val256;
		__m128 val128[2];
	} val;
	
	val.val256 = a;
	return val.val128[offset];
}

INLINE __m256 _mm256_maskload_ps(float *a, __m256i mask) {
	__m256 val256;

	int i; for (i=0; i<8; i++)
		val256.f[i] = mask.i[4*i+3] & 0x80 ? a[i] : 0.0;

	return val256;
}

INLINE void _mm256_maskstore_ps(float *a, __m256i mask, __m256 b) {
	int i; for (i=0; i<8; i++)
		if (mask.i[4*i+3] & 0x80) 
			a[i] = b.f[i];
}

//	FMA intrinsics __m256
INLINE __m256 _mm256_fmadd_ps(__m256 a, __m256 b, __m256 c, const int ctrl) {
	__m256 val256;
	val256.f[0] = a.f[0] * b.f[0] + c.f[0];
	val256.f[1] = a.f[1] * b.f[1] + c.f[1];
	val256.f[2] = a.f[2] * b.f[2] + c.f[2];
	val256.f[3] = a.f[3] * b.f[3] + c.f[3];
	val256.f[4] = a.f[4] * b.f[4] + c.f[4];
	val256.f[5] = a.f[5] * b.f[5] + c.f[5];
	val256.f[6] = a.f[6] * b.f[6] + c.f[6];
	val256.f[7] = a.f[7] * b.f[7] + c.f[7];
	return val256;
}

INLINE __m256 _mm256_fmaddsub_ps(__m256 a, __m256 b, __m256 c, const int ctrl) {
	__m256 val256;
	val256.f[0] = a.f[0] * b.f[0] - c.f[0];
	val256.f[1] = a.f[1] * b.f[1] + c.f[1];
	val256.f[2] = a.f[2] * b.f[2] - c.f[2];
	val256.f[3] = a.f[3] * b.f[3] + c.f[3];
	val256.f[4] = a.f[4] * b.f[4] - c.f[4];
	val256.f[5] = a.f[5] * b.f[5] + c.f[5];
	val256.f[6] = a.f[6] * b.f[6] - c.f[6];
	val256.f[7] = a.f[7] * b.f[7] + c.f[7];
	return val256;
}

INLINE __m256 _mm256_fmsubadd_ps(__m256 a, __m256 b, __m256 c, const int ctrl) {
	__m256 val256;
	val256.f[0] = a.f[0] * b.f[0] + c.f[0];
	val256.f[1] = a.f[1] * b.f[1] - c.f[1];
	val256.f[2] = a.f[2] * b.f[2] + c.f[2];
	val256.f[3] = a.f[3] * b.f[3] - c.f[3];
	val256.f[4] = a.f[4] * b.f[4] + c.f[4];
	val256.f[5] = a.f[5] * b.f[5] - c.f[5];
	val256.f[6] = a.f[6] * b.f[6] + c.f[6];
	val256.f[7] = a.f[7] * b.f[7] - c.f[7];
	return val256;
}

INLINE __m256 _mm256_fmsub_ps(__m256 a, __m256 b, __m256 c, const int ctrl) {
	__m256 val256;
	val256.f[0] = a.f[0] * b.f[0] - c.f[0];
	val256.f[1] = a.f[1] * b.f[1] - c.f[1];
	val256.f[2] = a.f[2] * b.f[2] - c.f[2];
	val256.f[3] = a.f[3] * b.f[3] - c.f[3];
	val256.f[4] = a.f[4] * b.f[4] - c.f[4];
	val256.f[5] = a.f[5] * b.f[5] - c.f[5];
	val256.f[6] = a.f[6] * b.f[6] - c.f[6];
	val256.f[7] = a.f[7] * b.f[7] - c.f[7];
	return val256;
}

INLINE __m256 _mm256_fnmadd_ps(__m256 a, __m256 b, __m256 c, const int ctrl) {
	__m256 val256;
	val256.f[0] = -(a.f[0] * b.f[0] + c.f[0]);
	val256.f[1] = -(a.f[1] * b.f[1] + c.f[1]);
	val256.f[2] = -(a.f[2] * b.f[2] + c.f[2]);
	val256.f[3] = -(a.f[3] * b.f[3] + c.f[3]);
	val256.f[4] = -(a.f[4] * b.f[4] + c.f[4]);
	val256.f[5] = -(a.f[5] * b.f[5] + c.f[5]);
	val256.f[6] = -(a.f[6] * b.f[6] + c.f[6]);
	val256.f[7] = -(a.f[7] * b.f[7] + c.f[7]);
	return val256;
}

INLINE __m256 _mm256_fnmsub_ps(__m256 a, __m256 b, __m256 c, const int ctrl) {
	__m256 val256;
	val256.f[0] = -(a.f[0] * b.f[0] - c.f[0]);
	val256.f[1] = -(a.f[1] * b.f[1] - c.f[1]);
	val256.f[2] = -(a.f[2] * b.f[2] - c.f[2]);
	val256.f[3] = -(a.f[3] * b.f[3] - c.f[3]);
	val256.f[4] = -(a.f[4] * b.f[4] - c.f[4]);
	val256.f[5] = -(a.f[5] * b.f[5] - c.f[5]);
	val256.f[6] = -(a.f[6] * b.f[6] - c.f[6]);
	val256.f[7] = -(a.f[7] * b.f[7] - c.f[7]);
	return val256;
}

//=======================================
INLINE __m256d _mm256_setzero_pd(void) {
	__m256d val256;
	int i; for (i=0; i<4; i++)
		val256.d[i] = 0;
	return val256;
}

INLINE __m256d _mm256_insertf128_pd(__m256d a, __m128d b, int offset) {
	union {
		__m256d val256;
		__m128d val128[2];
	} val1, val2;
	
	val1.val256 = a;
	if (offset == 0) {
		val2.val128[0] = b;
		val2.val128[1] = val1.val128[1];
	}
	else {
		val2.val128[0] = val1.val128[0];
		val2.val128[1] = b;
	}
	return val2.val256;
}

INLINE __m128d _mm256_extractf128_pd(__m256d a, int offset) {
	union {
		__m256d val256;
		__m128d val128[2];
	} val;
	
	val.val256 = a;
	return val.val128[offset];
}

INLINE __m256d _mm256_maskload_pd(double *a, __m256i mask) {
	__m256d val256;

	int i; for (i=0; i<4; i++)
		val256.d[i] = mask.i[8*i+7] & 0x80 ? a[i] : 0.0;

	return val256;
}

INLINE void _mm256_maskstore_pd(double *a, __m256i mask, __m256d b) {
	int i; for (i=0; i<4; i++)
		if (mask.i[8*i+7] & 0x80) 
			a[i] = b.d[i];
}

INLINE __m256i _mm256_set_epi32(unsigned __int32 i7, unsigned __int32 i6, unsigned __int32 i5, unsigned __int32 i4, 
								unsigned __int32 i3, unsigned __int32 i2, unsigned __int32 i1, unsigned __int32 i0) {
	union {
		__m256i val256;
		unsigned __int32 val32[8];
	} val;

	val.val32[0] = i0;
	val.val32[1] = i1;
	val.val32[2] = i2;
	val.val32[3] = i3;
	val.val32[4] = i4;
	val.val32[5] = i5;
	val.val32[6] = i6;
	val.val32[7] = i7;
	return val.val256;
}

INLINE __m256d _mm256_set1_pd(double d) {
	__m256d val256;
	int i; for (i=0; i<4; i++)
		val256.d[i] = d;
	return val256;
}

INLINE __m256d _mm256_set_pd(double d0, double d1, double d2, double d3) {
	__m256d val256;
	val256.d[0] = d3;
	val256.d[1] = d2;
	val256.d[2] = d1;
	val256.d[3] = d0;
	return val256;
}

INLINE __m256d _mm256_xor_pd (__m256d a, __m256d b) {
	__m256d val256;
	__int64 *ap = (__int64 *)&a, 
		*bp = (__int64 *)&b,
		*vp = (__int64 *)&val256;

	int i; for (i=0; i<4; i++)
		vp[i] = ap[i] ^ bp[i];
	
	return val256;
}

INLINE __m256d _mm256_broadcast_sd(double * a) {
	__m256d val256;
	int i; for (i=0; i<4; i++)
		val256.d[i] = *a;
	return val256;
}

INLINE __m256d _mm256_load_pd(double * p) {
	__m256d val256;
	int i; for (i=0; i<4; i++)
		val256.d[i] = p[i];
	return val256;
}

INLINE void _mm256_store_pd (double * p, __m256d a) {
	int i; for (i=0; i<4; i++)
		p[i] = a.d[i];
}

INLINE __m256d _mm256_add_pd(__m256d a, __m256d b) {
	__m256d val256;
	int i; for (i=0; i<4; i++)
		val256.d[i] = a.d[i] + b.d[i];
	return val256;
}

INLINE __m256d _mm256_sub_pd(__m256d a, __m256d b) {
	__m256d val256;
	int i; for (i=0; i<4; i++)
		val256.d[i] = a.d[i] - b.d[i];
	return val256;
}

INLINE __m256d _mm256_mul_pd(__m256d a, __m256d b) {
	__m256d val256;
	int i; for (i=0; i<4; i++)
		val256.d[i] = a.d[i] * b.d[i];
	return val256;
}

INLINE __m256d _mm256_unpacklo_pd(__m256d a, __m256d b) {
	__m256d val256;
	val256.d[0] = a.d[0];
	val256.d[1] = b.d[0];
	val256.d[2] = a.d[2];
	val256.d[3] = b.d[2];
	return val256;
}

INLINE __m256d _mm256_unpackhi_pd(__m256d a, __m256d b) {
	__m256d val256;
	val256.d[0] = a.d[1];
	val256.d[1] = b.d[1];
	val256.d[2] = a.d[3];
	val256.d[3] = b.d[3];
	return val256;
}

INLINE __m256d _mm256_permute_pd(__m256d a, int control) {
	__m256d val256;
	val256.d[0] = control & 1 ? a.d[1] : a.d[0];
	val256.d[1] = control & 2 ? a.d[1] : a.d[0];
	val256.d[2] = control & 4 ? a.d[3] : a.d[2];
	val256.d[3] = control & 8 ? a.d[3] : a.d[2];
	return val256;
}

INLINE __m256d _mm256_permutevar_pd(__m256d a, __m256i control) {
	__m256d val256;

	val256.d[0] = a.d[(control.i[0] >> 1) & 0x01]; 
	val256.d[1] = a.d[(control.i[8] >> 1) & 0x01]; 
	val256.d[2] = a.d[((control.i[16] >> 1) & 0x01) + 2]; 
	val256.d[3] = a.d[((control.i[24] >> 1) & 0x01) + 2]; 
	return val256;
}

INLINE __m256d _mm256_permute2f128_pd(__m256d a, __m256d b, int control) {
	__m256d val256;

	switch (control & 0x03) {
		case 0x00: 
			val256.d[0] = a.d[0]; 
			val256.d[1] = a.d[1];
			break;
		case 0x01: 
			val256.d[0] = a.d[2]; 
			val256.d[1] = a.d[3];
			break;
		case 0x02: 
			val256.d[0] = b.d[0]; 
			val256.d[1] = b.d[1];
			break;
		case 0x03: 
			val256.d[0] = b.d[2]; 
			val256.d[1] = b.d[3];
			break;
	}

	switch (control & 0x30) {
		case 0x00: 
			val256.d[2] = a.d[0]; 
			val256.d[3] = a.d[1];
			break;
		case 0x10: 
			val256.d[2] = a.d[2]; 
			val256.d[3] = a.d[3];
			break;
		case 0x20: 
			val256.d[2] = b.d[0]; 
			val256.d[3] = b.d[1];
			break;
		case 0x30: 
			val256.d[2] = b.d[2]; 
			val256.d[3] = b.d[3];
			break;
	}
	
	if (control & 0x08) {
		val256.d[0] = 0.0; 
		val256.d[1] = 0.0;
	}

	if (control & 0x80) {
		val256.d[2] = 0.0; 
		val256.d[3] = 0.0;
	}

	return val256;
}

INLINE __m256d _mm256_permute2_pd(__m256d a, __m256d b, __m256i control, int imm) {
	__m256d val256;
	int c0, c1, c2, c3, zero_match;

	c0 = control.i[0];
	c1 = control.i[8];
	c2 = control.i[16];
	c3 = control.i[24];
	zero_match = imm & 3;

	switch (c0 & 0x06) {
		case 0x00: 
			val256.d[0] = a.d[0]; 
			break;
		case 0x02: 
			val256.d[0] = a.d[1]; 
			break;
		case 0x04: 
			val256.d[0] = b.d[0]; 
			break;
		case 0x06: 
			val256.d[0] = b.d[1]; 
			break;
	}
	if ((zero_match == 2) && ((c0 & 0x08) == 8)) 
		val256.d[0] = 0.0;
	if ((zero_match == 3) && ((c0 & 0x08) == 0)) 
		val256.d[0] = 0.0;

	switch (c1 & 0x06) {
		case 0x00: 
			val256.d[1] = a.d[0]; 
			break;
		case 0x02: 
			val256.d[1] = a.d[1]; 
			break;
		case 0x04: 
			val256.d[1] = b.d[0]; 
			break;
		case 0x06: 
			val256.d[1] = b.d[1]; 
			break;
	}
	if ((zero_match == 2) && ((c1 & 0x08) == 8)) 
		val256.d[1] = 0.0;
	if ((zero_match == 3) && ((c1 & 0x08) == 0)) 
		val256.d[1] = 0.0;

	switch (c2 & 0x06) {
		case 0x00: 
			val256.d[2] = a.d[2]; 
			break;
		case 0x02: 
			val256.d[2] = a.d[3]; 
			break;
		case 0x04: 
			val256.d[2] = b.d[2]; 
			break;
		case 0x06: 
			val256.d[2] = b.d[3]; 
			break;
	}
	if ((zero_match == 2) && ((c2 & 0x08) == 8)) 
		val256.d[2] = 0.0;
	if ((zero_match == 3) && ((c2 & 0x08) == 0)) 
		val256.d[2] = 0.0;

	switch (c3 & 0x06) {
		case 0x00: 
			val256.d[3] = a.d[2]; 
			break;
		case 0x02: 
			val256.d[3] = a.d[3]; 
			break;
		case 0x04: 
			val256.d[3] = b.d[2]; 
			break;
		case 0x06: 
			val256.d[3] = b.d[3]; 
			break;
	}
	if ((zero_match == 2) && ((c3 & 0x08) == 8)) 
		val256.d[3] = 0.0;
	if ((zero_match == 3) && ((c3 & 0x08) == 0)) 
		val256.d[3] = 0.0;

	return val256;
}

INLINE __m256d _mm256_shuffle_pd (__m256d a, __m256d b, const int select) {
	__m256d val256;
	val256.d[0] = select & 1 ? a.d[1] : a.d[0];
	val256.d[1] = select & 2 ? b.d[1] : b.d[0];
	val256.d[2] = select & 4 ? a.d[3] : a.d[2];
	val256.d[3] = select & 8 ? b.d[3] : b.d[2];
	return val256;
}

INLINE __m256d _mm256_addsub_pd (__m256d a, __m256d b) {
	__m256d val256;
	val256.d[0] = a.d[0] - b.d[0];
	val256.d[1] = a.d[1] + b.d[1];
	val256.d[2] = a.d[2] - b.d[2];
	val256.d[3] = a.d[3] + b.d[3];
	return val256;
}

//	FMA intrinsics __m256d
INLINE __m256d _mm256_fmadd_pd(__m256d a, __m256d b, __m256d c, const int ctrl) {
	__m256d val256;
	val256.d[0] = a.d[0] * b.d[0] + c.d[0];
	val256.d[1] = a.d[1] * b.d[1] + c.d[1];
	val256.d[2] = a.d[2] * b.d[2] + c.d[2];
	val256.d[3] = a.d[3] * b.d[3] + c.d[3];
	return val256;
}

INLINE __m256d _mm256_fmaddsub_pd(__m256d a, __m256d b, __m256d c, const int ctrl) {
	__m256d val256;
	val256.d[0] = a.d[0] * b.d[0] - c.d[0];
	val256.d[1] = a.d[1] * b.d[1] + c.d[1];
	val256.d[2] = a.d[2] * b.d[2] - c.d[2];
	val256.d[3] = a.d[3] * b.d[3] + c.d[3];
	return val256;
}

INLINE __m256d _mm256_fmsubadd_pd(__m256d a, __m256d b, __m256d c, const int ctrl) {
	__m256d val256;
	val256.d[0] = a.d[0] * b.d[0] + c.d[0];
	val256.d[1] = a.d[1] * b.d[1] - c.d[1];
	val256.d[2] = a.d[2] * b.d[2] + c.d[2];
	val256.d[3] = a.d[3] * b.d[3] - c.d[3];
	return val256;
}

INLINE __m256d _mm256_fmsub_pd(__m256d a, __m256d b, __m256d c, const int ctrl) {
	__m256d val256;
	val256.d[0] = a.d[0] * b.d[0] - c.d[0];
	val256.d[1] = a.d[1] * b.d[1] - c.d[1];
	val256.d[2] = a.d[2] * b.d[2] - c.d[2];
	val256.d[3] = a.d[3] * b.d[3] - c.d[3];
	return val256;
}

INLINE __m256d _mm256_fnmadd_pd(__m256d a, __m256d b, __m256d c, const int ctrl) {
	__m256d val256;
	val256.d[0] = -(a.d[0] * b.d[0] + c.d[0]);
	val256.d[1] = -(a.d[1] * b.d[1] + c.d[1]);
	val256.d[2] = -(a.d[2] * b.d[2] + c.d[2]);
	val256.d[3] = -(a.d[3] * b.d[3] + c.d[3]);
	return val256;
}

INLINE __m256d _mm256_fnmsub_pd(__m256d a, __m256d b, __m256d c, const int ctrl) {
	__m256d val256;
	val256.d[0] = -(a.d[0] * b.d[0] - c.d[0]);
	val256.d[1] = -(a.d[1] * b.d[1] - c.d[1]);
	val256.d[2] = -(a.d[2] * b.d[2] - c.d[2]);
	val256.d[3] = -(a.d[3] * b.d[3] - c.d[3]);
	return val256;
}

//=======================================
INLINE __m256i _mm256_load_si256(__m256i *p) {
	return *p;
}

#endif