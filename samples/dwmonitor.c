/*	Dynamic Window Approach Safety Monitor

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

int dwmonitor(double  *X, double  *D) {
    double q3, q4, s1, s5, s7, s8, w1;
    double s4, s6;
    s5 = 0.0;
    s8 = X[0];
    s7 = 1.0;
    for(int i5 = 0; i5 <= 2; i5++) {
        s6 = (s7*D[i5]);
        s5 = (s5 + s6);
        s7 = (s7*s8);
    }
    s1 = 0.0;
    for(int i3 = 0; i3 <= 1; i3++) {
        q3 = X[(i3 + 1)];
        q4 = X[(3 + i3)];
        w1 = (q3 - q4);
        s4 = ((((w1 >= 0))) ? (w1) : (-(w1)));
        s1 = ((((s1 >= s4))) ? (s1) : (s4));
    }
    return ((s1 >= s5));
}
