#include <stdio.h>
#include <math.h>

/* We inline SPIRAL-generated-code to have singe IR file for VELLVM testing */
#include "dwmonitor.c"

#define EPS 0.00001
/* #define N 500000000 */
#define N 1
#undef CMP_VEROBSE

#ifdef CMP_VEROBSE
# include <stdio.h>
#endif /* CMP_VEROBSE */

int main()
{
    float  x[3] = { 0.92592843, 0.2884513 , 0.29302841 };
    double d[3] = { 0.50234433, 0.30505838, 0.40963706 };

    int res;
    for(int i=0;i<N;i++) /* benchmarking loop */
    {
        res = dwmonitor(x,d);
    }
#ifdef CMP_VEROBSE
    printf("RES: %d\n",res);
#endif /* CMP_VEROBSE */
    
    return res;
}
