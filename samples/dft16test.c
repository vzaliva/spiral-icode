#include <math.h>

/* We inline SPIRAL-generated-code to have singe IR file for VELLVM testing */
#include "dft16.c"

#define EPS 0.00001
/* #define N 500000000 */
#define N 1
#undef CMP_VEROBSE

#ifdef CMP_VEROBSE
# include <stdio.h>
#endif /* CMP_VEROBSE */

int main()
{
    float x[32] = {
        0.92592843, 0.2884513 , 0.29302841, 0.59476958,
        0.50234433, 0.30505838, 0.40963706, 0.49709958,
        0.60914499, 0.06590301, 0.824081  , 0.63956507,
        0.76967576, 0.49243068, 0.65559785, 0.21940147,
        0.30151457, 0.44974889, 0.16478086, 0.22746391,
        0.14799808, 0.90849232, 0.56751938, 0.63717726,
        0.17912022, 0.85561443, 0.61403273, 0.93086884,
        0.03174374, 0.77083716, 0.63307604, 0.56894789
    };
    float y_expected[32] = {
        7.629223, 8.451830, 2.115070, 1.637304,
        0.300441, -2.021516, 0.928387, 0.664806,
        0.093905, -1.187008, 0.499933, -0.289084,
        1.146814, -0.172031, -0.625218, -2.207773,
        -0.694283, -0.178757, 1.417819, -0.014793,
        0.677349, 1.352727, 0.530601, 0.156922,
        1.033987, -0.447194, 1.623679, -0.258518,
        -0.367893, 0.107551, -1.494961, -0.979244
    };
    float y[32];
    
    for(int i=0;i<32;i++)
        y[i]=0;
    
    init_dft16();

    for(int i=0;i<N;i++) /* benchmarking loop */
    {
        dft16(y, x);
    }
    
    for(int i=0;i<32;i++)
    {
        if(fabs(y[i]-y_expected[i])>EPS)
        {
#ifdef CMP_VEROBSE
            printf("Got %f instead of %f at %d\n",y[i],y_expected[i],i);
#endif /* CMP_VEROBSE */
            return i+1;
        }
    }

    return 0;
}
