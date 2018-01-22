#ifndef __DFT16_H__
#define __DFT16_H__

#ifdef __cplusplus
extern "C" {
#endif

    void init_dft16();
    
    void dft16(float  *Y, float  *X);
    
#ifdef __cplusplus
}
#endif

#endif
