#include <stdlib.h>
#include <string.h>
#define FNL_IMPL
#include "FastNoiseLite.h"


extern fnl_state fnlCreateState();

fnl_state* fnlCreateStatePtr() {
    fnl_state* stptr = malloc( sizeof(fnl_state) );
    fnl_state st = fnlCreateState();
    memcpy(stptr, &st, sizeof(fnl_state));
    return stptr; }

extern float fnlGetNoise2D(fnl_state *state, FNLfloat x, FNLfloat y);

extern float fnlGetNoise3D(fnl_state *state, FNLfloat x, FNLfloat y, FNLfloat z);

extern void fnlDomainWarp2D(fnl_state *state, FNLfloat *x, FNLfloat *y);

extern void fnlDomainWarp3D(fnl_state *state, FNLfloat *x, FNLfloat *y, FNLfloat *z);
