/* prim.h
 * Created 7/15/90 by David Tarditi for ML->C
 *
 * Define types of assembly language or C values to be used directly by
 * ML.
 *
 */

#include "ml_state.h"

#ifndef _PRIM_
#define _PRIM_
extern int datalist[];
extern ML_val_t array_v[];
extern ML_val_t callc_v[];
extern ML_val_t create_b_v[];
extern ML_val_t create_r_v[];
extern ML_val_t create_s_v[];
extern ML_val_t create_v_v[];
extern ML_val_t floor_v[];
extern ML_val_t logb_v[];
extern ML_val_t scalb_v[];
extern ML_val_t try_lock_v[];
extern ML_val_t unlock_v[];
extern ML_val_t handle_v[];
extern ML_val_t *return_c;
extern ML_val_t *sigh_return_c;

extern int masksigs_v[];

#ifdef C
extern int sigh_resume();
extern unsigned int Cmask;
extern int *plimit;
#else
extern int sigh_resume[];
#endif

extern ML_val_t arctan_v[],cos_v[],exp_v[],ln_v[],sin_v[],sqrt_v[];
#endif /* !_PRIM_ */
