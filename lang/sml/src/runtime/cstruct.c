/* cstruct.c
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 */

#ifndef THINK_C
#include <sys/param.h>  /* for NOFILE */
#else
#define NOFILE 69
#endif
#include "tags.h"
#include "ml_types.h"
#include "prim.h"

#ifdef __STDC__
#define CONCAT(ex,suffix)	ex ## suffix
#else
#define CONCAT(ex,suffix)	ex/**/suffix
#endif

#ifndef THINK_C

/* Exceptions are identified by (string ref) values */
#define ML_EXNID(ex,name)				\
    struct {						\
	int  tag;					\
	char s[(sizeof(name)+3) & ~3];			\
    } CONCAT(ex,_s) =					\
    {MAKE_DESC(sizeof(name)-1, TAG_string), name};	\
    int CONCAT(ex,_id0)[2] = {				\
	MAKE_DESC(1, TAG_array),			\
	(int)PTR_CtoML(CONCAT(ex,_s).s)}

/* A nullary exception is represented by an exn packet */
#define ML_EXN(ex,name)					\
    ML_EXNID(ex,name);					\
    int CONCAT(ex,_e0)[3] = {				\
	MAKE_DESC(2, TAG_record),			\
	(int)PTR_CtoML(CONCAT(ex,_id0)+1),		\
	(int)ML_unit}


#else

/* THINK_C v5.0.1 cannot deal with the PTR_CtoML line of each of
 * the previous two macros when expanded.  The error it gives
 * is: "could not evaluate constant expression"
 * So I've modified the macros and added the function
 * thinkc_workaround() at the end of this file to patch things
 * up; it must be called from main() at startup.            -- 4Dec92  e  */


/* Exceptions are identified by (string ref) values */
#define ML_EXNID(ex,name)				\
    struct {						\
	int  tag;					\
	char s[(sizeof(name)+3) & ~3];			\
    } CONCAT(ex,_s) =					\
    {MAKE_DESC(sizeof(name)-1, TAG_string), name};	\
    int CONCAT(ex,_id0)[2] = {				\
	MAKE_DESC(1, TAG_array),			\
	(int)PTR_CtoML(0/*CONCAT(ex,_s).s*/)}

/* A nullary exception is represented by an exn packet */
#define ML_EXN(ex,name)					\
    ML_EXNID(ex,name);					\
    int CONCAT(ex,_e0)[3] = {				\
	MAKE_DESC(2, TAG_record),			\
	(int)PTR_CtoML(0/*CONCAT(ex,_id0)+1*/),		\
	(int)ML_unit}


#endif

#ifdef THINK_C
#define ASM_CLOSURE(name)						\
    extern int CONCAT(name,_a)();					\
    ML_val_t CONCAT(name,_v)[2] = {(ML_val_t)MAKE_DESC(1,TAG_record),	\
				   PTR_CtoML(CONCAT(name,_a))}
#else
#define ASM_CLOSURE(name)						\
    extern ML_val_t CONCAT(name,_a)[];					\
    ML_val_t CONCAT(name,_v)[2] = {(ML_val_t)MAKE_DESC(1,TAG_record),	\
				   PTR_CtoML(CONCAT(name,_a))}
#endif

#if (CALLEESAVE > 0)
#ifdef THINK_C
#define ASM_CONT(name) 							\
    extern int CONCAT(name,_a)();					\
    ML_val_t * CONCAT(name,_c) = (ML_val_t *)(CONCAT(name,_a))

#else
#define ASM_CONT(name) 							\
    extern ML_val_t CONCAT(name,_a)[];					\
    ML_val_t * CONCAT(name,_c) = (ML_val_t *)(CONCAT(name,_a))

#endif
#else
#define ASM_CONT(name)							\
    ASM_CLOSURE(name);							\
    ML_val_t * CONCAT(name,_c) = (ML_val_t *)(CONCAT(name,_v)+1)
#endif

ASM_CLOSURE(array);
ASM_CLOSURE(callc);
ASM_CLOSURE(create_b);
ASM_CLOSURE(create_r);
ASM_CLOSURE(create_s);
ASM_CLOSURE(create_v);
ASM_CLOSURE(floor);
ASM_CLOSURE(logb);
ASM_CLOSURE(scalb);
ASM_CLOSURE(try_lock);
ASM_CLOSURE(unlock);
ASM_CLOSURE(handle);

ASM_CONT(return);
ASM_CONT(sigh_return);

#define RUNVEC_SZ	12
ML_val_t runvec[RUNVEC_SZ+1] = {
    (ML_val_t)MAKE_DESC(RUNVEC_SZ, TAG_record),
    PTR_CtoML(array_v+1),
    PTR_CtoML(callc_v+1),
    PTR_CtoML(create_b_v+1),
    PTR_CtoML(create_r_v+1),
    PTR_CtoML(create_s_v+1),
    PTR_CtoML(create_v_v+1),
    PTR_CtoML(floor_v+1),
    PTR_CtoML(logb_v+1),
    PTR_CtoML(scalb_v+1),
    PTR_CtoML(try_lock_v+1),
    PTR_CtoML(unlock_v+1),
};

				/* aggregate structures of length zero */
int array0_v[]      = {MAKE_DESC(0,TAG_array)};
int bytearray0_v[]  = {MAKE_DESC(0,TAG_bytearray)};
int realarray0_v[]  = {MAKE_DESC(0,TAG_realdarray)};
int vector0_v[]     = {MAKE_DESC(0,TAG_array)};

ML_EXN(div,"Div");
ML_EXN(overflow,"Overflow");
ML_EXN(unboundTable,"UnboundTable");
ML_EXNID(syserror,"SysError");

extern int active_procs0[];
extern int collected0[];
extern int collectedfrom0[];
extern int times0[];
extern int current0[];
extern int gcmessages0[];
extern int majorcollections0[];
extern int minorcollections0[];
extern int pstruct0[];
extern int ratio0[];
extern int softmax0[];
extern int lastratio0[];
extern int sighandler0[];
extern int errstrings[];
extern int externlist0[];


#define CSTRUCT_SZ	28

#ifdef M68
MACHINEID("m68");
#endif
#ifdef C
MACHINEID("ansi-c");
#endif
#ifdef MIPSEL
MACHINEID("mipsl");
#endif
#ifdef MIPSEB
MACHINEID("mipsb");
#endif
#ifdef SPARC
MACHINEID("sparc");
#endif
#ifdef VAX
MACHINEID("vax");
#endif
#ifdef RS6000
MACHINEID("rs6000");
#endif
#ifdef I386
MACHINEID("i386");
#endif
#ifdef HPPA
MACHINEID("hppa");
#endif

ML_val_t cstruct[CSTRUCT_SZ+1] = {
    (ML_val_t)MAKE_DESC(CSTRUCT_SZ, TAG_record),
    PTR_CtoML(runvec+1),
    PTR_CtoML(div_e0+1),
    PTR_CtoML(overflow_e0+1),   		
    PTR_CtoML(syserror_id0+1),
    PTR_CtoML(unboundTable_e0+1),
    PTR_CtoML(active_procs0+1),
    PTR_CtoML(array0_v+1),
    PTR_CtoML(bytearray0_v+1),
    INT_CtoML(CALLEESAVE),
    PTR_CtoML(collected0+1),
    PTR_CtoML(collectedfrom0+1),
    PTR_CtoML(current0+1),
    PTR_CtoML(datalist+1),
    INT_CtoML(NOFILE),
    PTR_CtoML(externlist0+1),
    PTR_CtoML(gcmessages0+1),
    PTR_CtoML(times0+1),
    PTR_CtoML(lastratio0+1),
    PTR_CtoML(machine_id.s),
    PTR_CtoML(majorcollections0+1),
    PTR_CtoML(minorcollections0+1),
#if defined(V9) || defined(HPUX) || defined(RISCos) || defined(SGI) || defined(AUX)
    INT_CtoML(3),
#else
#  ifdef VAX
    INT_CtoML(1),
#  else
    INT_CtoML(2),
#  endif
#endif
    PTR_CtoML(pstruct0+1),
    PTR_CtoML(ratio0+1),
    PTR_CtoML(realarray0_v+1),
    PTR_CtoML(sighandler0+1),
    PTR_CtoML(softmax0+1),
    PTR_CtoML(vector0_v+1),
};

#if defined(M68)
  ASM_CLOSURE(arctan);
  ASM_CLOSURE(cos);
  ASM_CLOSURE(exp);
  ASM_CLOSURE(ln);
  ASM_CLOSURE(sin);
  ASM_CLOSURE(sqrt);
#endif

#if defined(M68) || defined(C)
ML_EXN(ln,"Ln");
ML_EXN(sqrt,"Sqrt");

#define MATHVEC_SZ	9
ML_val_t mathvec[MATHVEC_SZ+1] = {
    (ML_val_t)MAKE_DESC(MATHVEC_SZ, TAG_record),
    PTR_CtoML(ln_e0+1),
    PTR_CtoML(sqrt_e0+1),
    PTR_CtoML(arctan_v+1),
    PTR_CtoML(cos_v+1),
    PTR_CtoML(exp_v+1),
    PTR_CtoML(ln_v+1),
    PTR_CtoML(sin_v+1),
    PTR_CtoML(sqrt_v+1),
};
#endif

#ifdef THINK_C
void
thinkc_workaround(void)	{
	/*
	xxx_id0[1] = (int)PTR_CtoML(xxx_s.s);
	xxx_e0[1]  = (int)PTR_CtoML(xxx_id0+1);
	*/
	syserror_id0[1]     = (int)PTR_CtoML(syserror_s.s);
	div_id0[1]          = (int)PTR_CtoML(div_s.s);
	div_e0[1]           = (int)PTR_CtoML(div_id0+1);
	overflow_id0[1]     = (int)PTR_CtoML(overflow_s.s);
	overflow_e0[1]      = (int)PTR_CtoML(overflow_id0+1);
	unboundTable_id0[1] = (int)PTR_CtoML(unboundTable_s.s);
	unboundTable_e0[1]  = (int)PTR_CtoML(unboundTable_id0+1);
	ln_id0[1]           = (int)PTR_CtoML(ln_s.s);
	ln_e0[1]            = (int)PTR_CtoML(ln_id0+1);
	sqrt_id0[1]         = (int)PTR_CtoML(sqrt_s.s);
	sqrt_e0[1]          = (int)PTR_CtoML(sqrt_id0+1);
}
#endif
