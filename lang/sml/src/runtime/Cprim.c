/* Cprim.c
 *
 * (c) Copyright 1990 Carnegie Mellon University */
 *
 * Primitives for sml2c.  Written by David Tarditi.
 */
  
/*
   Assumptions in this file.

   (1) We are using two's complement arithmetic, where the minimum
   integer is -2^31 and the maximum integer is 2^31-1 
   (2) floating point numbers are represented as doubles
   (3) A floating point number takes at most 8 bytes of storage
   (4) values of type Cint are large enough to hold pointers
   (5) arithmetic is done on integers of type Cint
*/

#include <math.h>
#include <errno.h>
#include <setjmp.h>
#include "tags.h"
#include "request.h"
#include "cause.h"
#include "ml_state.h"
#include "prim.h"

/* some header files are missing this declarations */

extern int errno;
extern ML_val_t overflow_e0[], sqrt_e0[], ln_e0[];
extern int inML,handlerPending;
extern jmp_buf top_level;

#define INT_MAX 0x7fffffff
#define INT_MIN -0x80000000

/* maximum float value for floor = 2^30, minimum float value for floor
   is 2^30 */

#define FLOOR_MAX 1073741824.0
#define FLOOR_MIN -1073741824.0
typedef int Cint;
typedef double Creal;
#define CREAL_SIZE 8

#define CRA 

#define NUMREGS 32
#ifdef CRA

#define LIMIT_PTR_REG 2
#define STORE_PTR_REG 3
#define EXN_PTR_REG 5
#define PC_REG 6
#define STANDARD_CLOSURE_REG 7
#define STANDARD_ARG_REG 8
#define STANDARD_CONT_REG 9

#define LIMIT_PTR ((Cint *)Csp[LIMIT_PTR_REG])
#define STORE_PTR (Csp[STORE_PTR_REG])
#define DATA_PTR  R4
#define EXN_PTR   (Csp[EXN_PTR_REG])
#define PC (Csp[PC_REG])
#define STANDARD_CLOSURE (Csp[STANDARD_CLOSURE_REG])
#define STANDARD_ARG (Csp[STANDARD_ARG_REG])
#define STANDARD_CONT (Csp[STANDARD_CONT_REG])

#else

#define LIMIT_PTR R2
#define STORE_PTR R3
#define DATA_PTR R4
#define EXN_PTR R5
#define PC R6
#define STANDARD_CLOSURE R7
#define STANDARD_ARG  R8
#define STANDARD_CONT R9

#endif

/* register descriptor for functions using the standard calling
   convention */

#define STDGCMASK 7
#define CLOSURE(name,func_name) int name[2] = { MAKE_DESC(1,tag_record), (int) func_name};

#define RAISE(x) \
{ MLState->ml_allocptr = (int) DATA_PTR; \
  MLState->ml_storeptr = (int) STORE_PTR; \
  MLState->ml_roots[CONT_INDX] = (ML_val_t) EXN_PTR; \
  MLState->ml_roots[ARG_INDX] = (ML_val_t) (x); \
  MLState->ml_roots[PC_INDX] = (ML_val_t) (*(int*)EXN_PTR); \
  request = REQ_RUN; \
 longjmp(top_level,1); }


#ifdef CRA
Cint Csp[NUMREGS],*R4;
#else
Cint R0, R1, *R2, R3, *R4, R5, R6, R7, R8, R9, R10, R11, R12, R13,
      R14, R15, R16, R17, R18, R19, R20, R21, R22, R23, R24, R25,
      R26, R27, R28, R29, R30, R31;
#endif

Cint *plimit;
unsigned int Cmask;

Cint sig_return_v_function()
{ request = REQ_SIG_RETURN;
  quicksave();
}

Cint sigh_resume()
{ request = REQ_SIG_RESUME;
  quicksave();
}

Cint handle_c_function()
{ request = REQ_EXN;
  quicksave();
}

Cint return_c_function()
{ request = REQ_RETURN;
   quicksave();
}

Cint callc_v_function()
{ l0: if (DATA_PTR <= plimit)
         { request = REQ_CALLC; quicksave(); }
  invoke_gc(STDGCMASK,callc_v_function);
  goto l0;
}

Cint quicksave()
{ register MLState_t *msp = MLState;
  register Cint *csp=Csp;
  inML = 0;
  msp->ml_allocptr = (int) DATA_PTR;
  msp->ml_storeptr = (int) csp[STORE_PTR_REG];
  msp->ml_roots[EXN_INDX] = (ML_val_t) csp[EXN_PTR_REG];
  msp->ml_roots[CONT_INDX] = (ML_val_t) csp[STANDARD_CONT_REG];
  msp->ml_roots[ARG_INDX] = (ML_val_t) csp[STANDARD_ARG_REG];
  longjmp(top_level,1);

 /* should never reach here */

 die("quicksave: should never reach this point!\n");

}

#ifdef CRA

static void moveregs()
{ register Cint *csp = Csp;
  register Cint *s = csp+NUMREGS;
  register MLState_t *msp = MLState;
  register ML_val_t *roots = msp->ml_roots;
  msp->ml_allocptr = (int) DATA_PTR;
  msp->ml_limitptr = (int) csp[LIMIT_PTR_REG];
  msp->ml_storeptr = (int) csp[STORE_PTR_REG];
  for (csp += 5; csp < s; *roots++ = (ML_val_t) *csp++);
}

static void fetchregs()
{ register Cint *csp = Csp;
  register Cint *s = csp+NUMREGS;
  register MLState_t *msp = MLState;
  register ML_val_t *roots = msp->ml_roots;
  register Cint limit = msp->ml_limitptr;
  DATA_PTR = (Cint *) msp->ml_allocptr;
  csp[LIMIT_PTR_REG] = limit;
  plimit = (Cint *) limit;
  csp[STORE_PTR_REG] = (Cint) msp->ml_storeptr;
  for (csp += 5; csp < s; *csp++ = (Cint) *roots++);
}

#else

/* macros assume that msp is a pointer to the ML state vector */

#define SAVE(a,b) msp->ml_roots[(a)] = (ML_val_t) (b);
#define RESTORE(a,b) (b) = (Cint) (msp->ml_roots[(a)]);

/* moveregs: move C registers to MLState vector */

static void moveregs()
{ register MLState_t *msp = MLState;
  msp->ml_allocptr = (int) DATA_PTR;
  msp->ml_limitptr = (int) LIMIT_PTR;
  msp->ml_storeptr = STORE_PTR;
  SAVE(PC_INDX,PC);
  SAVE(EXN_INDX,EXN_PTR);
  SAVE(CLOSURE_INDX,STANDARD_CLOSURE);
  SAVE(ARG_INDX,STANDARD_ARG);
  SAVE(CONT_INDX,STANDARD_CONT);
  SAVE(5, R10);
  SAVE(6, R11);
  SAVE(7, R12);
  SAVE(8, R13);
  SAVE(9, R14);
  SAVE(10, R15);
  SAVE(11, R16);
  SAVE(12, R17);
  SAVE(13, R18);
  SAVE(14, R19);
  SAVE(15, R20);
  SAVE(16, R21);
  SAVE(17, R22);
  SAVE(18, R23);
  SAVE(19, R24);
  SAVE(20, R25);
  SAVE(21, R26);
  SAVE(22, R27);
  SAVE(23, R28);
  SAVE(24, R29);
  SAVE(25, R30);
  SAVE(26, R31);
}

/* fetchregs: fetch C registers from MLState vector */
v
static void fetchregs()
{ register MLState_t *msp = MLState;
  DATA_PTR = (Cint *) msp->ml_allocptr;
  LIMIT_PTR = (Cint *) msp->ml_limitptr;
  plimit = (Cint *) LIMIT_PTR;
  STORE_PTR = msp->ml_storeptr;
  RESTORE(EXN_INDX,EXN_PTR);
  RESTORE(PC_INDX,PC);
  RESTORE(CLOSURE_INDX,STANDARD_CLOSURE);
  RESTORE(ARG_INDX,STANDARD_ARG);
  RESTORE(CONT_INDX,STANDARD_CONT);
  RESTORE(5, R10);
  RESTORE(6, R11);
  RESTORE(7, R12);
  RESTORE(8, R13);
  RESTORE(9, R14);
  RESTORE(10, R15);
  RESTORE(11, R16);
  RESTORE(12, R17);
  RESTORE(13, R18);
  RESTORE(14, R19);
  RESTORE(15, R20);
  RESTORE(16, R21);
  RESTORE(17, R22);
  RESTORE(18, R23);
  RESTORE(19, R24);
  RESTORE(20, R25);
  RESTORE(21, R26);
  RESTORE(22, R27);
  RESTORE(23, R28);
  RESTORE(24, R29);
  RESTORE(25, R30);
  RESTORE(26, R31);
}
#endif

void saveregs()
{
 inML = 0;
 moveregs();
 longjmp(top_level,1);

 /* should never reach here */

 die("saveregs: should never reach this point!\n");
}

void restoreregs()
{ extern int NumPendingSigs, maskSignals,inSigHandler,handlerPending;
  register Cint (*next)();
#ifdef CDEBUG
 register Cint (*prev)(),(*tmp)();
#endif

  fetchregs(); 
  next = (Cint (*)()) PC;

 if (NumPendingSigs && !maskSignals && !inSigHandler) {
       handlerPending = 1;
       plimit = (Cint *) 0;
  }

 inML = 1;
loop:
#ifdef CDEBUG
      tmp = (Cint  (*)()) ((*next)());
      prev = next;
      next = tmp;
      goto loop;
#else
   next = (Cint (*)()) ((*next)());
   next = (Cint (*)()) ((*next)());
   next = (Cint (*)()) ((*next)());
   next = (Cint (*)()) ((*next)());
   next = (Cint (*)()) ((*next)());
   next = (Cint (*)()) ((*next)());
   next = (Cint (*)()) ((*next)());
   next = (Cint (*)()) ((*next)());
   next = (Cint (*)()) ((*next)());
   goto loop;
#endif
}

int invoke_gc(mask,func)
unsigned int mask;
{ inML = 0;
  if (handlerPending) {
    sig_setup();
    PC = func;
    Cmask = mask;
    saveregs();
  }
  moveregs();
  callgc0(CAUSE_GC,mask);
  fetchregs();
  inML = 1;
}

int inlined_gc(mask)
unsigned int mask;
{ inML = 0;
  moveregs();
  callgc0(CAUSE_GC,mask);
  fetchregs();
  inML = 1;
}

#define ALLOC(v) (*DATA_PTR++) = ((int) (v))
#define UNTAG(v) ((v) >> 1)

Cint array_v_function()
{ register Cint *start, *finish, initial_value;
  register Cint l = UNTAG( *((Cint *) STANDARD_ARG));
  register Cint newtag = (l << width_tags) | tag_array;
l0:  if (l+DATA_PTR < plimit)
       { initial_value = *(((Cint *) STANDARD_ARG) + 1);
         ALLOC(newtag);
         STANDARD_ARG = (Cint) DATA_PTR;
         for (start = DATA_PTR, finish = start + l; start < finish;
	        *start++ = initial_value);
         DATA_PTR = finish;
         return( *((Cint *) STANDARD_CONT));
       }
  invoke_gc(STDGCMASK,array_v_function);
  goto l0;
}

Cint create_s_v_function()
{ register Cint l = UNTAG(STANDARD_ARG);
  register Cint newtag = (l << width_tags) | tag_string;

  /* # of longwords needed */

  l = (l+3) >> 2;

l0:  if (l + DATA_PTR  < plimit)
      { ALLOC(newtag);
        STANDARD_ARG = (Cint) DATA_PTR;
        DATA_PTR += l;
        return( *((Cint *) STANDARD_CONT));
      }
    invoke_gc(STDGCMASK,create_s_v_function); goto l0;
}
 
Cint create_b_v_function()
{ register Cint l = UNTAG (STANDARD_ARG);
  register Cint newtag = (l << width_tags) | tag_bytearray;


  /* # of longwords needed */

  l = (l+3) >> 2;

l0:  if (l + DATA_PTR < plimit)
      { ALLOC(newtag);
         STANDARD_ARG = (Cint) DATA_PTR;
         DATA_PTR += l;
         return( *((Cint *) STANDARD_CONT));
      }
     invoke_gc(STDGCMASK,create_b_v_function);
     goto l0;
}     

Cint logb_v_function()
{ RAISE(overflow_e0+1); }

Cint scalb_v_function()
{ RAISE(overflow_e0+1); }

Cint floor_v_function()
{ register Creal d = floor(*(double *) STANDARD_ARG);
  if (d< FLOOR_MIN || d>FLOOR_MAX) {
                RAISE(overflow_e0+1);
    }
  STANDARD_ARG = (Cint) ((Cint) d * 2 + 1) ;
  return (*(Cint *) STANDARD_CONT);
}

#define MATH_FUNC(f,name) \
Cint name() \
{ register Creal d; \
l0:  if (3 + DATA_PTR <= plimit) { \
    d = f (* ((double *) STANDARD_ARG)); \
    ALLOC(MAKE_DESC(CREAL_SIZE,tag_string)); \
    STANDARD_ARG = (Cint) DATA_PTR; \
    *(Creal *) DATA_PTR = d; \
    DATA_PTR = (Cint *) ((char *) DATA_PTR+CREAL_SIZE); \
    return (*((Cint *) STANDARD_CONT)); \
  } \
  invoke_gc(STDGCMASK,name); \
  goto l0; \
} \

/* possible portability problem here; errno isn't reset by some math functions
   and edition 2 of Kernighan and Ritchie says nothing about resetting
   errno.  So we're resetting it ourselves here.  This could be wrong for
   other OS/local environments. */

#define MATH_FUNC_WITH_ERR(f,name,err) \
Cint name() \
{ register Creal d; \
l0:  if (3 + DATA_PTR <= plimit) { \
    d = f (* ((double *) STANDARD_ARG)); \
    ALLOC(MAKE_DESC(CREAL_SIZE,tag_string)); \
    STANDARD_ARG = (Cint) DATA_PTR; \
    *(double *) DATA_PTR = d; \
    DATA_PTR = (Cint *) ((char *) DATA_PTR + CREAL_SIZE); \
    if ((errno == EDOM) || (errno == ERANGE)) {errno = -1; RAISE(err); } \
    return (*((Cint *) STANDARD_CONT)); \
  } \
  else { invoke_gc(STDGCMASK,name); goto l0; } \
} \

MATH_FUNC(sin, sin_v_function)
MATH_FUNC(cos,  cos_v_function)
MATH_FUNC(atan, arctan_v_function)

MATH_FUNC_WITH_ERR(exp,  exp_v_function, overflow_e0+1)
MATH_FUNC_WITH_ERR(log, ln_v_function, ln_e0+1)
MATH_FUNC_WITH_ERR(sqrt, sqrt_v_function, sqrt_e0+1)

int startptr;

CLOSURE(sigh_return_c,sig_return_v_function)
CLOSURE(handle_c,handle_c_function)
CLOSURE(return_c,return_c_function)
CLOSURE(callc_v,callc_v_function)
CLOSURE(array_v,array_v_function)
CLOSURE(create_b_v,create_b_v_function)
CLOSURE(create_s_v,create_s_v_function)
CLOSURE(arctan_v,arctan_v_function)
CLOSURE(cos_v,cos_v_function)
CLOSURE(exp_v,exp_v_function)
CLOSURE(floor_v,floor_v_function)
CLOSURE(ln_v,ln_v_function)
CLOSURE(sin_v,sin_v_function)
CLOSURE(sqrt_v,sqrt_v_function)
CLOSURE(logb_v,logb_v_function)
CLOSURE(scalb_v,scalb_v_function)

/* multiplication with overflow checking.
   We break the operands into 16-bit parts, multiply them, and put them
   back together.
*/

/* overflow check for unsigned integer addition, where a and b are the
   msb of the operands:

        a b r | ov
        ----------
        0 0 0   0
        0 0 1   0
        0 1 0   1
        0 1 1   0
        1 0 0   1
        1 0 1   0
        1 1 0   1
        1 1 1   1

    Overflow = and(a,b)|((eor(a,b)&(~r)))
*/

#define NO_OVERFLOW(a,b,r) (((a&b)|((a^b)&(~r)))>=0)
#define WORD_SIZE 16
#define LONG_WORD_SIZE 32
#define POW2(x) (1<<x)

/* mult: multiply two two's complement numbers, raise exception
   if an overflow occurs. */

int mult(b,d)
register unsigned int b,d;
{ register unsigned int a,c;
  register int sign = b^d;

/* break b and d into hi/lo words 

      -------------   ---------
      |  a  |  b  |  |c   |  d|
      -------------  ---------
*/

  if ((int)b<0) {b = -(int)b; }
  if ((int)d<0) {d = -(int)d; }
  a = b >> WORD_SIZE;
  b = b & (POW2(WORD_SIZE)-1);
  c = d >> WORD_SIZE;
  d = d & (POW2(WORD_SIZE)-1);
  if (a&c) goto overflow;
  a = a*d;
  c = c*b;
  b = b*d;
  if (a<(POW2(LONG_WORD_SIZE-WORD_SIZE)) &&
      c<(POW2(LONG_WORD_SIZE-WORD_SIZE)))
    { d = a+c;
      if (d<(POW2(LONG_WORD_SIZE-WORD_SIZE)))
          { d <<= WORD_SIZE;
            a=d+b;
           if NO_OVERFLOW(d,b,a)
	       if (sign<0)
		  if (a<=POW2(LONG_WORD_SIZE-1))
                      return (-a);
		  else goto overflow;
	       else if (a<(POW2(LONG_WORD_SIZE-1))) return (a);
	  }
    }
 overflow:
#ifdef DEBUG
      printf("overflow occurred\n");
#endif
  inML = 0;
  RAISE (overflow_e0+1)
}

