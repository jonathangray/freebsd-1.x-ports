/* ml_types.h
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 *
 * C typedefs and macros for accessing and allocating ML values in C.  The goal
 * of these macros is to isolate the C code from the representation of ML values.
 * For example, it should be possible to use either a lowest-bit 1 or lowest-bit 0
 * tagging scheme for unboxed values, without having to change C code (other than
 * this file).
 */

#ifndef _ML_TYPES_
#define _ML_TYPES_

#ifndef _ML_STATE_
#include "ml_state.h"
#endif
#ifndef _TAGS_
#include "tags.h"
#endif

/* All ML values are represented by a (32-bit) word.  A value is either a tagged
 * integer (unboxed), or a pointer to a heap object (boxed).
 */
/* typedef unsigned int *ML_val_t; */  /* defined in ml_state.h */

/* The following macros are used to convert ML and C values.
 */
#define OBJ_isBOXED(p)		(((int)(p) & 0x3) == 0)
#define INT_MLtoC(n)		(((int)(n)) >> 1)
#define INT_CtoML(n)		((ML_val_t)(((n) << 1) + 1))
#define BOOL_CtoML(x)           (INT_CtoML(x))
#define BOOL_MLtoC(x)           (INT_MLtoC(x))
#define PTR_MLtoC(p)		((int *)(p))
#define PTR_CtoML(p)		((ML_val_t)(p))

/* ML unit */
#define ML_unit			INT_CtoML(0)

/* ML booleans */
#define ML_false		INT_CtoML(0)
#define ML_true			INT_CtoML(1)

/* ML integers */
#define INT_incr(n, i)		(ML_val_t)((int)(n) + ((i) << 1))

/* ML record field selection */
#define REC_SEL(p, i)		((ML_val_t)(PTR_MLtoC(p)[(i)]))
#define REC_SELPTR(p, i)	PTR_MLtoC(REC_SEL(p, i))
#define REC_SELINT(p, i)	INT_MLtoC(REC_SEL(p, i))

/* Record allocation macros */
#define REC_ALLOC1(msp, r, a)	{					\
	register ML_val_t *__p = (ML_val_t *)((msp)->ml_allocptr);	\
	*__p++ = (ML_val_t)MAKE_DESC(1, TAG_record);			\
	*__p++ = (a);							\
	(r) = PTR_CtoML((msp)->ml_allocptr + 4);			\
	(msp)->ml_allocptr = (int)__p;					\
    }

#define REC_ALLOC2(msp, r, a, b)	{				\
	register ML_val_t *__p = (ML_val_t *)((msp)->ml_allocptr);	\
	*__p++ = (ML_val_t)MAKE_DESC(2, TAG_record);			\
	*__p++ = (a);							\
	*__p++ = (b);							\
	(r) = PTR_CtoML((msp)->ml_allocptr + 4);			\
	(msp)->ml_allocptr = (int)__p;					\
    }

#define REC_ALLOC3(msp, r, a, b, c)	{				\
	register ML_val_t *__p = (ML_val_t *)((msp)->ml_allocptr);	\
	*__p++ = (ML_val_t)MAKE_DESC(3, TAG_record);			\
	*__p++ = (a);							\
	*__p++ = (b);							\
	*__p++ = (c);							\
	(r) = PTR_CtoML((msp)->ml_allocptr + 4);			\
	(msp)->ml_allocptr = (int)__p;					\
    }

#define REC_ALLOC4(msp, r, a, b, c, d)	{				\
	register ML_val_t *__p = (ML_val_t *)((msp)->ml_allocptr);	\
	*__p++ = (ML_val_t)MAKE_DESC(4, TAG_record);			\
	*__p++ = (a);							\
	*__p++ = (b);							\
	*__p++ = (c);							\
	*__p++ = (d);							\
	(r) = PTR_CtoML((msp)->ml_allocptr + 4);			\
	(msp)->ml_allocptr = (int)__p;					\
    }

#define REC_ALLOC6(msp, r, a, b, c, d, e, f)	{			\
	register ML_val_t *__p = (ML_val_t *)((msp)->ml_allocptr);	\
	*__p++ = (ML_val_t)MAKE_DESC(6, TAG_record);			\
	*__p++ = (a);							\
	*__p++ = (b);							\
	*__p++ = (c);							\
	*__p++ = (d);							\
	*__p++ = (e);							\
	*__p++ = (f);							\
	(r) = PTR_CtoML((msp)->ml_allocptr + 4);			\
	(msp)->ml_allocptr = (int)__p;					\
    }

/* ML strings */
extern int ML_eqstr();
extern ML_val_t ML_alloc_string();

struct machineid {int d; char s[16];};

/* machine identification strings */
#define MACHINEID(id)	\
    struct machineid machine_id = { MAKE_DESC(sizeof(id)-1, TAG_string), id }

/* heap allocation macros */
#define ML_alloc_write(msp, i, x)	\
    (((int *)((msp)->ml_allocptr))[(i)] = (int)(x))
#define ML_alloc(msp, n)	(			\
    ((msp)->ml_allocptr += ((n)+1)<<2),			\
    PTR_CtoML((msp)->ml_allocptr - ((n)<<2)))

/* ML lists */
#define ML_hd(l)		REC_SEL(l, 0)
#define ML_tl(l)		REC_SEL(l, 1)
#define ML_nil			INT_CtoML(0)
#define ML_cons(msp,a,b)	(			\
    ML_alloc_write (msp, 0, MAKE_DESC(2, TAG_record)),	\
    ML_alloc_write (msp, 1, (a)),			\
    ML_alloc_write (msp, 2, (b)),			\
    ML_alloc(msp, 2))


/* ML closures */
#define CODE_ADDR(c)		((ML_val_t)REC_SELPTR(c, 0))

/* get type and length info (boxed objects only) */
#define OBJ_DESC(obj)		((int)REC_SEL(obj, -1))
#define OBJ_TAG(obj)		(OBJ_DESC(obj) & (power_tags-1))
#define OBJ_LEN(obj)		(OBJ_DESC(obj) >> width_tags)

/* the store list */
#define STORLST_obj(p)		REC_SEL(p, 0)
#define STORLST_objdesc(p)	OBJ_DESC(STORLST_obj(p))
#define STORLST_index(p)	REC_SELINT(p, 1)
#define STORLST_next(p)		REC_SEL(p, 2)
#define STORLST_nil		INT_CtoML(0)

/* the layout of datalist (mo objects) items */
#define MOLST_name(p)		REC_SEL(p, 0)
#define MOLST_closure(p)	REC_SEL(p, 1)
#define MOLST_next(p)		REC_SEL(p, 2)
#define MOLST_nil		INT_CtoML(0)

#endif /* !_ML_TYPES_ */
