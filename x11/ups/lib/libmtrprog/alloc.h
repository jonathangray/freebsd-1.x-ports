/* alloc.h - free list management macros */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)alloc.h	1.7 26/4/92 (UKC) */

/*  This header file defines a free list management package.
 *  The ALLOC_* macros below generate functions to allocate and
 *  free arbitrary structures.
 */

/*  Number of objects to malloc at a time.
 */
#define CHUNK	64

/*  These allocation routines use e_malloc() to allocate memory (they never 
 *  free memory). e_malloc() should have exactly the same specification
 *  as malloc(3), except that it should never return NULL. In situations
 *  where allocation has failed, the function should not return - it
 *  should exit() or abort() (or possibly longjmp()?).
 *
 *  A prototype for this function should be in scope at the point this
 *  header file is included.
 */

/*  Macro which generates a function to allocate new structures of
 *  type type called new_x, where x is whatever you specify as
 *  name. nextname must be the name of a field of type
 *  "type *".
 *
 *  The first argument is the storage class of the functions generated,
 *  which should be static or extern.
 *
 *  This implementation also generates the static global Head_free_x, where
 *  x is replaced by the value of name. This variable should not be read or set.
 *
 *  Example: given a structure "struct xxst", with a field
 *  "struct xxst *xx_next". Then the line
 *
 *  ALLOC_NEW(static, struct xxst, xx, xx_next)
 *  
 *  Would generate a function
 *
 *  static struct xxst *new_xx()
 *
 *  which returns a new structure of type xx. It would also generate
 *  the static global Head_free_xx.
 */
#define ALLOC_NEW(stclass,type,name,next) \
\
static type *CAT(Head_free_,name) = NULL; \
\
stclass type *CAT(new_,name) PROTO((void)); \
stclass type *CAT(new_,name)() \
{ \
	register type *ptr, *lim; \
\
	if (CAT(Head_free_,name) == NULL) { \
		ptr = CAT(Head_free_,name) = (type *) e_malloc(CHUNK*sizeof(type)); \
		lim = ptr + CHUNK - 1; \
		for (; ptr < lim; ptr++) \
			ptr->next = ptr + 1; \
		ptr->next = NULL; \
	} \
	ptr = CAT(Head_free_,name); \
	CAT(Head_free_,name) = ptr->next; \
	return(ptr); \
}

/*  Private macros used by the ALLOC_* macros below.
 *  Should not be used by the user.
 */
#define _ALLOC_FREE(stclass,type,name,next) \
\
stclass void CAT(free_,name) PROTO((type *ptr)); \
stclass void CAT(free_,name)(ptr) \
type *ptr; \
{ \
	ptr->next = CAT(Head_free_,name); \
	CAT(Head_free_,name) = ptr; \
}

#define _ALLOC_FREE_LIST(stclass,type,name,next) \
\
stclass void CAT3(free_,name,_list) PROTO((type *head)); \
stclass void CAT3(free_,name,_list)(head) \
type *head; \
{ \
	register type *ptr, *prev; \
\
	prev = NULL; /* to keep gcc happy */ \
	if (head != NULL) { \
		for (ptr = head; ptr != NULL; ptr = ptr->next) \
			prev = ptr; \
		prev->next = CAT(Head_free_,name); \
		CAT(Head_free_,name) = head; \
	} \
}

/*  As ALLOC_NEW above, but also generate a function free_name, which
 *  frees the strucure handed to it.
 *  Using the example above,
 *
 *  ALLOC_NEW(static, struct xxst, xx, xx_next)
 *
 *  would generate, as well as the function new_xx() described above,
 *  the function
 *
 *  static free_xx(xx)
 *  struct xxst *xx;
 *
 *  which adds the structure xx to the free list.
 */
#define ALLOC_NEW_FREE(stclass,type,name,next) \
ALLOC_NEW(stclass,type,name,next) \
_ALLOC_FREE(stclass,type,name,next)

/*  As ALLOC_FREE above, but instead of free_x, generates free_x_list.
 *  Using the same example,
 *
 *  ALLOC_NEW_FREELIST(static, struct xx, xx, xx_next)
 *
 *  would generate new_xx() and the function
 *
 *  static free_xx_list(xx)
 *  struct xxst *xx;
 *
 *  which frees the LIST of structures pointed to by xx. The list is
 *  terminated by a structure with an xx_next value of NULL (0).
 */
#define ALLOC_NEW_FREELIST(stclass,type,name,next) \
ALLOC_NEW(stclass,type,name,next) \
_ALLOC_FREE_LIST(stclass,type,name,next)

/*  Generates free_xx and free_xx_list as well as new_xx (see above)
 */
#define ALLOC_NEW_FREE_FREELIST(stclass,type,name,next) \
ALLOC_NEW(stclass,type,name,next) \
_ALLOC_FREE(stclass,type,name,next) \
_ALLOC_FREE_LIST(stclass,type,name,next)
