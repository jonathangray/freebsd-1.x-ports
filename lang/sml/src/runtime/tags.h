/* tags.h
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 *
 * This file has a corresponding ML structure tags embedded in structure Boot
 * in the file boot/perv.sml.
 */

#ifndef _TAGS_
#define _TAGS_

#ifndef HPPA_ASM_HACK
# ifdef HPPA
#  define HPPA_ASM_HACK 0+
# else
#  define HPPA_ASM_HACK
# endif
#endif

#define width_tags	6
#define power_tags	64
#if defined(DYNIX) && defined(ASM)
#define MAKE_TAG(t)     [[4*[t]]+2]
#else
#define MAKE_TAG(t)	HPPA_ASM_HACK((4*(t))+2)
#endif

#define HAS_LEN		0x8

#define TAG_record	MAKE_TAG(HAS_LEN+0)
#define TAG_array	MAKE_TAG(HAS_LEN+1)
#define TAG_string	MAKE_TAG(HAS_LEN+2)
#define TAG_emb_string	MAKE_TAG(HAS_LEN+3)
#define TAG_bytearray	MAKE_TAG(HAS_LEN+4)
#define TAG_realdarray	MAKE_TAG(HAS_LEN+5)
#define TAG_pair	MAKE_TAG(0)
#define TAG_reald	MAKE_TAG(1)
#define TAG_emb_reald	MAKE_TAG(2)
/* TAG_variant is 3 */
#define TAG_special	MAKE_TAG(4)
#define TAG_backptr	MAKE_TAG(5)
#define TAG_forwarded	MAKE_TAG(7)

/* make an object descriptor from the length and the tag.  This
 * uses * instead of << so it will work for assemblers.
 * An even more special version is needed for the dynix3 assembler. */
#if defined(DYNIX) && defined(ASM)
#define MAKE_DESC(l,t) [[l]*power_tags+[t]]
#else
#define MAKE_DESC(l,t) HPPA_ASM_HACK((l)*power_tags+(t))
#endif

#define DESC_reald	MAKE_DESC(2,TAG_reald)

/* If the tag is TAG_special, then the high-order part is NOT a length.
   Instead, it is:   0 = unevaluated suspension
                     1 = evaluated suspension
                     2 = weak pointer
		     3 = nulled weak pointer
*/
#define DESC_weak	MAKE_DESC(2, TAG_special)
#define DESC_null_weak	MAKE_DESC(3, TAG_special)

#endif /* !_TAGS_ */
