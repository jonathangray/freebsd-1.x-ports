/*
**  TRACE.H -- conditional compilation flags
**
**	Some of these may be commented out to set them to "off".
**	The comment should be removed to set them to "on".
**
**	Version:
**		@(#)trace.h	8.2	1/22/85
*/
#ifndef INGRES_TRACE_H_
#define INGRES_TRACE_H_

/* access methods compilation flags */

#define	xATR1		/* trace info, level 1 */
#define	xATR2		/* trace info, level 2, implies xTR1 */
#define	xATR3		/* trace info, level 3, implies xTR1 & xTR2 */

/* decomposition compilation flags */

#define	xDTR1		/* trace info, level 1 */
#define	xDTR2		/* trace info, level 2, implies xTR1 */
#define	xDTR3		/* trace info, level 3, implies xTR1 & xTR2 */

/* EQUEL compilation flags */

#define	xETR1		/* trace info, level 1 */
#define	xETR2		/* trace info, level 2, implies xTR1 */
#define	xETR3		/* trace info, level 3, implies xTR1 & xTR2 */

/* monitor compilation flags */

#define	xMTR1		/* trace info, level 1 */
#define	xMTR2		/* trace info, level 2, implies xTR1 */
#define	xMTR3		/* trace info, level 3, implies xTR1 & xTR2 */

/* OVQP compilatiion flags */

#define	xOTR1		/* trace info, level 1 */
#define	xOTR2		/* trace info, level 2, implies xTR1 */
#define	xOTR3		/* trace info, level 3, implies xTR1 & xTR2 */

/* parser compilation flags */

#define	xPTR1		/* trace info, level 1 */
#define	xPTR2		/* trace info, level 2, implies xTR1 */
#define	xPTR3		/* trace info, level 3, implies xTR1 & xTR2 */

/* qrymod compilation flags */

#define	xQTR1		/* trace info, level 1 */
#define	xQTR2		/* trace info, level 2, implies xTR1 */
#define	xQTR3		/* trace info, level 3, implies xTR1 & xTR2 */

/* scanner compilation flags */

#define	xSTR1		/* trace info, level 1 */
#define	xSTR2		/* trace info, level 2, implies xTR1 */
#define	xSTR3		/* trace info, level 3, implies xTR1 & xTR2 */

/* DBU compilation flags */

#define	xZTR1		/* trace info, level 1 */
#define	xZTR2		/* trace info, level 2, implies xTR1 */
#define	xZTR3		/* trace info, level 3, implies xTR1 & xTR2 */

/* support compilation flags */

#define	xTTR1		/* trace info, level 1 */
#define	xTTR2		/* trace info, level 2, implies xTR1 */
#define	xTTR3		/* trace info, level 3, implies xTR1 & xTR2 */

/*
**  Inline expansion for trace flags
*/

extern short	*tT;

#ifndef tTf
#define ABS(val)	(((val) < 0) ? -(val) : (val))
#define	tTf(a, b)	((b < 0) ? tT[a] : (tT[a] & (1 << ABS(b))))
#endif /* tTf */

#endif /* INGRES_TRACE_H_ */
