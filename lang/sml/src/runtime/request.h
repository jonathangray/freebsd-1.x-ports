/* request.h
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 *
 * These are the request codes for controlling the run_ml() loop.
 */

#ifndef _REQUEST_
#define _REQUEST_

#define REQ_RETURN	0
#define REQ_EXN		1
#define REQ_FAULT	2
#define REQ_GC		3
#define REQ_CALLC	4
#define REQ_SIGNAL	5
#define REQ_SIG_RETURN	6
#define REQ_SIG_RESUME	7
#define REQ_SIG_RAISE	8
#define REQ_RUN         9

#ifndef ASM
/* extern int	request; -- per thread now */
#endif

#endif /* !_REQUEST_ */

