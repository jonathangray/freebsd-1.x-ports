/*-
 * Copyright (c) 1994 Westley Computing Ltd.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by Westley Computing Ltd
 * 4. Neither the name of Westley Computing Ltd nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY WESTLEY COMPUTING LTD. ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL WESTLEY COMPUTING LTD OR CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
 * THE POSSIBILITY OF SUCH DAMAGE.
 *
 *	Written by: Alistair G. Crooks, (agc@uts.amdahl.com)
 *
 *	$Id: nodbuf.h,v 1.1 1994/06/19 04:08:05 alm Exp $
 */

#ifndef	INGRES_NODBUF_H_
#define	INGRES_NODBUF_H_

/* structure that the routines use to allocate space */
typedef struct nodbuffer {
	int	nleft;			/* bytes left */
	int	err_num;		/* error code on overflow */
	int	(*err_func)(int, int);	/* error function on overflow */
	char	*xfree;			/* next free byte */
	char	buffer[1];		/*beginning of buffer area */
} nodbuf_t;

#endif /* INGRES_NODBUF_H_ */
