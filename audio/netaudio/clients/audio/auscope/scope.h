/*
 * Copyright 1993 Network Computing Devices, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and
 * its documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name Network Computing Devices, Inc. not be
 * used in advertising or publicity pertaining to distribution of this 
 * software without specific, written prior permission.
 * 
 * THIS SOFTWARE IS PROVIDED `AS-IS'.  NETWORK COMPUTING DEVICES, INC.,
 * DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING WITHOUT
 * LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NONINFRINGEMENT.  IN NO EVENT SHALL NETWORK
 * COMPUTING DEVICES, INC., BE LIABLE FOR ANY DAMAGES WHATSOEVER, INCLUDING
 * SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES, INCLUDING LOSS OF USE, DATA,
 * OR PROFITS, EVEN IF ADVISED OF THE POSSIBILITY THEREOF, AND REGARDLESS OF
 * WHETHER IN AN ACTION IN CONTRACT, TORT OR NEGLIGENCE, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 * 
 * $NCDId: @(#)scope.h,v 1.5 1993/09/01 22:16:48 greg Exp $
 */

/* **********************************************
 *						*
 * header file for the Server spy scope           *
 *						*
 *	James Peterson, 1987			*
 *	(c) Copyright MCC, 1987 		*
 *						*
 ********************************************** */

#include <stdio.h>
#define	AMD_H	/* don't redefine all the types */
/* all we want are constants */
#define	NO_AUDIO_REQUESTS
#define	NO_AUDIO_REPLIES
#define	NO_AUDIO_EVENTS

#ifndef __alpha
#define INT32		long
#define CARD32		unsigned long
#else /* __alpha */
#define INT32		int
#define CARD32		unsigned int
#endif /* __alpha */

#define INT16		short
#define CARD16		unsigned short
#define INT8		char
#define CARD8		unsigned char

#include	<audio/audio.h>
#include	<audio/Aproto.h>

#undef INT32
#undef CARD32
#undef INT16
#undef CARD16
#undef INT8
#undef CARD8

#define Boolean short
#define true 1
#define false 0

/* ********************************************** */
/*                                                */
/* ********************************************** */

#define Assert(b) (b)
#define debug(n,f) (void)((debuglevel & n) ? (fprintf f,fflush(stderr)) : 0)
short       debuglevel;

/* ********************************************** */
/*                                                */
/* ********************************************** */

short       Verbose /* quiet (0) or increasingly verbose  ( > 0) */ ;


int         ScopePort;
char       *ScopeHost;

/* external function type declarations */

extern char *malloc();
extern char *strcpy();
char       *ClientName();

/* ********************************************** */
/*                                                */
/* ********************************************** */

/* need to change the MaxFD to allow larger number of fd's */
#define StaticMaxFD 64

int         silent;

#include "fd.h"
