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
 * $NCDId: @(#)fd.h,v 1.4 1993/08/16 19:00:37 greg Exp $
 */

/* **********************************************
 *						*
 * header file file descriptor (FD) code        *
 *						*
 *	James Peterson, 1987			*
 *	(c) Copyright MCC, 1987 		*
 *						*
 ********************************************** */


/*
   the following structure remembers for each file descriptor its
   state.  In particular, we need to know if it is busy or free
   and if it is in use, by whom.
*/

typedef int FD;

struct FDDescriptor {
    Boolean     Busy;
    int         (*InputHandler) ();
};

struct FDDescriptor *FDD /* array of FD descriptors */ ;
short       MaxFD /* maximum number of FD's possible */ ;

short       nFDsInUse /* number of FD's actually in use */ ;

AuInt32        ReadDescriptors /* bit map of FD's in use -- for select  */ ;
short       HighestFD /* highest FD in use -- for select */ ;

extern void set_fd_close_on_exec();
extern void set_fd_nonblocking();
