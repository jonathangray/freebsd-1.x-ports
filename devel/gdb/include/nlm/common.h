/* NLM (NetWare Loadable Module) support for BFD.
   Copyright (C) 1993 Free Software Foundation, Inc.

   Written by Fred Fish @ Cygnus Support

This file is part of BFD, the Binary File Descriptor library.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */


/* This file is part of NLM support for BFD, and contains the portions
   that are common to both the internal and external representations. */

/* Semi-portable string concatenation in cpp.
   The NLM_CAT4 hack is to avoid a problem with some strict ANSI C
   preprocessors.  The problem is, "32_" or "64_" are not a valid
   preprocessing tokens, and we don't want extra underscores (e.g.,
   "nlm_32_").  The XNLM_CAT2 macro will cause the inner NLM_CAT macros
   to be evaluated first, producing still-valid pp-tokens.  Then the
   final concatenation can be done.  (Sigh.)  */

#ifdef SABER
#  define NLM_CAT(a,b)		a##b
#  define NLM_CAT3(a,b,c)	a##b##c
#  define NLM_CAT4(a,b,c,d)	a##b##c##d
#else
#  ifdef __STDC__
#    define NLM_CAT(a,b)	a##b
#    define NLM_CAT3(a,b,c)	a##b##c
#    define XNLM_CAT2(a,b)	NLM_CAT(a,b)
#    define NLM_CAT4(a,b,c,d)	XNLM_CAT2(NLM_CAT(a,b),NLM_CAT(c,d))
#  else
#    define NLM_CAT(a,b)	a/**/b
#    define NLM_CAT3(a,b,c)	a/**/b/**/c
#    define NLM_CAT4(a,b,c,d)	a/**/b/**/c/**/d
#  endif
#endif

/* If NLM_ARCH_SIZE is not defined, default to 32.  NLM_ARCH_SIZE is
   optionally defined by the application. */

#ifndef NLM_ARCH_SIZE
#  define NLM_ARCH_SIZE			32
#endif

#if NLM_ARCH_SIZE == 32
#  define NLM_TARGET_LONG_SIZE		4
#  define NLM_TARGET_ADDRESS_SIZE	4
#  define NLM_NAME(x,y)			NLM_CAT4(x,32,_,y)
#  define NLM_HIBIT			(((bfd_vma) 1) << 31)
#endif
#if NLM_ARCH_SIZE == 64
#  define NLM_TARGET_LONG_SIZE		8
#  define NLM_TARGET_ADDRESS_SIZE	8
#  define NLM_NAME(x,y)			NLM_CAT4(x,64,_,y)
#  define NLM_HIBIT			(((bfd_vma) 1) << 63)
#endif

#define NlmNAME(X)		NLM_NAME(Nlm,X)
#define nlmNAME(X)		NLM_NAME(nlm,X)

/* Give names to things that should not change. */

#define NLM_MAX_DESCRIPTION_LENGTH		127
#define NLM_MAX_SCREEN_NAME_LENGTH		71
#define NLM_MAX_THREAD_NAME_LENGTH		71
#define NLM_MAX_COPYRIGHT_MESSAGE_LENGTH	255
#define NLM_OTHER_DATA_LENGTH 			400		/* FIXME */
#define NLM_OLD_THREAD_NAME_LENGTH		5
#define NLM_SIGNATURE_SIZE			24
#define NLM_SIGNATURE				"NetWare Loadable Module\032"
#define NLM_HEADER_VERSION			4
#define NLM_MODULE_NAME_SIZE			14
#define NLM_DEFAULT_STACKSIZE			(8 * 1024)
