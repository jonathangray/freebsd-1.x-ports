/*  -*- Mode: C;  -*-
 * File: except_.h
 * Author: Heinz Schmidt (hws@ICSI.Berkeley.EDU)
 * Copyright (C) International Computer Science Institute, 1991
 *
 * COPYRIGHT NOTICE: This code is provided "AS IS" WITHOUT ANY WARRANTY
 * and is subject to the terms of the SATHER LIBRARY GENERAL PUBLIC
 * LICENSE contained in the file: "sather/doc/license.txt" of the Sather
 * distribution. The license is also available from ICSI, 1947 Center
 * St., Suite 600, Berkeley CA 94704, USA. 
 *
 * Changes: Heinz W. Schmidt (hws@csis.dit.csiro.au)
 * (c) Commonwealth Scientific and Industrial Research Organisation (CSIRO),
 * Australia, 1992, 1993.
 * The modifications are provided "AS IS" WITHOUT ANY WARRANTY and are subject
 * to the terms of the SATHER LIBRARY GENERAL PUBLIC LICENCE referred to above.
 **~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 ** FUNCTION: Exception handling based on catch and throw.
 **  This is written as a basis for several implementations including C and
 **  C++, so that the macros can be easily used where Sather is used to
 **  prototype C++ classes. Once the C++ exception handling is out there,
 **  the mechanism below can be adapted to run on top of that.
 **  Usage outside Sather see comment below.
 **
 ** RELATED PACKAGES: ./exc_test*.c
 **
 ** RCS: $Id: except_.h,v 1.1 1994/02/12 03:23:35 hsu Exp $
 ** HISTORY:
 ** Last edited: Oct 24 19:17 1993 (hws)
 **  Feb 23 13:26 1993 (hws): signal exception errors too
 **  Jun 30 23:04 1992 (hws): EH* macros add $OB catcher, improve readability
 ** Created: Sun Jun 30 15:01:52 1991 (hws)
 **~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
 */

#ifndef EXCEPT_H
#define EXCEPT_H

#include <setjmp.h>

/* from <setjmp.h>: */
/* int setjmp(env)
 *     jmp_buf env;
 *     
 * void longjmp(env, val)
 *     jmp_buf env;
 *     int val;  
 */

/*  #ifdef __STDC__
extern int setjmp(ptr env);
extern void longjmp(ptr env, int val);
    #else
extern int setjmp();
extern void longjmp();
    #endif
*/

/* 
 * One exception handler "frame" portion holds this, allocated on stack.
 * Per thread info, here global.
 */

struct EH_frame {
        jmp_buf exc_buf;               /* really: jmp_buf exc_buf */
        int     exc_type;              /* mono > 0 ; poly < 0 */
        ptr     exc_prev;
};

ptr EH_last_frame;

/* TEH1 = dynamic chaining + (pop_to_nxt_hdl_if_non_matching*; longjmp).
 *        rather than        (longjmp; pop_to_nxt_hdlr_if_non_matching)*
 * TEH2 = array of TEH1 chains, justified in process control where bad things
 * can get worse fast.
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Usage:
 * EH_CATCH(TYPE,OBJ,
 *          TRY,             
 *          HDLR);           
 * EH_THROW(TYPE,OBJ);       
 *
 * TYPE is the tag to which the exception is raised, i.e., an int code.
 *      a positive tag matches just this type.
 *      a negative tag matches this type or one of its subtypes matches.
 * OBJ is a ptr to a user defined error object (describing the error), raised
 *      by EH_THROW, and received and used in HDLR code of the EH_CATCH.
 * TRY is the normal block of code.
 * HDLR the handler block of code that takes control when an error occurs in TRY.
 *
 * The dynamic handler chain is on the stack. Each frame pointing
 * to the previous handler and holding sufficient information to find the
 * right handler without longjmp. Only when a match is found we do the
 * expensive longjmp. Note that Sather code relies on type-safety of
 * of this code. In the C or C++ use of these macros the TYPE codes must
 * be chosen appropriately to guarantee similar type safety. We suggest
 * a table of EXCEPTION TYPES and a 2D BIT ARRAY representation of the
 * subtype relation for these few types (IS_A_DES_OF_).
 */

/* To be able to abort, we need to restore the system handler, then only raise
 * the exception. The final exit should never be reached, but who knows...
 */

void eh_restore_sigquit();

#define EH_CATCH(TYPE,OBJ,TRY,HDLR)\
      { struct EH_frame exc_cur;             \
        exc_cur.exc_type=TYPE;               \
        exc_cur.exc_prev=EH_last_frame;      \
        EH_last_frame=(ptr)&exc_cur;         \
        if (!(int)(OBJ=(ptr)setjmp((&exc_cur)->exc_buf))){   \
           {TRY;}                            \
	   EH_last_frame=exc_cur.exc_prev;   \
        } else {                             \
	   EH_last_frame=exc_cur.exc_prev;   \
           {HDLR;}                           \
        }; }

/* Note: for the the C++ use you may want to use EH_THROW_TYPE to pass
 * a tag explicitly. For Sather we want to allow polymorphic throws.
 * If we cannot find a handler to catch the exception, we try to throw
 * to an no-handler-exception. Otherwise we simply signal an error
 * for sdb to catch.
 */

#define EH_THROW_TYPE(TYPE,OBJ)              \
       {struct EH_frame *exc;                \
	  int exc_TYPE;                      \
          int TP=(int)TYPE;                  \
          int OB=(int)OBJ;                   \
        for (exc=(struct EH_frame *)EH_last_frame;\
             ((int)exc)!=0;                  \
             exc=(struct EH_frame *)(exc->exc_prev)){\
	   exc_TYPE = exc->exc_type;         \
           if (EH_MATCH((TP),exc_TYPE)){   \
              longjmp(exc->exc_buf, OB);\
           };\
        };\
        if (TP == SUX_ici) {\
           EH_print_exception_error2( \
		   TP, \
		   (char *)(str_ptr_(sux22_stype_(OBJ))),\
		   OB, \
		   (char *)(str_ptr_(sux22_sdoc_(OBJ)))); \
           eh_restore_sigquit();\
	   } else {\
        for (exc=(struct EH_frame *)EH_last_frame;\
             ((int)exc)!=0;                  \
             exc=(struct EH_frame *)(exc->exc_prev)){\
	   exc_TYPE = exc->exc_type;         \
           if ( exc_TYPE == SUX_ici )\
              longjmp(exc->exc_buf, OB);\
           };\
           EH_print_exception_error1(TYPE, OB);\
           eh_restore_sigquit();\
	   }}

/* Evaluate OBJ once only! */

#define EH_THROW(OBJ)  \
        { ptr EH_OBJ = (ptr)(OBJ); \
	    { if (EH_OBJ == 0) \
		{ EH_THROW_TYPE(SUX_ici, \
				sux22_create(S_ptr_VOID_,EH_VOID_OBJ_ERR_))\
		} else { \
		  int EH_TYPE = TYPE_(EH_OBJ); \
		  if (EH_TYPE == 0) \
		    { EH_THROW_TYPE(SUX_ici, \
				sux22_create(S_ptr_VOID_,EH_VOID_TYPE_ERR_))\
		    } else { \
		      EH_THROW_TYPE(EH_TYPE,EH_OBJ); \
		    }  \
		}      \
	    }          \
	}

#define EH_MONO(TYPE) (TYPE)>0
#define EH_ALL(TYPE) (TYPE)==-OB_ici
#define EH_DES_MATCH(T,ETYP) (T)==-(ETYP)?1:(IS_A_DES_OF_(T,-(ETYP)))
#define EH_MATCH(T,ETYP) ((T)==(ETYP)?1:EH_MONO(ETYP)?0:EH_ALL(ETYP)?1:EH_DES_MATCH(T,ETYP))

#endif
/* EOF except_.h */
