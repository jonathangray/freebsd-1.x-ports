/********************************************************************
 * lindner
 * 3.2
 * 1993/03/26 19:50:41
 * /home/mudhoney/GopherSrc/CVS/gopher+/object/BLblock.h,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992,1993 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: BLblock.h
 * Header file and abstraction of a gopher+ block
 *********************************************************************
 * Revision History:
 * BLblock.h,v
 * Revision 3.2  1993/03/26  19:50:41  lindner
 * Mitra fixes for better/clearer fromNet code
 *
 * Revision 3.1.1.1  1993/02/11  18:03:06  lindner
 * Gopher+1.2beta release
 *
 * Revision 1.1  1993/01/31  00:31:12  lindner
 * Initial revision
 *
 *
 *********************************************************************/


#ifndef BLBLOCK_H
#define BLBLOCK_H

#include "STRstring.h"
#include "STAarray.h"
#include "boolean.h"


/** Return Values for *fromNet() functions **/
#define SOFTERROR -1
#define HARDERROR -2
#define MORECOMING 1
#define FOUNDEOF 0


typedef struct block_struct Blockobj;
typedef DynArray BlockArray;
#include "GSgopherobj.h"


/** The different types of blocks **/
#define BLOCK_UNKNOWN  0
#define BLOCK_VIEW     1
#define BLOCK_ASK      2
#define BLOCK_ABSTRACT 3
#define BLOCK_ADMIN    4

typedef int BlockType;


/** The block data is a union, it can either be a filename or the
 ** actual data in a STRarray, or a gopher reference.
 **/

union BlockData_union {
     String      *filename;
     StrArray    *text;
     GopherObj   *gs;
};

typedef union BlockData_union BlockData;



#define BDATA_NONE 0
#define BDATA_FILE 1
#define BDATA_TEXT 2
#define BDATA_GREF 3
typedef int BlockDataType;


struct block_struct
{
     BlockType     btype;
     String        *Blockname;
     BlockDataType datatype;
     BlockData     data;
};


/****** Macros/data access ********/
#define BLgetName(a)        (STRget((a)->Blockname))
#define BLsetName(a,b)      (STRset((a)->Blockname,(b)))

#define BLgetBlocktype(a)   (STRget((a)->btype))
#define BLsetBlocktype(a,b) (STRset((a)->btype,(b)))

#define BLgetDatatype(a)    ((a)->datatype)

/**** Prototype declarations. ****/
Blockobj *BLnew();
void      BLdestroy();
void      BLinit();
void      BLcpy();
void      BLsetFile();
void      BLsetGref();
void      BLsetText();
char *    BLgetLine();

/*************************************************************
 ** Define a dynamic block array
 **/

#include "DAarray.h"

#define BLAnew(a)       (DAnew((a),BLnew,BLinit,BLdestroy,BLcpy))
#define BLAinit(a)       (DAinit((a)))
#define BLAgetTop(a)     (DAgetTop(a))
#define BLAgetEntry(a,b) (Blockobj*)(DAgetEntry(a,b))
#define BLApush(a,b)     (DApush((DynArray*)(a),(b)))
#define BLAdestroy(a)    (DAdestroy(a))
#define BLAcpy(a,b)      (DAcpy(a,b))

#endif
