/*  -*- Mode: C;  -*-
 * File: sather/sys/C/deep_save_.c
 * Author: Jeff Bilmes
 * Copyright (C) International Computer Science Institute, 1991, 1992, 1993 

 * COPYRIGHT NOTICE: This code is provided "AS IS" WITHOUT ANY WARRANTY
 * and is subject to the terms of the SATHER LIBRARY GENERAL PUBLIC
 * LICENSE contained in the file: "sather/doc/license.txt" of the Sather
 * distribution. The license is also available from ICSI, 1947 Center
 * St., Suite 600, Berkeley CA 94704, USA.
 *
 * Changes: Heinz W. Schmidt (hws@csis.dit.csiro.au)
 *          Oscar Bosman (oscar@csis.dit.csiro.au)
 * (c) Commonwealth Scientific and Industrial Research Organisation (CSIRO),
 * Australia, 1992, 1993.
 *
 * The modifications are provided "AS IS" WITHOUT ANY WARRANTY and are subject
 * to the terms of the SATHER LIBRARY GENERAL PUBLIC LICENCE referred to above.
 **~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 ** FUNCTION:   Saving and restoring all objects reachable from a given object
 **             in a disk file.
 **
 ** RCS: $Id: deep_save_.c,v 1.1 1994/02/12 03:23:35 hsu Exp $
 ** HISTORY:
 ** Last edited: Oct 24 19:22 1993 (hws)
 **  Oct  8 22:55 1993 (hws): cast lseek arg2 for SGI port
 **  Aug 30 15:37 1993 (oscar): added #include <stdio.h>
 ** Created: Wed May 22 14:05:57 1991 (bilmes)
 *----------------------------------------------------------------------
  Modified/Debugged: Chu-Cheow Lim
  Date: 9 Jan 1991
  -- Extend code to handle arrays.
  -- Alter documentation.
  -- Added interface routines for persistent objects with version.
----------------------------------------------------------------------*/
/*
** deep_save_/deep_restore_:
**   Note that these routines are not reentrant.
**   Uses the deep copy hash routines written in Sather.
**   Use read()/write()/lseek() over mmap since it is more portable
**   and since lseek() doesn't switch to kernel mode (it only changes a per
**   process variable).
**   
*/

#include <sys/types.h>
#if defined(mips) && defined(SYSTYPE_BSD43)
#  include <sysv/unistd.h>
#else
#  if defined(NeXT)
/* #    include "unistd.h" */
#  else
#    include <unistd.h>
#  endif
#endif

#include "all_.h"
#include <stdio.h>

#ifdef sequent
#define SEEK_SET 0
#define SEEK_CUR 1
#endif

/*
** "filepos" always contains the location at which we may write
** the next object
*/
static int filepos;

/*----------------------------------------------------------------------
  Persistent objects with versioning
----------------------------------------------------------------------*/
void deep_save_();
void deep_restore_();

void deep_save_vn_(p,fd,v)
ptr p;
int fd,v;
{
  write(fd,&v,sizeof(v));
  deep_save_(p,fd);
}

void deep_restore_vn_(pp,fd,v)
ptr *pp;
int fd;
int v;
{
  int oldv;
  read(fd,&oldv,sizeof(oldv));
  if (oldv != v) {
    *pp = NULL; return;
  }
  else {
    deep_restore_(pp,fd);
  }
}

/*
**    Save p, and all objects reachable by p to the file 'file'.
**    Move fd's file pointer by the size of all objects reachable by p.
*/
void deep_save_(p,fd)
    ptr p;
    int fd;
{

   if (p == NULL) {
      /* Null object is represented by 0 word in file */
	write(fd,&p,sizeof(p));
	return;
    }

    filepos = lseek(fd,0l,SEEK_CUR); /* Get current file position */
    dcHashInit_(0);
    deepSaveDoit_(p,fd);
    lseek(fd,filepos,SEEK_SET);

}


int deepSaveDoit_(p,fd)
    ptr p;
    int fd;
{
  int ploc;   /* Location of "p" stored on disk */
  int type_p; /* Type of "p" */
  int i;    
  ptr *ptrp;  /* Pointer to a "ptr" in "p" */
  int pploc;  /* File location of "*ptrp" */
  int floc;   /* Location in file of object that "*ptrp" refers to */
  int aDim;   /* Array dimension of "p" */
  int baseSize; /* Size of object "p" excluding it array elements */
  int elements; /* Number of elements in an array */
  int offsets;	/* Number of offset indices */
  int first_off; /* Offset to the first array element */
  
  /* Move to position "filepos" in file; save the location where p */
  /* will be stored in "ploc" */
  ploc = lseek(fd,filepos,SEEK_SET);
#ifdef PERSIST_TRACE_
  printf("Storing object (type = %d) at %d\n", TYPE_(p), filepos);
#endif

  write(fd,p,ob_size_(p));
  /* Set "filepos" to the location in the file where the next object may be 
     stored. */
  filepos = lseek(fd,0l,SEEK_CUR); /* get cfp */
  
  /* Contents of hash is the location of "p" in the file */
  dcHashInsert_(0,p,(ptr)ploc);
  type_p = TYPE_(p);
  
  for (i=0;i<ob_attr_num_(type_p);i++) {
    if (ob_attr_ctype_(type_p,i) == CTYPE_PTR_) {
      
      ptrp = (ptr *)(& ((char*)p)[ob_attr_offset_(type_p,i)]);
      if (*ptrp == NULL)
	continue;
      
      /* Get file location of where object referred to by "*ptrp" is 
	 stored */
      if ((floc = dcHashGet_(0,*ptrp)) == NULL)
	floc = deepSaveDoit_(*ptrp,fd);
      
      /* Write location back out to the file */
      pploc = ploc + ob_attr_offset_(type_p,i);
#ifdef PERSIST_TRACE_
      printf("storing i(=%d)th attribute; ploc = %d; pploc = %d\n", i, ploc, 
	     pploc);
      printf("   ptrp = %d; (*ptrp) = %d\n", ptrp, (*ptrp));
#endif
      lseek(fd,pploc,SEEK_SET);
      write(fd,&floc,sizeof(floc));
    }
  }
  
  if ((aDim = ob_arr_dim_(type_p)) > 0) {

    if (aDim <= 4) {
#ifdef PERSIST_TRACE_
      printf("Storing array object:\n");
#endif

      if (ob_arr_ctype_(type_p) == CTYPE_PTR_) {
	baseSize = ob_base_size_(type_p);
	/* Note: We divide the base size by size of integer to access 
	   an integer. */
#ifdef PERSIST_DEBUG_
	if ((baseSize % sizeof(int)) != 0) {
	  perror("Compiler generated non-4-based object size"); exit(1);
	}
#endif
	elements = 1;
	for (i=0; i<aDim; i++) {
	  elements *= ((int *)p)[(baseSize/sizeof(int)) + i];
	}
	  
	/* Number of index offsets in array */
	if (aDim == 1)
	  offsets = 0;
	else {
	  offsets = 1;
	  for (i=0; i<(aDim-1); i++) {
	    offsets *= ((int *)p)[(baseSize/sizeof(int)) + i];
	  }
	}
	
	first_off = baseSize + (aDim * sizeof(int)) + (offsets * sizeof(int));
#ifdef PERSIST_TRACE_
	printf("no of elements = %d; first_off = %d\n", elements, first_off);
	printf("stored location = %d\n", floc);
#endif
	for (i=0;i<elements;i++) {
	  ptrp = (ptr *)(& ((char *)p)[first_off + (i * sizeof(ptr))]);
	  if (*ptrp == NULL)
	    continue;
	  
	  if ((floc = dcHashGet_(0,*ptrp)) == NULL)
	    floc = deepSaveDoit_(*ptrp,fd);
	  
	  /* Write the location of element back to file */
	  pploc = ploc + baseSize + 4 + (i * sizeof(ptr));
#ifdef PERSIST_TRACE_
	  printf("i = %d; (*ptrp) = %d; location = %d\n", i, (*ptrp), pploc);
	  printf("stored location = %d\n", floc);
#endif
	  lseek(fd,pploc,SEEK_SET);
	  write(fd,&floc,sizeof(floc));
	}
      }
    }
    else {
      perror("Persistent 4- or more dimensional array not supported");
      exit(1);
    }
  }
  return ploc;
}


/*
** deep_restore_:
**    Restore the object saved by "deep_save_" in file 'file' to *pp
**    Move fd's file pointer by the size of the next object in fd.
**    This routine relies on the order of the entries of the object being
**      the same as when it was saved.
*/
void deepRestoreDoit_();

void deep_restore_(pp,fd)
     ptr *pp;
     int fd;
{
  read(fd,pp,sizeof(*pp));
  if (*pp == NULL)
    return;
  else 
    lseek(fd,-sizeof(*pp),SEEK_CUR); /*move back */
  
  dcHashInit_(0);
  deepRestoreDoit_(pp,fd);
}


void deepRestoreDoit_(pp,fd)
     ptr *pp;
     int fd;
{
  int ploc;
  int type_p;
  int i;
  
  ploc = lseek(fd,0l,SEEK_CUR); /* Get current disk location */
  readOb_(pp,fd);
  
  dcHashInsert_(0,(ptr)ploc,*pp); 
  type_p = TYPE_(*pp);
#ifdef PERSIST_TRACE_
    printf("Returned from readOb with type = %d\n", type_p);
#endif
  
  if ((type_p <= 0) || (type_p > num_classes_)) {
    fprintf(stderr, "(Warning): Unknown type = %d during restore\n", type_p);
  }
  else {
    if (attr_table_[type_p] == 0) {
      fprintf(stderr, "(Warning): Missing attribute table for class = %d during restore\n", type_p);
    }
  }
  for (i=0;i<ob_attr_num_(type_p);i++) {
    if (ob_attr_ctype_(type_p,i) == CTYPE_PTR_) {
      
      ptr *ptrp;	    
      ptr mloc;
      
      ptrp = (ptr *)(& ((char*)*pp)[ob_attr_offset_(type_p,i)]);
      if (*ptrp == NULL)
	continue;
      
      if ((mloc = (ptr)dcHashGet_(0,*ptrp)) != NULL)
		*ptrp = mloc;
      else {
	/* Move to file location to get ready to read object. */
	lseek(fd,(int)(*ptrp),SEEK_SET); 
#ifdef PERSIST_TRACE_
	printf("Reading i(=%d)th attribute of class = %d\n", i, type_p);
	printf("(*ptrp) = %d\n", (int)*ptrp);
#endif
	deepRestoreDoit_(ptrp,fd);
      }
    }
  } 
}




/*
** readOb_:
**     Read the object in the file to the ptr given by "pp".
**     For now assume that the type numbering scheme is the same
**     as when the object was saved.
**
*/

int readOb_(pp,fd)
    ptr *pp;
    int fd;
{
  int type_p;       /* Type of "p" */
  int obSize=0;     /* Actual number of bytes allocated for "p" */
  int obLoc;        /* Location of "p" on disk */
  int aDim;         /* Array dimension of "p"*/
  int elements = 1; /* Number of elements in array */
  int elements_sz;  /* Size occupied by array elements */
  int i;
  int as[4];        /* Sizes of each array dimension */
  int baseSize;     /* Size of object "p" excluding its array elements */ 
  ptr *ptrp;        /* Pointer to a array element which is a "ptr" in "p" */
  ptr mloc;         /* Pointer to a previously restored element now in memory */
  int offsets;	    /* Number of offset indices */
  int first_off;    /* Offset to the first array element */
#ifdef PERSIST_TRACE_
  int j;
#endif
  
  obLoc = lseek(fd,0l,SEEK_CUR);
  read(fd,&type_p,sizeof(type_p));
  obSize = ob_base_size_(type_p);
  
  if ((aDim = ob_arr_dim_(type_p)) > 0) {

    /* Move up to first array dimension size */
    lseek(fd,obLoc+obSize,SEEK_SET); 
    /* Compute total number of elements */
    for (i=0;i<aDim;i++) {
      read(fd,&as[i],sizeof(int));
    }
    for (i=0;i<aDim;i++)
      elements *= as[i];

    elements_sz = elements * ctype_size_(ob_arr_ctype_(type_p)); 
    /* Scale (ie calculate actual number of bytes) for byte "read()". */

    /* Number of index offsets in array */
    if (aDim == 1) 
      offsets = 0;
    else {
      offsets = 1;
      for (i=0; i<(aDim-1); i++) {
	offsets *= as[i];
      }
    }

    obSize += (elements_sz + aDim*sizeof(int) + offsets*sizeof(int));
  }

  lseek(fd,obLoc,SEEK_SET);
  *pp = (ptr)calloc(obSize,1);
  read(fd,*pp,obSize);
  
  if (aDim > 0) {

    if (aDim <= 4) {
      if (ob_arr_ctype_(type_p) == CTYPE_PTR_) {
	baseSize = ob_base_size_(type_p);

	/* Offset to first array element consists of the base size, space
	   for array sizes, and index offsets. */
	first_off = baseSize + (aDim * sizeof(int)) + (offsets * sizeof(int));
#ifdef PERSIST_TRACE_
	printf("no of elements = %d; first_off = %d\n", elements, first_off);
	for (j=0;j<elements;j++) {
	  ptrp = (ptr *)(& ((char*)(*pp))[baseSize + 4 + (j * sizeof(ptr))]);
	  printf("i(=%d)th element = %d\n", j, (int)(*ptrp));
	} 
#endif
	for (i=0;i<elements;i++) {
	  ptrp = (ptr *)(& ((char*)(*pp))[first_off + (i * sizeof(ptr))]);
	  if (*ptrp == NULL)
	    continue;
	  
	  if ((mloc = (ptr)dcHashGet_(0,*ptrp)) != NULL) 
	    *ptrp = mloc;
	  else {
	    /* Move to file location to read ith element of array. */
	    lseek(fd,(int)(*ptrp),SEEK_SET);
	    deepRestoreDoit_(ptrp,fd);
	  }
	}
      }
    }
    else {
      perror("Persistent-restore for 4- or more dimensional array not done");
      exit(1);
    }
  }
  return 0;
}
