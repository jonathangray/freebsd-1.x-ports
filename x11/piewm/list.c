/*****************************************************************************/
/**       Copyright 1988 by Evans & Sutherland Computer Corporation,        **/
/**                          Salt Lake City, Utah                           **/
/**  Portions Copyright 1989 by the Massachusetts Institute of Technology   **/
/**                        Cambridge, Massachusetts                         **/
/**                                                                         **/
/**                           All Rights Reserved                           **/
/**                                                                         **/
/**    Permission to use, copy, modify, and distribute this software and    **/
/**    its documentation  for  any  purpose  and  without  fee is hereby    **/
/**    granted, provided that the above copyright notice appear  in  all    **/
/**    copies and that both  that  copyright  notice  and  this  permis-    **/
/**    sion  notice appear in supporting  documentation,  and  that  the    **/
/**    names of Evans & Sutherland and M.I.T. not be used in advertising    **/
/**    in publicity pertaining to distribution of the  software  without    **/
/**    specific, written prior permission.                                  **/
/**                                                                         **/
/**    EVANS & SUTHERLAND AND M.I.T. DISCLAIM ALL WARRANTIES WITH REGARD    **/
/**    TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES  OF  MERCHANT-    **/
/**    ABILITY  AND  FITNESS,  IN  NO  EVENT SHALL EVANS & SUTHERLAND OR    **/
/**    M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL  DAM-    **/
/**    AGES OR  ANY DAMAGES WHATSOEVER  RESULTING FROM LOSS OF USE, DATA    **/
/**    OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER    **/
/**    TORTIOUS ACTION, ARISING OUT OF OR IN  CONNECTION  WITH  THE  USE    **/
/**    OR PERFORMANCE OF THIS SOFTWARE.                                     **/
/*****************************************************************************/


/**********************************************************************
 *
 * $XConsortium: list.c,v 1.18 90/03/13 15:28:51 jim Exp $
 *
 * TWM code to deal with the name lists for the NoTitle list and
 * the AutoRaise list
 *
 * 11-Apr-88 Tom LaStrange        Initial Version.
 *
 **********************************************************************/

#if !defined(lint) && !defined(SABER)
static char RCSinfo[]=
"$XConsortium: list.c,v 1.18 90/03/13 15:28:51 jim Exp $";
#endif

#include <stdio.h>
#include "twm.h"
#include "screen.h"
#include "gram.h"
#include "list.h"

/***********************************************************************
 *
 *  Procedure:
 *	AddToList - add a window name to the appropriate list
 *
 *  Inputs:
 *	list	- the address of the pointer to the head of a list
 *	name	- a pointer to the name of the window 
 *	ptr	- pointer to list dependent data
 *
 *  Special Considerations
 *	If the list does not use the ptr value, a non-null value 
 *	should be placed in it.  LookInList returns this ptr value
 *	and procedures calling LookInList will check for a non-null 
 *	return value as an indication of success.
 *
 ***********************************************************************
 */

void
AddToList(list_head, name, ptr)
name_list **list_head;
char *name;
char *ptr;
{
    name_list *nptr;

    if (!list_head) return;	/* ignore empty inserts */

    nptr = (name_list *)malloc(sizeof(name_list));
    if (nptr == NULL)
    {
	twmrc_error_prefix();
	fprintf (stderr, "unable to allocate %d bytes for name_list\n",
		 sizeof(name_list));
	Done();
    }

    nptr->next = *list_head;
    nptr->name = name;
    nptr->namelen = strlen(name);
    nptr->ptr = (ptr == NULL) ? (char *)TRUE : ptr;
    *list_head = nptr;
}    

/***********************************************************************
 *
 *  Procedure:
 *	LookInList - look through a list for a window name, or class
 *
 *  Returned Value:
 *	the ptr field of the list structure or NULL if the name 
 *	or class was not found in the list
 *
 *  Inputs:
 *	list	- a pointer to the head of a list
 *	name	- a pointer to the name to look for
 *	class	- a pointer to the class to look for
 *
 ***********************************************************************
 */

char *
LookInList(list_head, name, class)
name_list *list_head;
char *name;
XClassHint *class;
{
    name_list *nptr;
    char *return_name = NULL;

    /* look for the name first */
    for (nptr = list_head; nptr != NULL; nptr = nptr->next)
    {
	if (strncmp(name, nptr->name, nptr->namelen) == 0)
	{
	    return_name = nptr->ptr;
	    break;
	}
    }

    if ((return_name == NULL) && class)
    {
	/* look for the res_name next */
	for (nptr = list_head; nptr != NULL; nptr = nptr->next)
	{
	    if (strncmp(class->res_name, nptr->name, nptr->namelen) == 0)
	    {
		return_name = nptr->ptr;
		break;
	    }
	}

	/* finally look for the res_class */
	if (return_name == NULL)
	{
	    for (nptr = list_head; nptr != NULL; nptr = nptr->next)
	    {
		if (strncmp(class->res_class, nptr->name, nptr->namelen) == 0)
		{
		    return_name = nptr->ptr;
		    break;
		}
	    }
	}
    }

    if ((Scr->ListRings == TRUE) && (return_name != NULL)
	&& (list_head->next != NULL)) 
    {
	/* To implement a ring on the linked list where we cant change the */
	/* list_head, use a simple unlink/link-at-end alg. unless you need */
	/* to move the first link.   In that case swap the contents of the */
	/* first link with the contents of the second then proceed as */
	/* normal.  */
	name_list *tmp_namelist;
	
	if (list_head->ptr == return_name)
	{
	    int tmp_namelen;
	    char *tmp_name;
	    char *tmp_ptr;
	    
	    tmp_namelen = list_head->namelen;
	    tmp_name = list_head->name;
	    tmp_ptr = list_head->ptr;
	    
	    list_head->namelen = list_head->next->namelen;
	    list_head->name = list_head->next->name;
	    list_head->ptr = list_head->next->ptr;
	    
	    list_head->next->namelen = tmp_namelen;
	    list_head->next->name = tmp_name;
	    list_head->next->ptr = tmp_ptr;
	}
	
	for (nptr = list_head; nptr->next != NULL; nptr = nptr->next)
	{
	    if (nptr->next->ptr == return_name)
	      break;
	}
	
	if (nptr->next->next != NULL)
	{
	    tmp_namelist = nptr->next;
	    nptr->next = nptr->next->next;
	    
	    for (nptr = nptr->next; nptr->next != NULL; nptr = nptr->next);
	    nptr->next = tmp_namelist;
	    nptr->next->next = NULL;
	}
    }
    
    return (return_name);
}

char *
LookInNameList(list_head, name)
name_list *list_head;
char *name;
{
    return (LookInList(list_head, name, NULL));
}

/***********************************************************************
 *
 *  Procedure:
 *	GetFromList - look through a list for a window name, or class
 *
 *  Returned Value:
 *	TRUE if the name was found
 *	FALSE if the name was not found
 *
 *  Inputs:
 *	list	- a pointer to the head of a list
 *	name	- a pointer to the name to look for
 *	class	- a pointer to the class to look for
 *
 *  Outputs:
 *	ptr	- fill in the list value if the name was found
 *
 ***********************************************************************
 */

int GetColorFromList(list_head, name, class, ptr)
name_list *list_head;
char *name;
XClassHint *class;
Pixel *ptr;
{
    int save;
    name_list *nptr;

    for (nptr = list_head; nptr != NULL; nptr = nptr->next)
    {
	int len;

	len = strlen(nptr->name);
	if (strncmp(name, nptr->name, len) == 0)
	{
	    save = Scr->FirstTime;
	    Scr->FirstTime = TRUE;
	    GetColor(Scr->Monochrome, ptr, nptr->ptr);
	    Scr->FirstTime = save;
	    return (TRUE);
	}
    }
    if (class)
    {
	for (nptr = list_head; nptr != NULL; nptr = nptr->next)
	{
	    int len;

	    len = strlen(nptr->name);
	    if (strncmp(class->res_name, nptr->name, len) == 0)
	    {
		save = Scr->FirstTime;
		Scr->FirstTime = TRUE;
		GetColor(Scr->Monochrome, ptr, nptr->ptr);
		Scr->FirstTime = save;
		return (TRUE);
	    }
	}

	for (nptr = list_head; nptr != NULL; nptr = nptr->next)
	{
	    int len;

	    len = strlen(nptr->name);
	    if (strncmp(class->res_class, nptr->name, len) == 0)
	    {
		save = Scr->FirstTime;
		Scr->FirstTime = TRUE;
		GetColor(Scr->Monochrome, ptr, nptr->ptr);
		Scr->FirstTime = save;
		return (TRUE);
	    }
	}
    }
    return (FALSE);
}

/***********************************************************************
 *
 *  Procedure:
 *	FreeList - free up a list
 *
 ***********************************************************************
 */

FreeList(list)
name_list **list;
{
    name_list *nptr;
    name_list *tmp;

    for (nptr = *list; nptr != NULL; )
    {
	tmp = nptr->next;
	free((char *) nptr);
	nptr = tmp;
    }
    *list = NULL;
}
