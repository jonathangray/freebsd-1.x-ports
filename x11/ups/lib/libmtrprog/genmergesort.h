/* genmergesort.h - generate code to do a mergesort */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)genmergesort.h	1.6 26/4/92 (UKC) */

/*  GENERIC_MERGE_SORT generates the code necessary to mergesort
 *  an arbitrary linked list. The arguments are:
 *
 *	storageclass:
 *		should be static or extern. Detrmines the storage class
 *		of the function generated.
 *
 *	name:	the name of the function to be generated
 *
 *	type:	
 *		the type of the elements of the list
 *		for a structure xxst, the type will be: struct xxst
 *
 *	next:
 *		the name of the field linking the list
 *
 *  As an example, consider a structure struct xxst, with a link field
 *  "struct xxst *xx_next".  Then the following macro:
 *
 *	GENERIC_MERGE_SORT(static, sortxx, struct xxst, xx_next)
 *
 *  would generate a static function sortxx:
 *
 *	static struct xxst *sortxx(list, len, cmp)
 *	struct xxst *list;
 *	int len, (*cmp)();
 *
 *  where list is the head of the list to be sorted, len is the length
 *  of this list, and cmp is a comparison function, called in the style
 *
 *	(*cmp)(xx1, xx2)
 *
 *  This should return -1, 0 or 1 according as the first argument should
 *  be considered less than, equal to, or greater than the second.
 *
 *  sortxx() returns a pointer to the head of the sorted list.
 *
 *  The macro also generates the static functions split_name() and
 *  merge_name, where "name" is what you supplied to the macro.
 *  These functions are for the use of the sort routine only.
 */		

#define GENERIC_MERGE_SORT(storageclass,name,type,next) \
\
/*  Mergesort a linked list of structures of type type.
 *  List is a pointer to the head of the list, len is the number of
 *  elements in the list, and cmp is the comparison function
 *  as described above. Return a pointer to the head of the sorted list.
 *
 *  Uses the standard recursive mergesort algorithm (see e.g. Kruse)
 */ \
\
static type *CAT(name,_split) PROTO((type *list, int len));\
\
/*  split list, which has len elements at element len/2. Return
 *  a pointer to the second half of the list.
 */ \
static type *CAT(name,_split)(list,len) \
type *list; \
int len; \
{ \
	type *prev; \
	 \
	prev = NULL;	/* to satisfy gcc */ \
	for (; len > 0; --len) { \
		prev = list; \
		list = list->next; \
	} \
	prev->next = NULL; \
	return(list); \
} \
\
static type *CAT(name,_merge) PROTO((type *list1, type *list2, int (*cmp)PROTO((type *, type *))));\
\
/*  Merge the two lists list1 and list2, keeping the resulting
 *  list in order with respect to comparison function cmp (see
 *  above for specification of cmp.
 *  Return a pointer to the merged list.
 */ \
static type *CAT(name,_merge)(list1,list2,cmp) \
register type *list1, *list2; \
int (*cmp)PROTO((type *, type *)); \
{ \
	register type *list, *head; \
	 \
	if ((*cmp)(list1, list2) < 0) { \
		list = list1; \
		list1 = list1->next; \
	} \
	else { \
		list = list2; \
		list2 = list2->next; \
	} \
	head = list; \
	while (list1 != NULL && list2 != NULL) { \
		if ((*cmp)(list1, list2) < 0) { \
			list->next = list1; \
			list1 = list1->next; \
		} \
		else { \
			list->next = list2; \
			list2 = list2->next; \
		} \
		list = list->next; \
	} \
	list->next = (list1 != NULL) ? list1 : list2; \
	return(head); \
} \
\
storageclass type *name PROTO((type *list, int len, int (*cmp)PROTO((type *, type *))));\
\
storageclass type *name(list,len,cmp) \
type *list; \
int len, (*cmp)PROTO((type *, type *)); \
{ \
	type *list2; \
	 \
	if (len > 1) { \
		list2 = CAT(name,_split)(list,len/2); \
		list = name(list,len/2,cmp); \
		list2 = name(list2,(len+1)/2,cmp); \
		return(CAT(name,_merge)(list,list2,cmp)); \
	} \
	else \
		return(list); \
}
