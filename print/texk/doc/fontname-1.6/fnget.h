/* fnget.h
   Graham Asher  */

#ifndef FONTNAME_H_
#define FONTNAME_H_ 1

#if __STDC__
#include <stddef.h> /* for size_t */
#endif

/* This is a pretty kludgy way to get around prototypes, but at least
   non-STDC compilers won't bomb out.  --karl@cs.umb.edu  */
extern int FNget (
#if __STDC__
  char *dest, size_t size, char *filename, char *template
#endif
);

#endif /* FONTNAME_H_ */
