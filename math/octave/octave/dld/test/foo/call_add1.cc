
extern "C"
{
#include "dld.h"
}

#include <stdio.h>
    
int x;

void
list_undefined (void)
{
  int i;
  char **undefined_syms = dld_list_undefined_sym ();
  for (i = 0; i < dld_undefined_sym_count; i++)
    printf ("%s\n", undefined_syms[i]);
}

/*
 *  Dynamically link in "add1.o", which defines the function "add1".
 *  Invoke "add1" to increment the global variable "x" defined *here*.
 */
int
main (int argc, char **argv)
{
    register void (*func) ();

    /* required initialization. */
    dld_init (argv[0]);

    x = 1;
    printf ("global variable x = %d\n", x);

    dld_create_reference ("add1__Fv");

    list_undefined ();

    if (dld_link ("add1.a"))
      dld_perror ("add1.a");

    list_undefined ();

    if (dld_link ("add2.a"))
      dld_perror ("add2.a");

    list_undefined ();

    /* grap the entry point for function "add1" */
    func = (void (*) ()) dld_get_func ("add1__Fv");

    /* invoke "add1" */
    (*func) ();
    printf ("global variable x = %d\n", x);
    fflush (stdout);

    return 0;
}
