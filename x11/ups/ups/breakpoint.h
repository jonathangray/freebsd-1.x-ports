/* breakpoint.h - public header file for bp.c */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)breakpoint.h	1.4 4/7/91 (UKC) */

#define BREAKPOINT_H_INCLUDED

typedef struct breakpoint_idst { int bpi_dummy; } *breakpoint_t;

breakpoint_t add_breakpoint PROTO((taddr_t addr));
void set_breakpoint_data PROTO((breakpoint_t breakpoint, long));
long get_breakpoint_data PROTO((breakpoint_t breakpoint));
int remove_breakpoint PROTO((breakpoint_t breakpoint));
int breakpoint_is_installed PROTO((breakpoint_t breakpoint));
breakpoint_t addr_to_breakpoint PROTO((taddr_t addr));
taddr_t breakpoint_to_addr PROTO((breakpoint_t breakpoint));

#ifdef PROC_H_INCLUDED
int install_breakpoint PROTO((breakpoint_t breakpoint, proc_t proc));
int uninstall_breakpoint PROTO((breakpoint_t breakpoint));
int install_all_breakpoints PROTO((proc_t proc));
int uninstall_all_breakpoints PROTO((proc_t proc));
breakpoint_t get_breakpoint_at_addr PROTO((proc_t proc, taddr_t addr));
void mark_breakpoints_as_uninstalled PROTO((proc_t proc));
#endif
