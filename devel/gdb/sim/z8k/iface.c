/* gdb->simulator interface.
   Copyright (C) 1992, 1993 Free Software Foundation, Inc.

This file is part of Z8KSIM

Z8KSIM is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Z8KSIM is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Z8KSIM; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include <ansidecl.h>
#include "sim.h"
#include "tm.h"
#include "signal.h"
#include "../include/wait.h"

int
sim_clear_breakpoints ()
{
  return 1;
}

void
sim_set_pc (addr)
     int addr;
{
  tm_store_register (REG_PC, addr);
}

void
sim_store_register (regno, value)
     int regno;
     int value;
{
  tm_store_register (regno, value);
}

int
sim_fetch_register (regno, buf)
     int regno;
     char *buf;
{
  tm_fetch_register (regno, buf);
  return 1;
}

void
sim_write (where, what, howmuch)
     long int where;
     char *what;
     int howmuch;
{
  int i;

  for (i = 0; i < howmuch; i++)
    tm_write_byte (where + i, what[i]);
}

void
sim_read (where, what, howmuch)
     long int where;
     char *what;
     int howmuch;
{
  int i;

  for (i = 0; i < howmuch; i++)
    what[i] = tm_read_byte (where + i);

}

static
void 
control_c (sig, code, scp, addr)
     int sig;
     int code;
     char *scp;
     char *addr;
{
  tm_exception (SIM_INTERRUPT);
}

void
sim_resume (step, sig)
     int step;
     int sig;
{
  void (*prev) ();

  prev = signal (SIGINT, control_c);
  tm_resume (step);
  signal (SIGINT, prev);
}

int
sim_stop_signal ()
{
  int a;

  switch (tm_signal ())
    {
    case SIM_DIV_ZERO:
      WSETSTOP (a, SIGFPE);
      break;
    case SIM_INTERRUPT:
      WSETSTOP (a, SIGINT);
      break;
    case SIM_BAD_INST:
      WSETSTOP (a, SIGILL);
      break;
    case SIM_BREAKPOINT:
      WSETSTOP (a, SIGTRAP);
      break;
    case SIM_SINGLE_STEP:
      WSETSTOP (a, SIGTRAP);
      break;
    case SIM_BAD_SYSCALL:
      WSETSTOP (a, SIGSYS);
      break;
    case SIM_BAD_ALIGN:
      WSETSTOP (a, SIGSEGV);
      break;
    case SIM_DONE:
      WSETEXIT (a, 1);
      break;
    default:
      abort ();
    }
  return a;
}

void
sim_info (x)
     sim_state_type *x;
{
  tm_state (x);
}

void
sim_info_print (x)
     sim_state_type *x;
{
  tm_info_print (x);
}
