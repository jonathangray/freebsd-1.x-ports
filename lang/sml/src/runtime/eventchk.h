/* eventchk.h */

/* 25Jan93  e  - so event checking can take place on the Mac */

#ifdef THINK_C

#include <LoMem.h>

extern void os_event_check( void );
extern long next_eventchk_ticks;

#define TicksBetweenEventChecks 6

#define MAYBE_EVENTCHK() { if ( Ticks >= next_eventchk_ticks ) os_event_check(); }

#else

#define MAYBE_EVENTCHK()

#endif
