/* sync.h
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 *
 * Interface for synchronization primitives
 */
#ifndef _SYNCHRONIZATION_
#define _SYNCHRONIZATION_

typedef unsigned int *spin_lock_t;
extern void        sync_init ();
extern spin_lock_t runtime_spin_lock();
extern int         try_spin_lock();
extern void        spin_unlock();
#endif /* !_SYNCHRONIZATION_ */
