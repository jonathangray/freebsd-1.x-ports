/*
 * cache control
 */

#include <stdio.h>
#include <sys/types.h>
#include "news.h"
#include "active.h"
#include "caches.h"
#include "transmit.h"

statust
synccaches()			/* force dirty in-core caches to disk */
{
	return actsync() | trclose();
}
