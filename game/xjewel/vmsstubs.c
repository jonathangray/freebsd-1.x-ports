#include <stdlib.h>
struct timeval { long tv_sec; long tv_usec;};

void gettimeofday(z, q)
struct timeval *z, *q;
    {
    unsigned long tod[2];
    SYS$GETTIM(tod);

    z->tv_sec=( (tod[0]/10000000) + ((tod[1]* 429 )&0x7fffffffl) );
    z->tv_usec=((tod[0]/10)%1000000);
/*    printf("sec:%lx, u_sec:%lx\n", z->tv_sec, z->tv_usec);*/
    }

long random()
    {
    return(rand());
    }

void srandom(seed)
long seed;
    {
    srand(seed);
    }


