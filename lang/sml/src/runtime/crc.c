/* CRC function written by Andrew Appel, not tested since last edit. */
#include <stdio.h>

#define POLY 0x48000000
/* this stands for the prime polynomial x^32+x^7+x^3, with the high-order
   term removed, and then bits reversed */

#define LOG 8  /* any number 1-31 will do */

static unsigned long crctab[1<<LOG];

static initcrc()
{int i,j, sum;
 for (i=0; i<(1<<LOG); i++)
   {sum=0;
    for(j= LOG-1; j>=0; j=j-1)
     if (i&(1<<j)) sum ^= ((unsigned long)POLY)>>j;
    crctab[i]=sum;
   }
}

unsigned long crc(f) FILE *f;
{register int c, h=0;
 while ((c=getc(f))!=EOF)
    h = (h>>LOG)^crctab[(h^c) & ((1<<LOG)-1)];
 return h;
}

main()
{
 initcrc();
 printf("%08x\n",crc(stdin));
}
