#include <stdio.h>

extern long time() ;

int main(argc, argv)
int argc;
char **argv;
{
    long l;
   
    if (argc == 1) 
    {
	printf("%ld\n", time(0));    
    }
    else
    {
	sscanf(argv[1], "%ld", &l);
	printf("%ld\n", time(0) - l);
    }
    exit(0);
}
