/* imports from active.c */
extern statust actload(), actsync();
extern long incartnum();
extern char *realngname(), *actlook(), *findflag();
extern boolean isflag(), unwanted(), moderated();

#define nxtartnum(ng) incartnum(ng, 1)
#define prevartnum(ng) incartnum(ng, -1)
