/*
 * Here's a map generating program i spent way too much time on.  It makes maps
 * of whatever size you like, with lots of options for specifying map
 * generating parameters.
 * 
 * The program uses a random wandering path to make 'islands' of 'x's on the
 * map, then goes through several steps to clean up the results, add home
 * bases, fuel sites, cannons, etc.
 * 
 * You can vary the parameters (notably the map size and the number of
 * islands) to vary how hard it is to fly around the map.
 * 
 * It's pretty standard c, so it ought to compile wherever you are.  If you
 * have to make any changes to the code to get it to compile, please send me
 * patches; i don't have access to a wide variety of machines to try compiling
 * on.
 * 
 * If you make any functionality changes or notice any bugs, please let me know
 * that too and send me any changes you make.
 * 
 * Thanks and enjoy!
 */

#include <stdio.h>
#include <sys/types.h>
#include <time.h>
#include <stdlib.h>
#include <strings.h>

	/* gets the character at x,y from map */
#define POS(x,y) *(map+x+((y)*xsize))
	/* exit with message */
#define die(x) fprintf(stderr,x);fflush(stderr);exit(1);
#define die2(x,y) fprintf(stderr,x,y);fflush(stderr);exit(1);

#define VERSION 1.8

void        make_islands();	/* function definitions to keep the compiler
				 * happy */
void        smooth();
void        clean_map();
void        place_goodies();
void        make_W_safe();
void        parse();
void        usage();
int         count_voids();

#ifdef sun
extern long random();
extern void srandom();
#define rand random
#define srand srandom
#endif

static char worm_type[3] =
{'W', 'I', 'O'};	/* wormhole types */
static char grav_sources[4] =
{'+', '-', '<', '>'};	/* various gravity sources */
int         direction[8][2] =
{
    {-1, -1},
    {0, -1},
    {1, -1},		/* in this    012  */
    {1, 0},
    {1, 1},
    {0, 1},		/* order:     7.3  */
    {-1, 1},
    {-1, 0}};		/* 654  */

char        W_mask[9][9] =
{
    {'x', 'x', 's', '.', '.', '.', 'a', 'x', 'x'},
    {'x', '.', '.', '.', '.', '.', '.', '.', 'x'},	/* a mask to make */
    {'s', '.', '.', '.', '.', '.', '.', '.', 'a'},	/* safe wormholes */
    {'.', '.', '.', '.', '.', '.', '.', '.', '.'},
    {'.', '.', '.', '.', '.', '.', '.', '.', '.'},
    {'.', '.', '.', '.', '.', '.', '.', '.', '.'},
    {'w', '.', '.', '.', '.', '.', '.', '.', 'q'},
    {'x', '.', '.', '.', '.', '.', '.', '.', 'x'},
    {'x', 'x', 'w', '.', '.', '.', 'q', 'x', 'x'}};

char       *map;	/* pointer to the upper left corner of the map  */
			/* map dimensions are [0..xsize-1, 0..ysize-1]  */
char       *namep;	/* pointer to user-supplied name for map        */
int         xsize,
            ysize,	/* size of the map                              */
            num_islands,/* how many 'islands' to generate               */
            num_players,/* how many starting places (max 16)            */
            timestamp,	/* for generating an arbitrary mapname          */
            smoothness,	/* % of the time diagonals will be smoothed     */
            cannon_d,	/* density of cannons, fuel depots, gravity and */
            fuel_d,	/* dust sources: (xsize*ysize)/n of each per map */
            grav_d,
            dust_d,
            wormholes;	/* how many wormholes to make                   */
int         want_walls,	/* have border walls or not?                    */
            world_wrap;	/* wrap around edges (torus world) or not?      */

int         main(argc, argv)
int         argc;
char       *argv[];
{
    int         i,
                j,
                seed;

    time(&timestamp);		/* get time for mapname later   */
    xsize = ysize = 200;	/* default 200x200 map          */
    num_islands = 300;		/* default 300 islands          */
    smoothness = 100;		/* default 100% smoothness      */
    num_players = 16;		/* default 16 player spaces     */
    want_walls = 1;		/* default to having walls      */
    world_wrap = 0;		/* default no wrap              */
    cannon_d = 800;		/* 1 cannon per 800 area units  */
    fuel_d = 1000;		/* 1 fuel depot per 1000 a.u.   */
    grav_d = 5000;		/* 1 gravity generator per 5000 */
    dust_d = 2500;		/* 1 dust generator per 2500    */
    wormholes = 4;		/* default 4 wormholes          */

    time(&seed);		/* get a seed for rand */
    srand((unsigned int)seed);/* initialize rand with the seed */

    parse(argc, argv);		/* parse command line options */

    map = (char *)malloc(xsize * ysize);	/* get some space */
    memset(map, 056, xsize * ysize);	/* initialize it with '.' characters */

    /* should we make walls? */
    if (want_walls) {
	for (i = 0; i < xsize; i++) {	/* fill in top and bottom walls */
	    *(map + i) = 'x';
	    *(map + (xsize * ysize) - i - 1) = 'x';
	}
	for (j = 0; j < ysize; j++) {	/* fill in right and left walls */
	    *(map + (j * xsize)) = 'x';
	    *(map + (j * xsize) + (xsize - 1)) = 'x';
	}
    }
    make_islands();
    place_goodies();
    clean_map();

    /* write map */
    printf("%dx%d\n", xsize, ysize);	/* map size */
    if (world_wrap)
	printf("6\n");		/* world rule 6 = torus world */
    else
	printf("0\n");		/* world rule 0 = normal map */
    if (namep != NULL)		/* print user supplied name or */
	printf("%s\n", namep);	/* random name */
    else
	printf("randworld%d\n", timestamp);
    printf("cloister's mapmaker, version %3.1f\n", VERSION);

    for (j = 0; j < ysize; j++) {	/* print map */
	for (i = 0; i < xsize; i++)
	    putchar(POS(i, j));
	putchar('\n');
    }
    return (0);
}


int         in_bounds(a, b)	/* forces a to be within the range [0..b-1] */
int         a,
            b;
{
    if (a < 0)
	a = 0;
    if (a > b - 1)
	a = b - 1;
    return (a);
}


void        make_islands()
{		/* this is complicated.  wander randomly around the
		 * map, doing smoothing when the wander is diagonal */
    int         i,
                j,
                oldx,
                oldy;

    for (i = 0; i < num_islands; i++) {
	int         x,
	            y,
	            dir;

	x = rand() % xsize;	/* start somewhere */
	y = rand() % ysize;
	POS(x, y) = 'x';	/* set initial spot to be a block */
	for (j = 1; j < (rand() % (2 * (xsize + ysize))) + 1; j++) {
	    /* size of islands >= to the */
	    /* perimeter of the map in blocks */
	    oldx = x;
	    oldy = y;		/* remember these for the smoothing phase */
	    dir = rand() % 8;	/* pick a direction */
	    x += direction[dir][0];	/* go that way */
	    y += direction[dir][1];
	    x = in_bounds(x, xsize);	/* force x,y to be in bounds */
	    y = in_bounds(y, ysize);
	    POS(x, y) = 'x';	/* set that spot to be a block */
	    if ((dir % 2) == 0) {	/* do smoothing algorithm for diagonal
					 * dir's */
		switch (dir) {
		    /* attempt to fill in corner touching 'x's */
		    /* with an appropriate diagonal blocks     */
		case 0:	/* went northwest */
		    smooth(oldx, oldy - 1, 'w');
		    smooth(oldx - 1, oldy, 'a');
		    break;
		case 2:	/* went northeast */
		    smooth(oldx, oldy - 1, 'q');
		    smooth(oldx + 1, oldy, 's');
		    break;
		case 4:	/* went southeast */
		    smooth(oldx + 1, oldy, 'w');
		    smooth(oldx, oldy + 1, 'a');
		    break;
		case 6:	/* went southwest */
		    smooth(oldx - 1, oldy, 'q');
		    smooth(oldx, oldy + 1, 's');
		}
	    }
	}
    }
}

void        smooth(tempx, tempy, block)	/* does an in bounds check and puts
					 * block at */
int         tempx,
            tempy;	/* POS(tempx, tempy), unless it would over-  */
char        block;	/* write an existing block                   */
{
    in_bounds(tempx, xsize);	/* make sure we're in bounds */
    in_bounds(tempy, ysize);
    if (((POS(tempx, tempy)) == '.') &&	/* if space is empty and */
	(rand() % 100 < smoothness))	/* we get to smooth */
	POS(tempx, tempy) = block;	/* set to desired block shape */
}

void        clean_map()
{   /* remove inaccessable pockets, by flood filling '.' char's
     * with ' ' chars, then get rid of remaining '.' chars.  also
     * remove diagonal blocks without a ' ' next to them.        */

    struct spot {
	int         x;
	int         y;	/* struct for linked list */
	struct spot *next;
    };			/* of points to paint.   */

    int         x,
                y,
                h,
                v;
    char        c;
    struct spot list,
               *cursor;	/* a linked list and a pointer into it. */
    struct spot *end;	/* pointer to the end of the list.      */

    cursor = &list;
    end = &list;

    /*
     * get a spot which is an open '.'  ignore the chance that we could end up
     * in a little hole and fill in the whole map.
     */
    while (POS(x, y) != '.') {
	x = (rand() % (xsize - 2)) + 1;
	y = (rand() % (ysize - 2)) + 1;
    }
    list.x = x;			/* set list head to this spot */
    list.y = y;

    while (cursor->next != NULL) {
	x = cursor->x;
	y = cursor->y;
	for (h = x - 1; h <= x + 1; h++)	/* enqueue the '.' spaces
						 * surrounding the current */
	    for (v = y - 1; v <= y + 1; v++)	/* space. */
		if (h >= 0 && h < xsize && y >= 0
		    && y < ysize && POS(h, v) == '.') {
		    POS(h, v) = ' ';	/* erase '.' from h,v; prevent multiple
					 * enqueing */
		    end->next = malloc(sizeof(struct spot));
		    			/* attach a new node */
		    end = end->next;	/* move to the new node */
		    end->x = h;		/* fill in its values */
		    end->y = v;
		    end->next = (struct spot *)NULL;
		}
	cursor = cursor->next;	/* advance to next thing in the list */
    }

    for (h = 0; h < xsize; h++)
	for (v = 0; v < ysize; v++)
	    if (POS(h, v) == '.')
		POS(h, v) = 'x';/* erase remaining open spots.  */

    for (x = 1; x < xsize - 1; x++)	/* get rid of bad diagonal blocks. */
	for (y = 1; y < ysize - 1; y++) {
	    c = POS(x, y);
	    if ((c == 'q') || (c == 'w') || (c == 'a') || (c == 's'))
		if (count_voids(x, y, ' ') == 0)
		    POS(x, y) = 'x';
	}

}

/* returns number of c's bordering x,y */
int         count_voids(x, y, c)
int         x,
            y;
char        c;
{
    int         void_count = 0;

    if ((POS(x + 1, y)) == c)
	void_count++;
    if ((POS(x - 1, y)) == c)
	void_count++;
    if ((POS(x, y + 1)) == c)
	void_count++;
    if ((POS(x, y - 1)) == c)
	void_count++;
    return (void_count);
}

/* fuel depots, gravity thingies, home spaces, cannons */
void        place_goodies()
{
    int         i;

    for (i = 0; i < ((xsize * ysize) / fuel_d) + 1; i++) {
	/* place 1 depot per fuel_d spaces */
	int         x,
	            y,
	            placed_fuel,
	            tries;

	placed_fuel = 0;
	tries = 0;
	while ((!placed_fuel) && (tries < 500)) {
	    tries++;
	    x = (rand() % (xsize - 2)) + 1;
	    y = (rand() % (ysize - 2)) + 1;
	    if (POS(x, y) == 'x' && count_voids(x, y, '.') > 0) {
		POS(x, y) = 'F';
		placed_fuel = 1;
	    }
	}
    }

    for (i = 0; i < ((xsize * ysize) / dust_d) + 1; i++) {
	/* place 1 dust per dust_d spaces */
	int         x,
	            y,
	            placed_dust,
	            tries;

	placed_dust = 0;
	tries = 0;
	while ((!placed_dust) && (tries < 500)) {
	    tries++;
	    x = (rand() % (xsize - 2)) + 1;
	    y = (rand() % (ysize - 2)) + 1;
	    if (POS(x, y) == '.') {
		POS(x, y) = '@';
		placed_dust = 1;
	    }
	}
    }

    for (i = 0; i < ((xsize * ysize) / grav_d) + 1; i++) {
	/* place 1 gravity per * grav_d spaces */
	int         x,
	            y;

	x = (rand() % (xsize - 2)) + 1;
	y = (rand() % (ysize - 2)) + 1;
	POS(x, y) = grav_sources[rand() % 4];
    }

    {				/* temp. scope around wormhole logic */
	int         haveI = 0,
	            haveO = 0,
	            haveW = 0;

	for (i = 0; i < wormholes; i++) {	/* place as many wormholes as
						 * needed/wanted */
	    int         x,
	                y;

	    x = (rand() % (xsize - 2)) + 1;
	    y = (rand() % (ysize - 2)) + 1;
	    POS(x, y) = worm_type[rand() % 3];
	    if (POS(x, y) == 'I')
		haveI = 1;
	    if (POS(x, y) == 'O')
		haveO = 1;
	    if (POS(x, y) == 'W')
		haveW = 1;
	    make_W_safe(x, y);
	}
	if (haveI && !(haveO || haveW)) { /* wormhole checking. */
	    /* if there is an in, make sure there is an out. */
	    int         x,
	                y;

	    x = (rand() % (xsize - 2)) + 1;
	    y = (rand() % (ysize - 2)) + 1;
	    POS(x, y) = 'O';
	    make_W_safe(x, y);
	}
	if (haveO && !(haveI || haveW)) { /* wormhole checking. */
	    /* if there is an out, make sure there is an in. */
	    int         x,
	                y;

	    x = (rand() % (xsize - 2)) + 1;
	    y = (rand() % (ysize - 2)) + 1;
	    POS(x, y) = 'I';
	    make_W_safe(x, y);
	}
    }

    for (i = 0; i < num_players; i++) {	/* add player start spaces */
	int         x,
	            y,
	            h,
	            v;

	do {
	    x = (rand() % (xsize - 4)) + 2;	/* puts a player start that
						 * looks like: */
	    y = (rand() % (ysize - 6)) + 5;	/* ...                                  */
	}			/* ...  where the solid 'x' is at       */
	while (POS(x, y) != '.');
	for (h = x - 1; h < x + 2; h++)	/* ...  random position x,y.  don't     */
	    for (v = y - 4; v < y; v++)	/* .*.  care what's next to the 'x'.    */
		POS(h, v) = '.';
	POS(x, (y - 1)) = '*';
	POS(x, y) = 'x';
    }

    for (i = 0; i < (xsize * ysize) / cannon_d + 1; i++) {	/* place one per
								 * cannon_d spaces */
	int         x,
	            y;
	int         open_spot,
	            placed;

	placed = 0;
	while (!placed) {
	    open_spot = 0;
	    while (!open_spot) {
		x = (rand() % (xsize - 2)) + 1;	/* get a non-border/wall spot,     */
		y = (rand() % (ysize - 2)) + 1;	/* and make sure it's an open
						 * spot */
		if (*(map + x + (y * xsize)) == '.')
		    open_spot = 1;
	    }
	    if (POS(x, (y + 1)) == 'x')
		POS(x, y) = 'r';/* check to make sure that  */
	    if (POS(x, (y - 1)) == 'x')
		POS(x, y) = 'c';/* there is a block for the */
	    if (POS((x + 1), y) == 'x')
		POS(x, y) = 'd';/* cannon to sit on, and    */
	    if (POS((x - 1), y) == 'x')
		POS(x, y) = 'f';/* place one if there is.   */
	    if (POS(x, y) != '.')
		placed = 1;	/* did we place one?        */
	}
    }
}



void        make_W_safe(x, y)	/* makes a safe hole around the W by &&'ing
				 * W_mask with */
int         x,
            y;		/* whatever's on the map around the W.
			 * moves x,y if they are too close to an edge. */
{
    int         h,
                v;
    char        wormType;

    wormType = POS(x, y);		/* remember what type it is. */
    POS(x, y) = 'x';		/* erase the hole, because x and y may change
				 * and we don't */
				/* want two of them. */
    if (x < 5)
	x = 5;		/* make sure x,y are far enough from the edges */
    if (x > xsize - 6)
	x = xsize - 6;		/* so as not to cause problems. */
    if (y < 5)
	y = 5;
    if (y > ysize - 6)
	y = ysize - 6;

    for (h = x - 4; h <= x + 4; h++)	/* make a hole in the map for the
					 * wormhole */
	for (v = y - 4; v <= y + 4; v++)
	    if (POS(h, v) != '.')
		POS(h, v) = W_mask[v - y + 4][h - x + 4];

    POS(x, y) = wormType;		/* put the wormhole back */
}



void        parse(argc, argv)	/* parse command line args. */
int         argc;
char       *argv[];
{
    int         i,
                parsed;

    for (i = 1; i < argc; i++) {
	parsed = 0;		/* haven't parsed argv[i] yet. */
	if (strcmp(argv[i], "-x") == 0)	/* xsize? */
	    if (sscanf(argv[++i], "%d", &xsize) == 1)	/* try and read value */
		parsed = 1;	/* note successful scan */
	    else {
		die("error: could not read x size.\n")
	    }			/* else exit with error */
	if (strcmp(argv[i], "-y") == 0)	/* the rest are very similar */
	    if (sscanf(argv[++i], "%d", &ysize) == 1)
		parsed = 1;
	    else {
		die("error: could not read y size.\n")
	    }
	if (strcmp(argv[i], "-i") == 0)
	    if (sscanf(argv[++i], "%d", &num_islands) == 1)
		parsed = 1;
	    else {
		die("error: could not read number of islands.\n")
	    }
	if (strcmp(argv[i], "-p") == 0)
	    if (sscanf(argv[++i], "%d", &num_players) == 1)
		parsed = 1;
	    else {
		die("error: could not read number of players.\n")
	    }
	if (strcmp(argv[i], "-s") == 0)
	    if (sscanf(argv[++i], "%d", &smoothness) == 1)
		parsed = 1;
	    else {
		die("error: could not read smoothness\n")
	    }
	if (strcmp(argv[i], "-f") == 0)
	    if (sscanf(argv[++i], "%d", &fuel_d) == 1)
		parsed = 1;
	    else {
		die("error: could not read fuel density\n")
	    }
	if (strcmp(argv[i], "-g") == 0)
	    if (sscanf(argv[++i], "%d", &grav_d) == 1)
		parsed = 1;
	    else {
		die("error: could not read gravity generator density\n")
	    }
	if (strcmp(argv[i], "-c") == 0)
	    if (sscanf(argv[++i], "%d", &cannon_d) == 1)
		parsed = 1;
	    else {
		die("error: could not read cannon density\n")
	    }
	if (strcmp(argv[i], "-d") == 0)
	    if (sscanf(argv[++i], "%d", &dust_d) == 1)
		parsed = 1;
	    else {
		die("error: could not read stardust density\n")
	    }
	if (strcmp(argv[i], "-n") == 0) {
	    namep = argv[++i];
	    parsed = 1;
	}
	if (strcmp(argv[i], "-h") == 0) {
	    if (sscanf(argv[++i], "%d", &wormholes) == 1) {
		parsed = 1;
		if (wormholes < 2) {
		    wormholes = 2;
		    fprintf(stderr, "error: must have >= 2 wormholes, using 2.\n");
		    fflush(stderr);
		}
	    } else {
		die("error: could not read number of wormholes\n");
	    }
	}
	if (strcmp(argv[i], "-w") == 0)
	    parsed = !(want_walls = 0);	/* parsed = 1 & want_walls = 0 */

	if (strcmp(argv[i], "-t") == 0)
	    parsed = world_wrap = 1;	/* parsed = 1 & world_wrap = 1 */

	if ((strcmp(argv[i], "-?") == 0) ||	/* poor user needs help */
	    (strcmp(argv[i], "-help") == 0))
	    usage();		/* print help and exit  */

	if (!parsed) {		/* user typed some garbage option */
	    die2("error: unknown option %s\n", argv[i])
	}
    }
}



void        usage()
{
    fprintf(stderr, "makemap:  usage:\n");
    fprintf(stderr, "   makemap [-x <val>] [-y <val>] [-i <val>] [-p <val>]\n");
    fprintf(stderr, "           [-s <val>] [-f <val>] [-g <val>] [-c <val>]\n");
    fprintf(stderr, "           [-h <val>] [-d <val>] [-w] [-t] [-n <name>]\n");
    fprintf(stderr, "where: <val> is a decimal integer, <name> is a string,\n");
    fprintf(stderr, "       -x and -y specify x and y sizes of the map,\n");
    fprintf(stderr, "       -i gives the number of islands to create,\n");
    fprintf(stderr, "       -p gives the number of player starting spaces,\n");
    fprintf(stderr, "       -s gives the percent smoothness, and\n");
    fprintf(stderr, "       -w do not put walls on the borders.\n");
    fprintf(stderr, "          walls are suggested unless you are creating\n");
    fprintf(stderr, "          panels of a larger map to be spliced together.\n");
    fprintf(stderr, "       -t torus world.  the map wraps at edges.  if you\n");
    fprintf(stderr, "          use this option, you should use the -w option.\n");
    fprintf(stderr, "          the default is to not wrap the map.\n");
    fprintf(stderr, "       -f, -g, -c and -d specify how much map area per\n");
    fprintf(stderr, "          fuel depot, gravity generator, cannon and\n");
    fprintf(stderr, "          startdust generator\n");
    fprintf(stderr, "          (xsize*ysize)/<val> is how many are placed.\n");
    fprintf(stderr, "       -h gives number of wormholes to place on map.\n");
    fprintf(stderr, "       -n specifies name of map.  name should be placed\n");
    fprintf(stderr, "          in single-quotes.\n");
    fflush(stderr);
    exit(0);
}
