/* A simple random-world generator for Xpilot.
 * Written by Paul Gardner (pgar@excalib.com) 2/93.
 * Updated by Paul Gardner for Xpilot 3.x 7/93.
 */

#include <stdio.h>
#include <errno.h>
#include <string.h>

/* Xpilot headers */
#include "map.h"

#ifdef _HPUX_SOURCE
#define srandom srand
#define random rand
#endif

#define MIN_WORMHOLES 2

static int wd = 200;
static int ht = 200;
static int seed = -1;
static int edgewrap = 0;
static char *author = "Paul Gardner (mkmap)";
static char *mapname = "Random (%d)";
static char *fn = "a.map";
static int verbose = 1;

/* probabilities are 1 in ... */
static int seed_prob = 800;
static int grow_pass = 40;
static int grow_prob = 20;
static int smear_prob = 1;

/* densities are expressed in parts per DENSITY_FACTOR */
#define DENSITY_FACTOR 1000000

#define GRAV_RAD 3
#define WORM_RAD 3

static char *progname;
static int sz;
static char **map;
static char **tmpmap;

#define GETMAP(m,x,y) ((m)[(y)][(x)])
#define SETMAP(m,x,y,v) ((m)[(y)][(x)]=(v))

/* terrain types for map[][] are those from map.h plus: */
#define NOT_VALID -1
#define NW_FILLED (WORMHOLE+1)
#define NE_FILLED (WORMHOLE+2)
#define SW_FILLED (WORMHOLE+3)
#define SE_FILLED (WORMHOLE+4)
#define UP_CANNON (WORMHOLE+5)
#define DN_CANNON (WORMHOLE+6)
#define LF_CANNON (WORMHOLE+7)
#define RT_CANNON (WORMHOLE+8)

static int valid_fuel( x, y )
int x, y;
{
	int k;

	if ( GETMAP(map,x,y) != FILLED )
		return( NOT_VALID );
	for ( k=1 ; ; k++ )
	{
		if ( k == 2 )
			return( NOT_VALID );
		if ( GETMAP(map,x-k,y) == SPACE )
			return( FUEL );
		if ( GETMAP(map,x,y-k) == SPACE )
			return( FUEL );
		if ( GETMAP(map,x+k,y) == SPACE )
			return( FUEL );
		if ( GETMAP(map,x,y+k) == SPACE )
			return( FUEL );
	}
}

static int valid_base( x, y )
int x, y;
{
	int k;

	for ( k=0 ; k<3 ; k++ )
	{
		if ( (y-k) < 0 )
			return( NOT_VALID );
		if ( GETMAP(map,x,y-k) != SPACE )
			return( NOT_VALID );
	}
	if ( (y+1) >= ht )
		return( NOT_VALID );
	return( (GETMAP(map,x,y+1) == FILLED) ? BASE : NOT_VALID );
}

static int valid_cannon( x, y )
int x, y;
{
	if ( GETMAP(map,x,y) != SPACE )
		return( NOT_VALID );
	switch ( random() % 4 ) /* select fairly among the 4 possibilities */
	{
		case 0:
			if ( (GETMAP(map,x,y-1)==FILLED) && (GETMAP(map,x,y+1)==SPACE) )
				return( DN_CANNON );
		case 1:
			if ( (GETMAP(map,x,y+1)==FILLED) && (GETMAP(map,x,y-1)==SPACE) )
				return( UP_CANNON );
		case 2:
			if ( (GETMAP(map,x-1,y)==FILLED) && (GETMAP(map,x+1,y)==SPACE) )
				return( RT_CANNON );
		case 3:
			if ( (GETMAP(map,x+1,y)==FILLED) && (GETMAP(map,x-1,y)==SPACE) )
				return( LF_CANNON );
			if ( (GETMAP(map,x,y-1)==FILLED) && (GETMAP(map,x,y+1)==SPACE) )
				return( DN_CANNON );
			if ( (GETMAP(map,x,y+1)==FILLED) && (GETMAP(map,x,y-1)==SPACE) )
				return( UP_CANNON );
			if ( (GETMAP(map,x-1,y)==FILLED) && (GETMAP(map,x+1,y)==SPACE) )
				return( RT_CANNON );
			return( NOT_VALID );
	}
}

/* examine a neighborhood around X,Y */
static int neigh( X, Y, radius, func )
int X, Y, radius, (*func)();
{
	int i, j, x, y, ret;

	for ( i= -radius, y=Y+i ; i<=radius ; i++, y++ )
	{
		if ( (y < 0) || (y >= ht) )
			continue;
		for ( j= -radius, x=X+j ; j<=radius ; j++, x++ )
		{
			if ( (x < 0) || (x >= wd) )
				continue;
			if ( ret = (*func)( X+j, Y+i ) )
				return( ret );
		}
	}
	return( 0 );
}

static int grav_neigh_tst( x, y )
int x, y;
{
	return( (GETMAP(map,x,y)==BASE) || (GETMAP(map,x,y)==WORMHOLE) );
}

static int valid_grav( x, y, type )
int x, y, type;
{
	return( neigh( x, y, GRAV_RAD, grav_neigh_tst ) ? NOT_VALID : type );
}

static int worm_neigh_tst( x, y )
int x, y;
{
	return( GETMAP(map,x,y) != SPACE );
}

static int valid_worm( x, y, type )
int x, y, type;
{
	return( neigh( x, y, WORM_RAD, worm_neigh_tst ) ? NOT_VALID : type );
}

typedef struct
{
	int type;
	char *name;
	char symbol;
	int density;
	int nmin, nmax;
	int (*valid)();
	int xminborder, yminborder, xmaxborder, ymaxborder;
	int n;
} Terrain;
#define MAX_CANNONS (-1)
#define MAX_BASES (-1)
#define MAX_FUELS (-1)
#define MAX_GRAVS (-4)
#define MAX_WORMHOLES (-1)
static Terrain terrain[] =
{
{ -1, "mystery", '?', -1, -1, -1, (int (*)())0, 0, 0, 0, 0, },
{ FILLED, "filled", 'x', -1, -1, -1, (int (*)())0, 0, 0, 0, 0, },
{ NW_FILLED, "nwfilled", 's', -1, -1, -1, (int (*)())0, 0, 0, 0, 0, },
{ NE_FILLED, "nefilled", 'a', -1, -1, -1, (int (*)())0, 0, 0, 0, 0, },
{ SW_FILLED, "swfilled", 'w', -1, -1, -1, (int (*)())0, 0, 0, 0, 0, },
{ SE_FILLED, "sefilled", 'q', -1, -1, -1, (int (*)())0, 0, 0, 0, 0, },
{ UP_CANNON, "cannon", 'r', 1800, -1, MAX_CANNONS, valid_cannon,
	1, 1, 1, 1, },
{ DN_CANNON, "cannon", 'c', -1, -1, -1, (int (*)())0, 0, 0, 0, 0, },
{ LF_CANNON, "cannon", 'd', -1, -1, -1, (int (*)())0, 0, 0, 0, 0, },
{ RT_CANNON, "cannon", 'f', -1, -1, -1, (int (*)())0, 0, 0, 0, 0, },
{ BASE, "base", '_', 340, 1, MAX_BASES, valid_base, 2, 2, 2, 2, },
{ SPACE, "space", ' ', -1, -1, -1, (int (*)())0, 0, 0, 0, 0, },
{ FUEL, "fuel", '#', 1100, -1, MAX_FUELS, valid_fuel, 0, 0, 0, 0, },
{ POS_GRAV, "pos_grav", '+', 50, -1, MAX_GRAVS/4, valid_grav,
	GRAV_RAD, GRAV_RAD, GRAV_RAD, GRAV_RAD, },
{ NEG_GRAV, "neg_grav", '-', 50, -1, MAX_GRAVS/4, valid_grav,
	GRAV_RAD, GRAV_RAD, GRAV_RAD, GRAV_RAD, },
{ ACWISE_GRAV, "acwise_grav", '<', 30, -1, MAX_GRAVS/4, valid_grav,
	GRAV_RAD, GRAV_RAD, GRAV_RAD, GRAV_RAD, },
{ CWISE_GRAV, "cwise_grav", '>', 30, -1, MAX_GRAVS/4, valid_grav,
	GRAV_RAD, GRAV_RAD, GRAV_RAD, GRAV_RAD, },
{ WORMHOLE, "wormhole", '@', 80, MIN_WORMHOLES, MAX_WORMHOLES, valid_worm,
	WORM_RAD, WORM_RAD, WORM_RAD, WORM_RAD, },
};
#define NTERRAIN (sizeof(terrain)/sizeof(terrain[0]))

static char terr2symbol( m )
int m;
{
	int i;

	for ( i=0 ; i<NTERRAIN ; i++ )
		if ( terrain[i].type == m )
			return( terrain[i].symbol );
	printf( "%s: Mystery terrain type.\n", progname );
	return( terrain[0].symbol );
}

/* Select random locations for up to N squares of the type returned by
 * (*tstfunc)().  See Knuth for a description of the adaptive probability
 * algorithm.
 */
static int random_distribute( N, type, tstfunc, xmin, ymin, xmax, ymax )
int N, type, (*tstfunc)(), xmin, ymin, xmax, ymax;
{
	int n, m, M;
	int x, y;
	int actual_type;

	/* N: total to place */
	if ( N <= 0 )
		return( 0 );
	n = 0; /* placed so far */
	m = 0; /* potential sites visited */
	M = 0; /* total potential sites */
	for ( y=ymin ; y<=ymax ; y++ )
		for ( x=xmin ; x<=xmax ; x++ )
		{
			actual_type = (*tstfunc)( x, y, type );
			SETMAP( tmpmap, x, y, actual_type );
			if ( actual_type != NOT_VALID )
				++M;
		}
	if ( N > M )
		N = M;
#if 0
printf( "Selecting %d sites out of %d for '%c'.\n", N, M, terr2symbol(type) );
#endif
	for ( y=ymin ; y<ymax ; y++ )
		for ( x=xmin ; x<=xmax ; x++ )
		{
			actual_type = GETMAP( tmpmap, x, y );
			if ( actual_type != NOT_VALID )
			{
				if ( (random() % (M - m)) < (N - n) )
				{
					SETMAP( map, x, y, actual_type);
					++n;
				}
				++m;
			}
		}
	return( n );
}

static void bad_option( arg )
char *arg;
{
	fprintf( stderr, "%s: Option %s?\n", progname, arg );
	exit( 1 );
}

main( argc, argv )
int argc;
char **argv;
{
	int i, j, k;
	int x, y;
	FILE *fp;
	int n, N, m, M;

	progname = argv[0];
	for ( i=1 ; i<argc ; i++ )
	{
		if ( argv[i][0] != '-' )
			bad_option( argv[i] );
		switch ( argv[i][1] )
		{
			case 'v': verbose = atoi( &argv[i][2] ); break;
			case 'w': wd = atoi( &argv[i][2] ); break;
			case 'h': ht = atoi( &argv[i][2] ); break;
			case 's': seed = atoi( &argv[i][2] ); break;
			case 'e': edgewrap = atoi( &argv[i][2] ); break;
			case 'a': author = &argv[i][2]; break;
			case 'm': mapname = &argv[i][2]; break;
			case 'o': fn = &argv[i][2]; break;
			case 'g':
				switch ( argv[i][2] )
				{
					case 's': seed_prob = atoi( &argv[i][3] ); break;
					case 'g': grow_prob = atoi( &argv[i][3] ); break;
					case 'p': grow_pass = atoi( &argv[i][3] ); break;
					default: bad_option( argv[i] );
				}
				break;
			case 'd':
				for ( j=0 ; ; j++ )
				{
					if ( j == NTERRAIN )
						bad_option( argv[i] );
					if ( terrain[j].density == -1 )
						continue;
					if ( strncmp( &argv[i][2], terrain[j].name,
						strlen(terrain[j].name) ) )
						continue;
					terrain[j].density =
						atoi( &argv[i][2+strlen(terrain[j].name)] );
					break;
				}
				break;
			default: bad_option( argv[i] );
		}
	}
	if ( seed == -1 )
		seed = time( (time_t *)0 );
	printf( "Random number seed = %d\n", seed );
	srandom( seed );

	sz = wd * ht;
	map = (char **)calloc( ht, sizeof(char *) );
	for ( y=0 ; y<ht ; y++ )
	{
		map[y] = (char *)malloc( wd );
		memset( map[y], SPACE, wd );
	}
	tmpmap = (char **)calloc( ht, sizeof(char *) );
	for ( y=0 ; y<ht ; y++ )
	{
		tmpmap[y] = (char *)malloc( wd );
		memset( tmpmap[y], SPACE, wd );
	}

	/* make border */
	if ( !edgewrap )
	{
		for ( x=0 ; x<wd ; x++ )
		{
			SETMAP( map, x, 0, FILLED );
			SETMAP( map, x, ht-1, FILLED );
		}
		for ( y=0 ; y<ht ; y++ )
		{
			SETMAP( map, 0, y, FILLED );
			SETMAP( map, wd-1, y, FILLED );
		}
	}

	/* plant seeds */
	for ( y=0 ; y<ht ; y++ )
		for ( x=0 ; x<wd ; x++ )
			if ( (random() % seed_prob) == 0 )
				SETMAP( map, x, y, FILLED );

	/* grow seeds */
	for ( k=0 ; k<grow_pass ; k++ )
	{
		for ( y=1 ; y<ht-1 ; y++ )
			for ( x=1 ; x<wd-1 ; x++ )
				SETMAP( tmpmap, x, y, GETMAP(map,x,y) );
		for ( y=1 ; y<ht-1 ; y++ )
			for ( x=1 ; x<wd-1 ; x++ )
			{
				int i, j;

				if ( GETMAP(map,x,y) != FILLED )
					continue;
				for ( j= -1 ; j<=1 ; j++ )
					for ( i= -1 ; i<=1 ; i++ )
					{
						if ( (i == 0) && (j == 0) )
							continue;
						if ( (GETMAP(map,x+i,y+j) != FILLED) &&
							((random() % grow_prob) == 0) )
							SETMAP( tmpmap, x+i, y+j, FILLED );
					}
			}
		for ( y=1 ; y<ht-1 ; y++ )
			for ( x=1 ; x<wd-1 ; x++ )
				SETMAP( map, x, y, GETMAP(tmpmap,x,y) );
	}

	for ( y=1 ; y<ht-1 ; y++ )
		for ( x=1 ; x<wd-1 ; x++ )
		{
			int n, s, e, w;

			if ( GETMAP(map,x,y) != SPACE )
				continue;
			n = GETMAP(map,x,y-1);
			s = GETMAP(map,x,y+1);
			w = GETMAP(map,x-1,y);
			e = GETMAP(map,x+1,y);

			/* fill SPACEs that are completely surrounded by non-SPACEs */
			if ( (n!=SPACE) && (s!=SPACE) && (e!=SPACE) && (w!=SPACE) )
			{
				SETMAP( map, x, y, FILLED );
				continue;
			}

			/* place half-filleds */
			if ( (s==SPACE) && (e==SPACE) && (n==FILLED) && (w==FILLED) )
				SETMAP( map, x, y, (random() % 2) ? NW_FILLED : SPACE );
			if ( (s==SPACE) && (w==SPACE) && (n==FILLED) && (e==FILLED) )
				SETMAP( map, x, y, (random() % 2) ? NE_FILLED : SPACE );
			if ( (n==SPACE) && (w==SPACE) && (s==FILLED) && (e==FILLED) )
				SETMAP( map, x, y, (random() % 2) ? SE_FILLED : SPACE );
			if ( (n==SPACE) && (e==SPACE) && (s==FILLED) && (w==FILLED) )
				SETMAP( map, x, y, (random() % 2) ? SW_FILLED : SPACE );
		}

	if ( verbose )
		printf( "%d by %d world, %d total squares.\n", wd, ht, sz );
	for ( i=0 ; i<NTERRAIN ; i++ )
	{
		if ( terrain[i].density == -1 )
			continue;
		terrain[i].n = (sz * terrain[i].density) / DENSITY_FACTOR +
			(random()%3)-1;
		if ( (terrain[i].nmin != -1) && (terrain[i].n < terrain[i].nmin) )
			terrain[i].n = terrain[i].nmin;
		if ( (terrain[i].nmax != -1) && (terrain[i].n > terrain[i].nmax) )
			terrain[i].n = terrain[i].nmax;
		terrain[i].n = random_distribute( terrain[i].n, terrain[i].type,
			terrain[i].valid,
			terrain[i].xminborder, terrain[i].yminborder,
			wd-terrain[i].xmaxborder-1, ht-terrain[i].ymaxborder-1 );
		if ( verbose )
			printf( "%14s:%4d\n", terrain[i].name, terrain[i].n );
	}

	fp = fopen( fn, "w" );
	if ( fp == 0 )
	{
		fprintf( stderr, "%s: Can't open '%s' for writing. %s.\n",
			progname, fn, strerror( errno ) );
		exit( 1 );
	}
	fprintf( fp, "mapWidth:            %d\n", wd );
	fprintf( fp, "mapHeight:           %d\n", ht );
	fprintf( fp, "mapName:             " );
	fprintf( fp, mapname, seed );
	fputc( '\n', fp );
	fprintf( fp, "mapAuthor:           %s\n", author );
	fprintf( fp, "teamPlay:            no\n" );
	fprintf( fp, "timing:              no\n" );
	fprintf( fp, "edgeWrap:            %s\n", edgewrap?"yes":"no" );
	fprintf( fp, "mapData: \\multiline: EndOfMapdata\n" );
	for ( y=0 ; y<ht ; y++ )
	{
		for ( x=0 ; x<wd ; x++ )
			fputc( terr2symbol( GETMAP( map, x, y ) ), fp );
		fputc( '\n', fp );
	}
	fprintf( fp, "EndOfMapdata\n" );
	fclose( fp );
	exit( 0 );
}
