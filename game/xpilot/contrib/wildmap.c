/*
 * Wildmap, a random map generator for XPilot.  Copyright (C) 1993 by
 *
 *      Bjørn Stabell        (bjoerns@staff.cs.uit.no)
 *      Ken Ronny Schouten   (kenrsc@stud.cs.uit.no)
 *      Bert Gÿsbers         (bert@mc.bio.uva.nl)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <time.h>
#include <string.h>

#define NELEM(a)	(sizeof(a) / sizeof(a[0]))

#define FUZZ_MASK	0xFFFFFFFFU
#define MAX_FUZZ	FUZZ_MASK

#define MAPOFFLEFT(i)	(((i) >= 1) ? (i) - 1 : map.datasize - 1)
#define MAPOFFRIGHT(i)	(((i) + 1 < map.datasize) ? (i) + 1 : 0)
#define MAPOFFUP(i)	(((i) >= map.linewidth) \
				? (i) - map.linewidth \
				: (i) - map.linewidth + map.datasize)
#define MAPOFFDOWN(i)	(((i) + map.linewidth < map.datasize) \
				? (i) + map.linewidth \
				: (i) + map.linewidth - map.datasize)
#define MAPLEFT(i)	map.data[MAPOFFLEFT(i)]
#define MAPRIGHT(i)	map.data[MAPOFFRIGHT(i)]
#define MAPUP(i)	map.data[MAPOFFUP(i)]
#define MAPDOWN(i)	map.data[MAPOFFDOWN(i)]

#define BLOCK_SPACE		' '
#define BLOCK_SOLID		'x'
#define BLOCK_UP_RIGHT		'a'
#define BLOCK_UP_LEFT		's'
#define BLOCK_DOWN_LEFT		'w'
#define BLOCK_DOWN_RIGHT	'q'
#define BLOCK_CANNON_RIGHT	'd'
#define BLOCK_CANNON_DOWN	'r'
#define BLOCK_CANNON_LEFT	'f'
#define BLOCK_CANNON_UP		'c'
#define BLOCK_FUEL		'#'
#define BLOCK_GRAV_POS		'+'
#define BLOCK_GRAV_NEG		'-'
#define BLOCK_GRAV_CLOCK	'>'
#define BLOCK_GRAV_ANTI_CLOCK	'<'
#define BLOCK_WORM_BOTH		'@'
#define BLOCK_WORM_IN		'('
#define BLOCK_WORM_OUT		')'

struct map {
    unsigned		seed,
			fuzz_word;
    char		*data;
    double		fill_ratio,
			seed_ratio,
			cannon_ratio,
			fuel_ratio,
			grav_ratio,
			worm_ratio;
    int			width,
			height,
			linewidth,
			datasize,
			num_bases,
			num_teams,
			flood_marker,
			flood_trigger,
			flood_count[256];
};

static struct map	map;

static unsigned fuzz(void)
{
    /*
     * Implement a private pseudo-random generator to be able
     * to regenerate the same map from the same seed on
     * different architectures.
     * It assumes an unsigned int with at least 32 bits (as I do always).
     * The magic constant used below is from Sedgewick's Algorithms.
     * Note that the low order digits are not random at all :(.
     */
    map.fuzz_word = (map.fuzz_word * 31415821 + 1) & FUZZ_MASK;

    return map.fuzz_word;
}

static unsigned fuzz_bound(unsigned max)
{
    /*
     * Return a pseudo-random unsigned integer
     * in the range: [0-max] inclusive.
     */
    return (unsigned)(((double) fuzz() * (double) max) / (double) MAX_FUZZ);
}

static void Default_map(void)
{
    map.width = 150;
    map.height = 150;
    map.seed = (unsigned)getpid() ^ (unsigned)time(NULL) * (unsigned)getppid();
    map.seed_ratio = 0.19;
    map.fill_ratio = 0.18;
    map.num_bases = 16;
    map.num_teams = 3;
    map.cannon_ratio = 0.0020;
    map.fuel_ratio   = 0.0006;
    map.grav_ratio   = 0.0006;
    map.worm_ratio   = 0.0002;
}

static void Option_map(int argc, char **argv)
{
    struct map_opt {
	char		*name;
	int		*intp;
	unsigned	*unsp;
	double		*dblp;
	double		min;
	double		max;
    };
    static struct map_opt mapopts[] = {
	{ "mapWidth", &map.width, 0, 0, 90, 936 },
	{ "mapHeight", &map.height, 0, 0, 90, 936 },
	{ "seed", 0, &map.seed, 0, 0, MAX_FUZZ },
	{ "seedRatio", 0, 0, &map.seed_ratio, 0.02, 0.22 },
	{ "fillRatio", 0, 0, &map.fill_ratio, 0.02, 0.80 },
	{ "numBases", &map.num_bases, 0, 0, 2, 50 },
	{ "numTeams", &map.num_teams, 0, 0, 3, 10 },
	{ "cannonRatio", 0, 0, &map.cannon_ratio, 0.0, 0.2 },
	{ "fuelRatio", 0, 0, &map.fuel_ratio, 0.0, 0.1 },
	{ "gravRatio", 0, 0, &map.grav_ratio, 0.0, 0.1 },
	{ "wormRatio", 0, 0, &map.worm_ratio, 0.0, 0.1 }
    };
    int			i,
			j,
			intval;
    unsigned		unsval;
    char		*opt,
			*arg = NULL;
    double		dblval;

    for (i = 1; i < argc; i += 2) {
	opt = argv[i];
	arg = NULL;
	if (*opt != '-') {
	    break;
	}
	for (j = 0; j < NELEM(mapopts); j++) {
	    if (strcmp(&opt[1], mapopts[j].name) == 0) {
		break;
	    }
	}
	if (j == NELEM(mapopts)) {
	    break;
	}
	if (i + 1 == argc) {
	    break;
	}
	arg = argv[i + 1];
	if (mapopts[j].intp) {
	    if (sscanf(arg, "%d", &intval) != 1) {
		break;
	    }
	    if (intval < mapopts[j].min || intval > mapopts[j].max) {
		break;
	    }
	    *mapopts[j].intp = intval;
	}
	else if (mapopts[j].unsp) {
	    if (sscanf(arg, "%u", &unsval) != 1) {
		break;
	    }
	    if (unsval < mapopts[j].min || unsval > mapopts[j].max) {
		break;
	    }
	    *mapopts[j].unsp = unsval;
	}
	else if (mapopts[j].dblp) {
	    if (sscanf(arg, "%lf", &dblval) != 1) {
		break;
	    }
	    if (dblval < mapopts[j].min || dblval > mapopts[j].max) {
		break;
	    }
	    *mapopts[j].dblp = dblval;
	}
    }
    if (i < argc) {
	if (strcmp(opt, "-help")) {
	    fprintf(stderr, "Invalid option: %s %s\n", opt, arg ? arg : "");
	}
	fprintf(stderr, "Usage: %s [-option value] ...\n", *argv);
	fprintf(stderr, "Where options include:\n");
	for (i = 0; i < NELEM(mapopts); i++) {
	    fprintf(stderr, "    -%-11s <value>\t", mapopts[i].name);
	    if (mapopts[i].intp) {
		fprintf(stderr, "(= %d, min: %d, max: %d)",
		    (int)*mapopts[i].intp,
		    (int)mapopts[i].min, (int)mapopts[i].max);
	    }
	    else if (mapopts[i].unsp) {
		fprintf(stderr, "(= %u, min: %u, max: %u)",
		    (unsigned)*mapopts[i].unsp,
		    (unsigned)mapopts[i].min, (unsigned)mapopts[i].max);
	    } else {
		fprintf(stderr, "(= %-6.4f, min: %-6.4f, max: %-6.4f)",
		    (double)*mapopts[i].dblp,
		    (double)mapopts[i].min, (double)mapopts[i].max);
	    }
	    fprintf(stderr, "\n");
	}
	fprintf(stderr,
"SeedRatio is the chance that a map object starts as solid instead of empty.\n"
"FillRatio is the amount of wall space divided by the total map size.\n"
"Seed is the seed for the random number generator and determines the map\n"
"layout uniquely given the values for seedRatio, fillRatio and the map size.\n"
"To get thick walls increase seedRatio.\n"
"For maps with lots of space to fly in decrease fillRatio.\n"
"\n");
	exit(1);
    }
}

static void Alloc_map(void)
{
    /*
     * Allocate the map data.
     * The (0, 0) coordinate is in the top left corner.
     * An extra column is appended at the far right, which
     * will later be turned into newlines for easy printing.
     */
    map.fuzz_word = map.seed;
    map.linewidth = map.width + 1;
    map.datasize = map.linewidth * map.height;
    map.data = (char *) malloc(map.datasize + 1);
    if (!map.data) {
	printf("no mem\n");
	exit(1);
    }
}

static void Flood_map(int i)
{
    /*
     * Connect all adjacent (up, down, left, right) map objects,
     * which have the same type as map.flood_trigger.
     * The map objects will be marked with value map.flood_marker,
     * to distinguish them from map objects which already have
     * been processed.
     */

#define INTARR_SIZE	1024

    struct int_arr {
	struct int_arr	*next;
	int		n;
	int		arr[INTARR_SIZE];
    };

    struct int_arr	intarr, *putp = &intarr, *getp, *tmpp;
    int			k, j;

    if (map.data[i] != map.flood_trigger) {
	return;
    }
    map.data[i] = map.flood_marker;
    putp->next = NULL;
    putp->n = 1;
    putp->arr[0] = i;

    for (getp = &intarr; getp != NULL; getp = tmpp) {

	while (getp->n > 0) {
	    k = getp->arr[--getp->n];
	    if (putp->n + 4 > INTARR_SIZE) {
		if ((putp->next = (struct int_arr *)
				  malloc(sizeof(struct int_arr))) == NULL) {
		    fprintf(stderr, "No mem\n");
		    exit(1);
		}
		putp = putp->next;
		putp->next = NULL;
		putp->n = 0;
	    }
	    j = MAPOFFUP(k);
	    if (map.data[j] == map.flood_trigger) {
		map.data[j] = map.flood_marker;
		putp->arr[putp->n++] = j;
	    }
	    j = MAPOFFLEFT(k);
	    if (map.data[j] == map.flood_trigger) {
		map.data[j] = map.flood_marker;
		putp->arr[putp->n++] = j;
	    }
	    j = MAPOFFDOWN(k);
	    if (map.data[j] == map.flood_trigger) {
		map.data[j] = map.flood_marker;
		putp->arr[putp->n++] = j;
	    }
	    j = MAPOFFRIGHT(k);
	    if (map.data[j] == map.flood_trigger) {
		map.data[j] = map.flood_marker;
		putp->arr[putp->n++] = j;
	    }
	}
	tmpp = getp->next;
	if (getp != &intarr) {
	    free(getp);
	}
    }
}

static void Generate_map(void)
{
    /* 
     * Initialize the map with noise.
     * Later the noise is either removed or connected,
     * depending upon the outcome of a randomizer.
     */

    int			todo, i;
    unsigned		edge;

    map.flood_trigger = 1;
    map.flood_marker = 2;

    edge = MAX_FUZZ * map.seed_ratio;
    for (todo = map.datasize; todo--; ) {
	map.data[todo] = (fuzz() < edge);
    }

    edge = (MAX_FUZZ / map.datasize);
    for (todo = map.datasize; todo--; ) {
	i = fuzz_bound(map.datasize - 1);
	if (map.data[i] == 0) {
	    if (MAPUP(i) == 1) MAPUP(i) = 0;
	    if (MAPLEFT(i) == 1) MAPLEFT(i) = 0;
	    if (MAPDOWN(i) == 1) MAPDOWN(i) = 0;
	    if (MAPRIGHT(i) == 1) MAPRIGHT(i) = 0;
	}
	else {
	    if (map.data[i] == 0) {
		map.data[i] = 1;
		Flood_map(i);
		map.data[i] = 1;
	    }
	    else if (map.data[i] == 1) {
		Flood_map(i);
		if (MAPUP(i) == 0) {
		    MAPUP(i) = 1;
		    Flood_map(MAPOFFUP(i));
		    MAPUP(i) = 1;
		}
		if (MAPLEFT(i) == 0) {
		    MAPLEFT(i) = 1;
		    Flood_map(MAPOFFLEFT(i));
		    MAPLEFT(i) = 1;
		}
		if (MAPDOWN(i) == 0) {
		    MAPDOWN(i) = 1;
		    Flood_map(MAPOFFDOWN(i));
		    MAPDOWN(i) = 1;
		}
		if (MAPRIGHT(i) == 0) {
		    MAPRIGHT(i) = 1;
		    Flood_map(MAPOFFRIGHT(i));
		    MAPRIGHT(i) = 1;
		}
	    }
	}
    }
}

static void Connect_map(void)
{
    /*
     * Connect small constructs to another if they are very close
     * to one another (1 or only 2 blocks apart).
     */

    char		*p0, *p1, *p2, *p3, *p4, *p5, *p6, *p7, *p8, *p9, *pa;
    char		*maxp;
    int			n;

    maxp = &map.data[map.datasize];

    do {
	n = 0;

	p0 = &map.data[map.datasize];
	p1 = p0 - 1;
	p2 = p0 - 2;
	p3 = p0 - 3;
	p4 = p0 - map.linewidth;
	p5 = p4 - 1;
	p6 = p4 - 2;
	p7 = p4 - map.linewidth;
	p8 = p7 - 1;
	p9 = p7 - 2;
	pa = p7 - map.linewidth;
	p0 = map.data;

	while (p0 < maxp) {
	    if (*p0) {
		if (!*p1) {
		    if (*p2) {
			*p1 = *p0;
			n++;
		    }
		    else if (*p3) {
			*p2 = *p1 = *p0;
			n++;
		    }
		}
		if (!*p4) {
		    if (*p7) {
			*p4 = *p0;
			n++;
		    }
		    else if (*pa) {
			*p7 = *p4 = *p0;
			n++;
		    }
		}
		if (*p5) {
		    if (!*p1 && !*p4) {
			*p4 = *p1 = *p0;
			n++;
		    }
		}
	    } else {
		if (!*p5 && *p1 && *p4) {
		    *p0 = *p5 = *p1;
		    n++;
		}
	    }
	    if (!*p5) {
		if ((*p0 || (*p1 && *p4)) && *p9) {
		    *p5 = *p0;
		    n++;
		}
		else if (*p6 && *p8 && *p0) {
		    *p5 = *p0;
		    n++;
		}
		else if (*p2 && (*p7 || (*p4 && *p8))) {
		    *p5 = *p2;
		    n++;
		}
		else if (*p7 && *p1 && *p6) {
		    *p5 = *p7;
		    n++;
		}
		else if (!*p0 && !*p1 && *p2
			&& !*p4 && *p6
			&& *p7 && *p8 && *p9) {
		    *p5 = *p9;
		    n++;
		}
		else if (*p0 && !*p1 && !*p2
			&& *p4 && !*p6
			&& *p7 && *p8 && *p9) {
		    *p5 = *p7;
		    n++;
		}
		else if (*p0 && *p1 && *p2
			&& *p4 && !*p6
			&& *p7 && !*p8 && !*p9) {
		    *p5 = *p0;
		    n++;
		}
		else if (*p0 && *p1 && *p2
			&& !*p4 && *p6
			&& !*p7 && !*p8 && *p9) {
		    *p5 = *p0;
		    n++;
		}
		else if (*p0 && !*p1 && !*p2
			&& !*p4 && !*p6
			&& !*p7 && *p8 && !*p9) {
		    *p5 = *p8;
		    n++;
		}
		else if (!*p0 && !*p1 && *p2
			&& !*p4 && !*p6
			&& !*p7 && *p8 && !*p9) {
		    *p5 = *p8;
		    n++;
		}
		else if (*p0 && !*p1 && !*p2
			&& !*p4 && *p6
			&& !*p7 && !*p8 && !*p9) {
		    *p5 = *p6;
		    n++;
		}
		else if (!*p0 && !*p1 && !*p2
			&& !*p4 && *p6
			&& *p7 && !*p8 && !*p9) {
		    *p5 = *p6;
		    n++;
		}
		else if (!*p0 && *p1 && !*p2
			&& !*p4 && !*p6
			&& !*p7 && !*p8 && *p9) {
		    *p5 = *p1;
		    n++;
		}
		else if (!*p0 && *p1 && !*p2
			&& !*p4 && !*p6
			&& *p7 && !*p8 && !*p9) {
		    *p5 = *p1;
		    n++;
		}
		else if (!*p0 && !*p1 && !*p2
			&& *p4 && !*p6
			&& !*p7 && !*p8 && *p9) {
		    *p5 = *p4;
		    n++;
		}
		else if (!*p0 && !*p1 && *p2
			&& *p4 && !*p6
			&& !*p7 && !*p8 && !*p9) {
		    *p5 = *p4;
		    n++;
		}
	    }
	    p3 = p2;
	    p2 = p1;
	    p1 = p0;
	    p0++;
	    p6 = p5;
	    p5 = p4;
	    if (++p4 >= maxp) {
		p4 = map.data;
	    }
	    p9 = p8;
	    p8 = p7;
	    if (++p7 >= maxp) {
		p7 = map.data;
	    }
	    if (++pa >= maxp) {
		pa = map.data;
	    }
	}

    } while (n > 0);
}

static void Count_labels(void)
{
    /*
     * For each possible map value count the number of occurences.
     */

    int			todo;

    memset(map.flood_count, 0, sizeof map.flood_count);

    for (todo = map.datasize; todo--; ) {
	map.flood_count[(unsigned char) map.data[todo]]++;
    }
}

static void Sort_labels(int tbl[256])
{
    /*
     * Sort the map to have big map constructs with low map values
     * close to zero and small map constructs with high map values.
     */

    int			i, j, n;

    Count_labels();

    memset(tbl, 0, sizeof(int *) * 256);

    tbl[0] = 0;
    tbl[1] = 1;
    n = 2;
    for (i = 2; i < 256; i++) {
	for (j = n; j > 2; j--) {
	    if (map.flood_count[i] > map.flood_count[tbl[j - 1]]) {
		tbl[j] = tbl[j - 1];
	    } else {
		break;
	    }
	}
	tbl[j] = i;
	n++;
    }
}

static void Label_map(int label)
{
    /*
     * Give each map construct an unique value depending on how
     * big it is.  It is OK to delete small constructs if we are
     * running out of label values.
     */

    int			todo,
			i,
			tbl[256],
			del[256];

    memset(tbl, 0, sizeof tbl);
    tbl[label] = 1;
    for (todo = map.datasize; todo--; ) {
	map.data[todo] = tbl[(unsigned char)map.data[todo]];
    }

    map.flood_trigger = 1;
    map.flood_marker = 2;

    for (todo = map.datasize; todo--; ) {
	if (map.data[todo] == map.flood_trigger) {
	    if (map.flood_marker >= 256) {
		Sort_labels(tbl);
		for (i = 0; i < 224; i++) {
		    del[tbl[i]] = i;
		}
		for (i = 224; i < 256; i++) {
		    del[tbl[i]] = 0;
		}
		for (i = map.datasize; i--; ) {
		    map.data[i] = del[(unsigned char)map.data[i]];
		}
		map.flood_marker = 224;
	    }
	    Flood_map(todo);
	    map.flood_marker++;
	}
    }
}

static void Partition_map(void)
{
    /*
     * Remove all the inaccessible holes from the map.
     * And remove enough small map constructs to meet our fillRatio.
     */

    int			todo,
			i,
			j,
			marker,
			count,
			excess,
			tbl[256],
			del[256];

    Label_map(0);
    Count_labels();
    marker = 0;
    for (i = 2; i < 256; i++) {
	if (map.flood_count[i] > map.flood_count[marker]) {
	    marker = i;
	}
    }
    memset(tbl, 0, sizeof tbl);
    tbl[marker] = 1;
    for (todo = map.datasize; todo--; ) {
	map.data[todo] = tbl[(unsigned char)map.data[todo]];
    }

    Label_map(0);
    Sort_labels(tbl);
    for (i = 0; i < 256; i++) {
	del[tbl[i]] = tbl[i];
    }
    count = 0;
    excess = map.datasize - map.flood_count[0]
	- (int)(map.fill_ratio * map.datasize + 0.5);
    for (i = 256; i-- > 2; ) {
	if (excess < count) {
	    break;
	}
	count += map.flood_count[tbl[i]];
    }
    if (++i < 256) {
	for (j = i + 1; j < 256; j++) {
	    if (count - map.flood_count[tbl[j]] > excess) {
		if (2 * map.flood_count[tbl[j]] >= map.flood_count[tbl[i]]) {
		    continue;
		}
	    }
	    del[tbl[j]] = 0;
	}
	del[tbl[i]] = 0;
	for (todo = map.datasize; todo--; ) {
	    map.data[todo] = del[(unsigned char)map.data[todo]];
	}
    }
}

static void Smooth_map(void)
{
    /*
     * Round all sharp edges and corners.
     */

    char		*p0, *p1, *p2, *p3, *p4, *p5, *p6, *p7, *p8;
    char		*maxp;
    int			todo, n;

    for (todo = map.datasize; todo--; ) {
	map.data[todo] = (map.data[todo] ? BLOCK_SOLID : BLOCK_SPACE);
    }

    maxp = &map.data[map.datasize];

    do {
	n = 0;

	p0 = &map.data[map.datasize];
	p1 = p0 - 1;
	p2 = p0 - 2;
	p3 = p0 - map.linewidth;
	p4 = p3 - 1;
	p5 = p4 - 2;
	p6 = p3 - map.linewidth;
	p7 = p6 - 1;
	p8 = p7 - 2;
	p0 = map.data;

	while (p0 < maxp) {

	    if (*p4 == BLOCK_SPACE || *p4 == BLOCK_SOLID) {
		if (*p0 == BLOCK_SOLID && *p1 == BLOCK_SOLID
		    && *p3 == BLOCK_SOLID && *p5 == BLOCK_SPACE
		    && *p7 == BLOCK_SPACE && *p8 == BLOCK_SPACE) {
		    *p4 = BLOCK_DOWN_RIGHT;
		    n++;
		}
		else if (*p1 == BLOCK_SOLID && *p2 == BLOCK_SOLID
		    && *p3 == BLOCK_SPACE && *p5 == BLOCK_SOLID
		    && *p6 == BLOCK_SPACE && *p7 == BLOCK_SPACE) {
		    *p4 = BLOCK_DOWN_LEFT;
		    n++;
		}
		else if (*p0 == BLOCK_SPACE && *p1 == BLOCK_SPACE
		    && *p3 == BLOCK_SPACE && *p5 == BLOCK_SOLID
		    && *p7 == BLOCK_SOLID && *p8 == BLOCK_SOLID) {
		    *p4 = BLOCK_UP_LEFT;
		    n++;
		}
		else if (*p1 == BLOCK_SPACE && *p2 == BLOCK_SPACE
		    && *p3 == BLOCK_SOLID && *p5 == BLOCK_SPACE
		    && *p6 == BLOCK_SOLID && *p7 == BLOCK_SOLID) {
		    *p4 = BLOCK_UP_RIGHT;
		    n++;
		}
		else if (*p4 == BLOCK_SOLID) {
		    if (*p5 == BLOCK_SPACE && *p8 == BLOCK_SPACE
			&& *p7 == BLOCK_SPACE
			&& *p6 == BLOCK_SPACE && *p3 == BLOCK_SPACE) {
			if (*p2 != BLOCK_SOLID && *p0 != BLOCK_SOLID) {
			    *p4 = BLOCK_SPACE;
			    n++;
			}
		    }
		    else if (*p7 == BLOCK_SPACE && *p6 == BLOCK_SPACE
			&& *p3 == BLOCK_SPACE
			&& *p0 == BLOCK_SPACE && *p1 == BLOCK_SPACE) {
			if (*p8 != BLOCK_SOLID && *p2 != BLOCK_SOLID) {
			    *p4 = BLOCK_SPACE;
			    n++;
			}
		    }
		    else if (*p3 == BLOCK_SPACE && *p0 == BLOCK_SPACE
			&& *p1 == BLOCK_SPACE
			&& *p2 == BLOCK_SPACE && *p5 == BLOCK_SPACE) {
			if (*p6 != BLOCK_SOLID && *p8 != BLOCK_SOLID) {
			    *p4 = BLOCK_SPACE;
			    n++;
			}
		    }
		    else if (*p1 == BLOCK_SPACE && *p2 == BLOCK_SPACE
			&& *p5 == BLOCK_SPACE
			&& *p8 == BLOCK_SPACE && *p7 == BLOCK_SPACE) {
			if (*p0 != BLOCK_SOLID && *p6 != BLOCK_SOLID) {
			    *p4 = BLOCK_SPACE;
			    n++;
			}
		    }
		}
	    }

	    p2 = p1;
	    p1 = p0;
	    p0++;
	    p5 = p4;
	    p4 = p3;
	    if (++p3 >= maxp) {
		p3 = map.data;
	    }
	    p8 = p7;
	    p7 = p6;
	    if (++p6 >= maxp) {
		p6 = map.data;
	    }
	}

    } while (n > 0);
}

static void Decorate_map(void)
{
    /*
     * Add the cannons, fuelstations, homebases,
     * wormholes and gravity objects.
     * A homebase should be free from influence from gravity objects.
     */

    struct xy {
	int		x,
			y;
    };

    int			margin,
			h,
			i,
			hori,
			vert,
			off,
			x,
			y,
			base,
			cannon,
			num_cannons,
			grav,
			num_gravs,
			fuel,
			num_fuels,
			worm,
			num_worms,
			type,
			wall_offset,
			team,
			tries;
    struct xy		*home;
    unsigned		size;

    size = map.num_bases * sizeof(struct xy);
    if ((home = (struct xy *) malloc(size)) == NULL) {
	fprintf(stderr, "No mem\n");
	exit(1);
    }
    margin = 2;
    if (map.num_teams > map.num_bases) {
	map.num_teams = map.num_bases;
    }
    team = map.num_teams;
    for (i = 0; i < map.num_bases; i++) {
	for (tries = map.datasize; tries; tries--) {
	    x = margin + fuzz_bound(map.width - 2*margin - 1);
	    y = margin + fuzz_bound(map.height - 2*margin - 1);
	    base = x + y * map.linewidth;
	    if (map.data[base] != BLOCK_SPACE) {
		continue;
	    }
	    if (map.data[base + map.linewidth] != BLOCK_SOLID) {
		continue;
	    }
	    if (map.data[base - map.linewidth] != BLOCK_SPACE) {
		continue;
	    }
	    if (map.data[base - 2 * map.linewidth] != BLOCK_SPACE) {
		continue;
	    }
	    if (map.data[base - 1] != BLOCK_SPACE) {
		continue;
	    }
	    if (map.data[base + 1] != BLOCK_SPACE) {
		continue;
	    }
	    if (--team < 0) {
		team = map.num_teams - 1;
	    }
	    map.data[base] = '0' + team;
	    home[i].x = x;
	    home[i].y = y;
	    break;
	}
	if (tries == 0) {
	    break;
	}
    }

    num_cannons = map.cannon_ratio * map.datasize;
    margin = 1;
    for (i = 0; i < num_cannons; i++) {
	switch (fuzz_bound(3)) {
	case 0:
	    type = BLOCK_CANNON_RIGHT;
	    wall_offset = 1;
	    break;
	case 1:
	    type = BLOCK_CANNON_DOWN;
	    wall_offset = map.linewidth;
	    break;
	case 2:
	    type = BLOCK_CANNON_LEFT;
	    wall_offset = -1;
	    break;
	default:
	    type = BLOCK_CANNON_UP;
	    wall_offset = -map.linewidth;
	    break;
	}
	for (tries = map.datasize; tries; tries--) {
	    x = margin + fuzz_bound(map.width - 2*margin - 1);
	    y = margin + fuzz_bound(map.height - 2*margin - 1);
	    cannon = x + y * map.linewidth;
	    if (map.data[cannon] != BLOCK_SPACE) {
		continue;
	    }
	    if (map.data[cannon + wall_offset] == BLOCK_SPACE) {
		continue;
	    }
	    if (map.data[cannon + wall_offset] != BLOCK_SOLID) {
		switch (type) {
		case BLOCK_CANNON_RIGHT:
		    if (map.data[cannon + wall_offset] != BLOCK_UP_LEFT &&
			map.data[cannon + wall_offset] != BLOCK_DOWN_LEFT) {
			continue;
		    }
		    break;
		case BLOCK_CANNON_DOWN:
		    if (map.data[cannon + wall_offset] != BLOCK_UP_LEFT &&
			map.data[cannon + wall_offset] != BLOCK_UP_RIGHT) {
			continue;
		    }
		    break;
		case BLOCK_CANNON_LEFT:
		    if (map.data[cannon + wall_offset] != BLOCK_UP_RIGHT &&
			map.data[cannon + wall_offset] != BLOCK_DOWN_RIGHT) {
			continue;
		    }
		    break;
		default:
		    if (map.data[cannon + wall_offset] != BLOCK_DOWN_LEFT &&
			map.data[cannon + wall_offset] != BLOCK_DOWN_LEFT) {
			continue;
		    }
		    break;
		}
	    }
	    for (h = 0; h < map.num_bases; h++) {
		if (((x < home[h].x)
			? (x + margin < home[h].x)
			: (x - margin > home[h].x))
		    && ((y < home[h].y)
			? (y + margin < home[h].y)
			: (y - margin > home[h].y))) {
		    continue;
		}
		break;
	    }
	    if (h < map.num_bases) {
		continue;
	    }
	    map.data[cannon] = type;
	    break;
	}
	if (tries == 0) {
	    break;
	}
    }

    num_fuels = map.fuel_ratio * map.datasize;
    margin = 1;
    for (i = 0; i < num_fuels; i++) {
	for (tries = map.datasize; tries; tries--) {
	    x = margin + fuzz_bound(map.width - 2*margin - 1);
	    y = margin + fuzz_bound(map.height - 2*margin - 1);
	    fuel = x + y * map.linewidth;
	    if (map.data[fuel] != BLOCK_SOLID) {
		continue;
	    }
	    if (map.data[fuel + 1] != BLOCK_SPACE &&
		map.data[fuel - 1] != BLOCK_SPACE &&
		map.data[fuel + map.linewidth] != BLOCK_SPACE &&
		map.data[fuel - map.linewidth] != BLOCK_SPACE) {
		continue;
	    }
	    map.data[fuel] = BLOCK_FUEL;
	    break;
	}
	if (tries == 0) {
	    break;
	}
    }

    margin = 11;
    num_gravs = map.grav_ratio * map.datasize;
    for (i = 0; i < num_gravs; i++) {
	switch (fuzz_bound(3)) {
	case 0:
	    type = BLOCK_GRAV_POS;
	    break;
	case 1:
	    type = BLOCK_GRAV_NEG;
	    break;
	case 2:
	    type = BLOCK_GRAV_CLOCK;
	    break;
	default:
	    type = BLOCK_GRAV_ANTI_CLOCK;
	    break;
	}
	for (tries = map.datasize; tries; tries--) {
	    x = margin + fuzz_bound(map.width - 2*margin - 1);
	    y = margin + fuzz_bound(map.height - 2*margin - 1);
	    grav = x + y * map.linewidth;
	    if (map.data[grav] != BLOCK_SPACE ||
		map.data[grav + 1] != BLOCK_SPACE ||
		map.data[grav - 1] != BLOCK_SPACE ||
		map.data[grav + map.linewidth] != BLOCK_SPACE ||
		map.data[grav - map.linewidth] != BLOCK_SPACE ||
		map.data[grav + map.linewidth + 1] != BLOCK_SPACE ||
		map.data[grav - map.linewidth + 1] != BLOCK_SPACE ||
		map.data[grav + map.linewidth - 1] != BLOCK_SPACE ||
		map.data[grav - map.linewidth - 1] != BLOCK_SPACE) {
		continue;
	    }

	    for (h = 0; h < map.num_bases; h++) {
		if ((x < home[h].x)
			? (x + margin < home[h].x)
			: (x - margin > home[h].x)) {
		    continue;
		}
		if ((y < home[h].y)
			? (y + margin < home[h].y)
			: (y - margin > home[h].y)) {
		    continue;
		}
		break;
	    }
	    if (h < map.num_bases) {
		continue;
	    }
	    map.data[grav] = type;
	    break;
	}
	if (tries == 0) {
	    break;
	}
    }

    margin = 3;
    num_worms = map.worm_ratio * map.datasize;
    for (i = 0; i < num_worms; i++) {
	switch (fuzz_bound(3)) {
	case 0:
	    type = BLOCK_WORM_IN;
	    break;
	case 1:
	    type = BLOCK_WORM_OUT;
	    break;
	default:
	    type = BLOCK_WORM_BOTH;
	    break;
	}
	for (tries = map.datasize; tries; tries--) {
	    x = margin + fuzz_bound(map.width - 2*margin - 1);
	    y = margin + fuzz_bound(map.height - 2*margin - 1);
	    worm = x + y * map.linewidth;
	    if (map.data[worm] != BLOCK_SPACE) {
		continue;
	    }

	    for (vert = -margin; vert <= margin; vert++) {
		off = x - margin + (y + vert) * map.linewidth;
		for (hori = -margin; hori <= margin; hori++, off++) {
		    switch (map.data[off]) {
		    case BLOCK_SPACE:
		    case BLOCK_GRAV_POS:
		    case BLOCK_GRAV_NEG:
		    case BLOCK_GRAV_CLOCK:
		    case BLOCK_GRAV_ANTI_CLOCK:
		    case BLOCK_WORM_BOTH:
		    case BLOCK_WORM_IN:
		    case BLOCK_WORM_OUT:
			continue;
		    default:
			break;
		    }
		    break;
		}
		if (hori <= margin) {
		    break;
		}
	    }
	    if (vert <= margin) {
		continue;
	    }

	    for (h = 0; h < map.num_bases; h++) {
		if ((x < home[h].x)
			? (x + margin < home[h].x)
			: (x - margin > home[h].x)) {
		    continue;
		}
		if ((y < home[h].y)
			? (y + margin < home[h].y)
			: (y - margin > home[h].y)) {
		    continue;
		}
		break;
	    }
	    if (h < map.num_bases) {
		continue;
	    }
	    map.data[worm] = type;
	    break;
	}
	if (tries == 0) {
	    break;
	}
    }

    free(home);
}

static void Print_map(void)
{
    /*
     * Output the map in XPilot 2.0 map format.
     */

    int			i;
    char		*left, *middle, *right;

    left = &map.data[map.width - 1];
    middle = left + 1;
    right = &map.data[0];
    for (i = 0; i < map.height; i++) {
	*middle = '\n';
	if (*right == BLOCK_SOLID) {
	    if (*left == BLOCK_DOWN_LEFT || *left == BLOCK_UP_LEFT) {
		*left = BLOCK_SOLID;
	    }
	}
	if (*right == BLOCK_SPACE
	    || *right == BLOCK_DOWN_RIGHT
	    || *right == BLOCK_UP_RIGHT) {
	    if (*left == BLOCK_DOWN_RIGHT || *left == BLOCK_UP_RIGHT) {
		*left = BLOCK_SPACE;
	    }
	}
	if (*left == BLOCK_SOLID) {
	    if (*right == BLOCK_DOWN_RIGHT || *right == BLOCK_UP_RIGHT) {
		*right = BLOCK_SOLID;
	    }
	}
	if (*left == BLOCK_SPACE
	    || *left == BLOCK_UP_LEFT
	    || *left == BLOCK_DOWN_LEFT) {
	    if (*right == BLOCK_DOWN_LEFT || *right == BLOCK_UP_LEFT) {
		*right = BLOCK_SPACE;
	    }
	}
	left += map.linewidth;
	middle += map.linewidth;
	right += map.linewidth;
    }
    map.data[map.datasize] = '\0';

    printf("mapWidth:	%d\n", map.width);
    printf("mapHeight:	%d\n", map.height);
    printf("mapName:	Wild Map %u\n", map.seed);
    printf("mapAuthor:	The Wild Map Generator\n");
    printf("edgeWrap:	True\n");
    printf("mapData:	\\multiline: EndOfMapData\n");
    printf("%sEndOfMapData\n", map.data);

    free(map.data);
}

int main(int argc, char **argv)
{
    Default_map();
    Option_map(argc, argv);
    Alloc_map();
    Generate_map();
    Connect_map();
    Partition_map();
    Smooth_map();
    Decorate_map();
    Print_map();
 
    return 0;
}
