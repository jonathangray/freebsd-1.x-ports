/*
 * Name: mapmapper
 * Author: Bert Gÿsbers (bert@mc.bio.uva.nl)
 * Date: 28 March 93
 * Version: 1.2
 * Description: Maps the Xpilot 1.2 maps to Xpilot 2.0 format.
 * Usage: mapmapper [ -f ] [ 1.2-map [ 2.0-map ] ]
 * If the output filename or the input filename is missing,
 * then standard output and/or standard input are used.
 * Options: -f: don't ask silly questions if the output file already exists.
 *
 * Changes:
 * Added more informative messages for lines which don't have enough map data.
 *
 * 1.2 added conversion for In and Out wormholes, previously only normal
 * wormholes were handled.  Pointed out by Tomas Kristensen (tk@iesd.auc.dk)
 * Blasters are now converted to filled blocks instead of spaces.
 */

#ifndef	lint
static char sourceid[] =
    "@(#)$Id: mapmapper.c,v 1.1 1994/02/23 14:40:16 jkh Exp $";
static char versionid[] =
    "@(#)Version 1.2";
#endif

#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#define NUM_RULES	6
#define HAS_RULE(n,r)	((rules[n].rules & r) == r)
#define BOOL_RULE(n,r)	(HAS_RULE(n,r) ? "yes" : "no")
#define YN(r)		BOOL_RULE(map->rule,(r))
#define RULE_LIVES(n)	(rules[n].lives)

#define CRASH_WITH_PLAYER   (1<<0)
#define PLAYER_KILLINGS	    (1<<1)
#define LIMITED_LIVES	    (1<<2)
#define TIMING		    (1<<3)
#define ONE_PLAYER_ONLY	    (1<<4)
#define PLAYER_SHIELDING    (1<<6)
#define LIMITED_VISIBILITY  (1<<7)
#define TEAM_PLAY	    (1<<8)

struct map {
    int x, y;
    int rule;
    char *name;
    char *author;
    unsigned char *data;
};

static void *alloc_memory (unsigned size)
{
    void *p;

    if ((p = malloc (size)) == NULL)
    {
	perror ("malloc");
	exit (1);
    }
    return p;
}

static char *new_string (char *str)
{
    char *newstr;

    newstr = (char *) alloc_memory (strlen (str) + 1);
    strcpy (newstr, str);

    return newstr;
}

static void next_line (char *buf, unsigned size, FILE *fp)
{
    if (fgets (buf, size, fp) == NULL)
    {
	fprintf (stderr, "Unexpected end of file\n");
	exit (1);
    }
}

static void strip_newline (char *str)
{
    char *nl;

    if ((nl = strchr (str, '\n')) == NULL)
    {
	fprintf (stderr, "Line too long\n");
	exit (1);
    }
    *nl = '\0';
}

static void read_old_map (FILE *fp, struct map *map)
{
    int c, i, x, y;
    char buf[4096];

    next_line (buf, sizeof buf, fp);
    if (sscanf (buf, "%dx%d\n", &map->x, &map->y) != 2)
    {
	fprintf (stderr, "Can't get map dimensions\n");
	exit (1);
    }

    next_line (buf, sizeof buf, fp);
    map->rule = atoi (buf);
    if (map->rule >= NUM_RULES || map->rule < 0)
    {
	fprintf (stderr, "Unknown map rule %d, assuming rule 0\n", map->rule);
	map->rule = 0;
    }

    next_line (buf, sizeof buf, fp);
    strip_newline (buf);
    map->name = new_string (buf);

    next_line (buf, sizeof buf, fp);
    strip_newline (buf);
    map->author = new_string (buf);

    map->data = (unsigned char *) alloc_memory (map->x * map->y);

    for (i = x = y = 0; (c = getc (fp)) != EOF; )
    {
	if (c == '\n')
	{
	    if (x < map->x)
	    {
		fprintf (stderr, "Not enough data on map data line %d\n", y + 1);
	    }
	    if (++y >= map->y)
	    {
		break;
	    }
	    x = 0;
	}
	else if (x < map->x)
	{
	    map->data[i++] = c;
	    x++;
	}
    }
    if (i != map->x * map->y)
    {
	if (x < map->x && y < map->y)
	{
	    fprintf (stderr, "Not enough data on map data line %d\n", y);
	}
	if (y + (x >= map->x) < map->y)
	{
	    fprintf (stderr, "Could read only %d whole lines of map data\n",
		y + (x >= map->x));
	}
	fprintf (stderr, "Can't read all map data\n");
	exit (1);
    }
}

static void convert_map_data (struct map *map)
{
    static unsigned char conversions[][2] = {
	{'x', 'x'},
	{' ', ' '},
	{'s', 's'},
	{'a', 'a'},
	{'w', 'w'},
	{'q', 'q'},
	{'r', 'r'},
	{'d', 'd'},
	{'f', 'f'},
	{'c', 'c'},
	{'F', '#'},
	{'*', '_'},
	{'+', '+'},
	{'-', '-'},
	{'>', '>'},
	{'<', '<'},
	{'W', '@'},
	{'I', '('},
	{'O', ')'},
	{'@', 'x'},
	{'0', 'A'},
	{'1', 'B'},
	{'2', 'C'},
	{'3', 'D'},
	{'4', 'E'},
	{'5', 'F'},
	{'6', 'G'},
	{'7', 'H'},
	{'8', 'I'},
	{'9', 'J'}
    };
    int i, n, old, new;
    unsigned char buf[256];

    memset (buf, ' ', sizeof buf);
    for (i = 0; i < sizeof(conversions) / sizeof(conversions[0]); i++)
    {
	old = conversions[i][0];
	new = conversions[i][1];
	buf[old] = new;
    }
    n = map->x * map->y;
    for (i = 0; i < n; i++)
    {
	old = map->data[i];
	new = buf[old];
	map->data[i] = new;
    }
}

static void write_new_map (FILE *fp, struct map *map)
{
    static struct rule {
	int lives;
	int rules;
    } rules[NUM_RULES] = {
	{ 0, (CRASH_WITH_PLAYER|PLAYER_KILLINGS|
	      PLAYER_SHIELDING|LIMITED_VISIBILITY|TEAM_PLAY) },
	{ 1, (LIMITED_LIVES|PLAYER_SHIELDING|ONE_PLAYER_ONLY) },
	{ 0, (TIMING|ONE_PLAYER_ONLY) },
	{ 0, (TIMING) },
	{ 0, (CRASH_WITH_PLAYER|PLAYER_KILLINGS|
	      PLAYER_SHIELDING|TEAM_PLAY) },
	{ 3, (CRASH_WITH_PLAYER|PLAYER_KILLINGS|
	      LIMITED_LIVES|PLAYER_SHIELDING|TEAM_PLAY) }
    };
    int x, y;

    fprintf (fp, "# Map converted by mapmapper %s\n\n", versionid);
    fprintf (fp, "mapWidth:            %d\n", map->x);
    fprintf (fp, "mapHeight:           %d\n", map->y);
    fprintf (fp, "mapName:             %s\n", map->name);
    fprintf (fp, "mapAuthor:           %s\n", map->author);
    fprintf (fp, "crashWithPayer:      %s\n", YN(CRASH_WITH_PLAYER));
    fprintf (fp, "playerKillings:      %s\n", YN(PLAYER_KILLINGS));
    fprintf (fp, "playerShielding:     %s\n", YN(PLAYER_SHIELDING));
    fprintf (fp, "limitedVisibility:   %s\n", YN(LIMITED_VISIBILITY));
    fprintf (fp, "teamPlay:            no\n");
    fprintf (fp, "timing:              %s\n", YN(TIMING));
    fprintf (fp, "onePlayerOnly:       %s\n", YN(ONE_PLAYER_ONLY));
    fprintf (fp, "limitedLives:        %s\n", YN(LIMITED_LIVES));
    fprintf (fp, "worldLives:          %d\n", RULE_LIVES(map->rule));
    fprintf (fp, "mapData: \\multiline: EndOfMapdata\n");
    for (y = 0; y < map->y; y++)
    {
	for (x = 0; x < map->x; x++)
	{
	    fputc (map->data[y * map->x + x], fp);
	}
	fputc ('\n', fp);
    }
    fprintf (fp, "EndOfMapdata\n");
    fflush (fp);
}

static FILE *open_file (char *filename, char *mode, int overwrite)
{
    FILE *fp;
    int c;

    if (mode[0] == 'w' && overwrite == 0 && access (filename, 0) == 0)
    {
	fprintf (stderr, "Output file %s already exists, overwrite? (y/n) ",
		 filename);
	fflush (stderr);
	if ((c = getchar ()) == EOF || c != 'y' && c != 'Y')
	{
	    fprintf (stderr, "Not overwritten\n");
	    exit (1);
	}
    }
    if ((fp = fopen (filename, mode)) == NULL)
    {
	perror (filename);
	exit (1);
    }
    return fp;
}

static void usage (char *progname)
{
    fprintf (stderr, "Usage: %s [ -f ] [ oldmap [ newmap ] ]\n", progname);
    exit (1);
}

int main (int argc, char **argv)
{
    int argi = 1, overwrite = 0;
    FILE *input, *output;
    struct map map;

    if (argc - argi > 0 && strncmp (argv[argi], "-h", 2) == 0)
    {
	usage (*argv);
    }
    if (argc - argi > 0 && strcmp (argv[argi], "-f") == 0)
    {
	argi++;
	overwrite = 1;
    }

    if (argc - argi > 2)
    {
	usage (*argv);
    }
    if (argc - argi > 0)
    {
	input = open_file (argv[argi++], "r", overwrite);
    }
    else
    {
	input = stdin;
    }
    read_old_map (input, &map);
    fclose (input);

    convert_map_data (&map);

    if (argc - argi > 0)
    {
	output = open_file (argv[argi++], "w", overwrite);
    }
    else
    {
	output = stdout;
    }
    write_new_map (output, &map);
    fclose (output);

    return 0;
}
