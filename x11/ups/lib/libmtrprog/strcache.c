/* st_strcache.c - random file access via an LRU buffer cache */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char shared_strcache_c_sccsid[] = "@(#)strcache.c	1.18 26/4/92 (UKC)";

/* define STATS to record statistics on caching */

#include "ifdefs.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>

#ifdef __STDC__
#include <unistd.h>
#endif

#include <local/ukcprog.h>

#include "strcache.h"

#ifndef L_SET
#define L_SET	0
#endif

#ifdef STATS
#define DBINC(sc, smem)	++(sc)->sc_stats.smem
#else
#define DBINC(sc, smem)	
#endif

#ifdef STATS
strcache_id_t Newest_strcache_id;	/* BUG: zap this */

typedef unsigned long bitmap_t;

#define BMBITS (sizeof(bitmap_t) * 8)
#endif

/*  Default number of buffers.  Change via the NBUFS env var.
 */
#define DEFAULT_NBUFS	4

/*  Default size of a single buffer.
 *  Can be made changed by the environment for tuning.
 *
 *  This must be a power of two.  See the check below.
 */
#define DEFAULT_BUFSIZE	2048

/*  Compile time check that BUFSIZE is a power of two.
 */
#if (BUFSIZE & (BUFSIZE - 1)) != 0
#include "bufsize is not a power of two"
#endif

/*  A type for offsets.  This type must be unsigned for two reasons:
 *
 *    - to ensure that the various offset comparisons are all unsigned
 *	(thus avoiding nasty signed->unsigned surprises).
 *
 *    - to make the bit twiddling in find_buf portable.
 */
typedef unsigned long offset_t;

typedef long timestamp_t;

typedef struct bufst {
	timestamp_t b_last_used;
	offset_t b_offset;
	offset_t b_lim;
	int b_len;
	char *b_data;
} buf_t;

#ifdef STATS
typedef struct statsst {
	int finds;
	int incache;
	int reads;
	int calls;
	int ifinds;
	int onebuf;
	int multi;
	int gt2;
} stats_t;
#endif

typedef struct strcachest {
	buf_t *sc_last_buf;
	buf_t *sc_bufs;
	int sc_nbufs;
	unsigned long sc_bufsize;

	off_t sc_offset;
	sc_input_func_t sc_input_func;
	sc_close_func_t sc_close_func;
	char *sc_arg;

	char *sc_saved_line;
	long sc_saved_line_len;
	timestamp_t sc_current_stamp;
#ifdef STATS
	stats_t sc_stats;
#endif
} strcache_t;

static buf_t *find_buf PROTO((strcache_t *sc, offset_t offset));
static int get_input_from_fd PROTO((char *arg, off_t offset,
							char *buf, int nbytes));

void
sc_dump_stats_of_newest_sc()
{
#ifdef STATS
	sc_dump_stats(Newest_strcache_id);
#endif
}

void
sc_dump_stats(strcache_id)
strcache_id_t strcache_id;
{
#ifdef STATS
	static const char fmt[] = "%s:%-5d ";
#ifdef __STDC__
#define DBDUMP(smem)	fprintf(stderr, fmt, #smem, sc->sc_stats.smem)
#else
#define DBDUMP(smem)	fprintf(stderr, fmt, "smem", sc->sc_stats.smem)
#endif
	strcache_t *sc;

	sc = (strcache_t *)strcache_id;

	DBDUMP(calls);
	DBDUMP(ifinds);
	DBDUMP(finds);
	DBDUMP(incache);
	DBDUMP(reads);
	DBDUMP(onebuf);
	DBDUMP(multi);
	DBDUMP(gt2);

	if (sc->sc_bufsize % 1024 == 0)
		fprintf(stderr, "bs:%dK, ", sc->sc_bufsize / 1024);
	else
		fprintf(stderr, "bs:%d, ", sc->sc_bufsize);
	fprintf(stderr, "nbufs:%d\n", sc->sc_nbufs);
#endif
}

void
sc_close_strcache(strcache_id)
strcache_id_t strcache_id;
{
	strcache_t *sc;

	sc = (strcache_t *)strcache_id;
	if (sc->sc_close_func != NULL)
		(*sc->sc_close_func)(sc->sc_arg);
	free(sc->sc_bufs->b_data);
	free((char *)sc->sc_bufs);
	if (sc->sc_saved_line != NULL)
		free(sc->sc_saved_line);
	free((char *)sc);
}

strcache_id_t
sc_make_fd_strcache(fd)
int fd;
{
	return sc_make_strcache(get_input_from_fd, (sc_close_func_t)NULL,
									(char *)fd);
}

void
sc_set_offset(strcache_id, offset)
strcache_id_t strcache_id;
off_t offset;
{
	((strcache_t *)strcache_id)->sc_offset = offset;
}

off_t
sc_get_offset(strcache_id)
strcache_id_t strcache_id;
{
	return ((strcache_t *)strcache_id)->sc_offset;
}

int
sc_set_bufs(strcache_id, nbufs, bufsize)
strcache_id_t strcache_id;
int nbufs;
int bufsize;
{
	buf_t *bufs, *b;
	char *data;
	strcache_t *sc;

	sc = (strcache_t *)strcache_id;

	if (nbufs == 0) {
		nbufs = DEFAULT_NBUFS;
#ifdef STATS
		if (getenv("NBUFS") != NULL)
			nbufs = atoi(getenv("NBUFS"));
#endif
	}
	if (bufsize == 0) {
		bufsize = DEFAULT_BUFSIZE;
#ifdef STATS
		if (getenv("BUFSIZE") != NULL)
			bufsize = atoi(getenv("BUFSIZE"));
#endif
	}

	if ((bufsize & (bufsize - 1)) != 0)
		panic("bufsize not a power of two");

	bufs = (buf_t *)e_malloc(nbufs * sizeof(buf_t));
	if ((data = malloc(nbufs * bufsize)) == NULL)
		return -1;
	for (b = bufs; b < bufs + nbufs; ++b) {
		b->b_offset = b->b_lim = 0;
		b->b_len = 0;
		b->b_last_used = 0;
		b->b_data = data;
		data += bufsize;
	}

	sc->sc_bufs = bufs;
	sc->sc_last_buf = bufs;
	sc->sc_nbufs = nbufs;
	sc->sc_bufsize = bufsize;

	return 0;
}

strcache_id_t
sc_make_strcache(input_func, close_func, arg)
sc_input_func_t input_func;
sc_close_func_t close_func;
char *arg;
{
	static buf_t dummy_buf;	/* we rely on this being initialised to zeroes */
	strcache_t *sc;

	sc = (strcache_t *)e_malloc(sizeof(strcache_t));
	sc->sc_last_buf = sc->sc_bufs = &dummy_buf;
	sc->sc_nbufs = 0;
	sc->sc_bufsize = 0;
	sc->sc_offset = 0;
	sc->sc_input_func = input_func;
	sc->sc_close_func = close_func;
	sc->sc_arg = arg;
	sc->sc_saved_line = NULL;
	sc->sc_saved_line_len = 0;
	sc->sc_current_stamp = 0;

#ifdef STATS
	{
		static stats_t zero_stats;

		sc->sc_stats = zero_stats;
	}
	Newest_strcache_id = (strcache_id_t)sc;
#endif

	return (strcache_id_t)sc;
}

/*  Return a strcache_id which is a copy of one passed, apart from
 *  the buffer count and size.  Subsequent changes affect each one
 *  individually.
 *
 *  This is used when you want lots of interleaved accesses to different
 *  parts of a file - it reduces contention for the buffers.  It is also
 *  useful when you want different buffer sizes/counts for the different
 *  bits of the file.
 */
strcache_id_t
sc_dup_strcache(strcache_id)
strcache_id_t strcache_id;
{
	strcache_t *osc, *nsc;

	osc = (strcache_t *)strcache_id;
	nsc = (strcache_t *)sc_make_fd_strcache(0);
	nsc->sc_offset = osc->sc_offset;
	nsc->sc_input_func = osc->sc_input_func;
	nsc->sc_close_func = osc->sc_close_func;
	nsc->sc_arg = osc->sc_arg;
	return (strcache_id_t)nsc;
}

static buf_t *
find_buf(sc, offset)
strcache_t *sc;
offset_t offset;
{
	register buf_t *b;
	timestamp_t lru_time;
	offset_t buf_offset;
	int len;
	buf_t *lru_buf, *maxbuf;

	DBINC(sc, finds);

	if (sc->sc_nbufs == 0) {
		if (sc_set_bufs((strcache_id_t)sc, 0, 0) != 0)
			return NULL;
	}

	b = sc->sc_bufs;
	maxbuf = b + sc->sc_nbufs;
	lru_time = b->b_last_used;
	lru_buf = b;
	buf_offset = offset & ~(sc->sc_bufsize - 1);

	for (; b < maxbuf; ++b) {
		if (b->b_len > 0 && b->b_offset == buf_offset) {
			if (offset < b->b_lim) {
				DBINC(sc, incache);
				return b;
			}
			return NULL;
		}
		if (b->b_last_used < lru_time) {
			lru_time = b->b_last_used;
			lru_buf = b;
		}
	}
	
	/*  Not found - reallocate the least recently used buffer.
	 */
	b = lru_buf;

	/*  Get the data.
	 */
	DBINC(sc, reads);

	len = (*sc->sc_input_func)(sc->sc_arg, (off_t)buf_offset,
					b->b_data, (size_t)sc->sc_bufsize);
	if (len < 0)
		return NULL;

	b->b_offset = buf_offset;
	b->b_len = len;
	b->b_lim = buf_offset + len;
	b->b_last_used = sc->sc_current_stamp++;

	return (offset < b->b_lim) ? b : NULL; 
}

/*  This is called by the application when it thinks that the file
 *  has got bigger (but is unchanged up to the original size).
 *  We forget any short buffers, on the grounds that a new read in
 *  find_buf() might get us some more data.
 */
void
sc_file_has_grown(strcache_id)
strcache_id_t strcache_id;
{
	strcache_t *sc;
	buf_t *b;

	sc = (strcache_t *)strcache_id;

	for (b = sc->sc_bufs; b < sc->sc_bufs + sc->sc_nbufs; ++b) {
		if (b->b_len < sc->sc_bufsize) {
			b->b_lim = b->b_offset;
			b->b_len = 0;
		}
	}
}

void
sc_forget_buffers(strcache_id)
strcache_id_t strcache_id;
{
	strcache_t *sc;
	buf_t *b;

	sc = (strcache_t *)strcache_id;

	for (b = sc->sc_bufs; b < sc->sc_bufs + sc->sc_nbufs; ++b) {
		b->b_lim = b->b_offset;
		b->b_len = 0;
	}
}

static int
get_input_from_fd(arg, offset, buf, nbytes)
char *arg;
off_t offset;
char *buf;
int nbytes;
{
	int fd;

	fd = (int)arg;

	if (lseek(fd, offset, L_SET) == -1)
		return -1;
	return read(fd, buf, nbytes);
}

const char *
sc_get_string(strcache_id, offset, endchar, p_len)
strcache_id_t strcache_id;
off_t offset;
int endchar;
long *p_len;
{
	register buf_t *b;
	strcache_t *sc;
	char *start, *end, *res;
	int res_len, alloc_len;

	sc = (strcache_t *)strcache_id;

	DBINC(sc, calls);
	
	offset += sc->sc_offset;
	if (offset < 0)
		panic("offset < 0 in gss");
	b = sc->sc_last_buf;
	if (b->b_offset > offset || offset >= b->b_lim) {
		if ((b = find_buf(sc, (offset_t)offset)) == NULL) {
			*p_len = 0;
			return NULL;
		}
		sc->sc_last_buf = b;
	}
	else {
		DBINC(sc, ifinds);
	}
	
	/*  Try for the simple case, where the string doesn't cross a
	 *  buffer boundary.
	 */
	start = b->b_data + (offset - b->b_offset);
	end = memchr(start, endchar, b->b_data + b->b_len - start);

	/*  Check for the case where we don't find the terminating
	 *  character because we hit EOF on the block.
	 *
	 *  We stick a NUL character after the data to indicate that
	 *  we hit EOF.
	 */
	if (end == NULL && b->b_len < sc->sc_bufsize) {
		end = b->b_data + b->b_len;
		*end = '\0';
	}

	if (end != NULL) {
		DBINC(sc, onebuf);
		*p_len = end - start;
		return start;
	}

	/*  Oh dear, we've got a string that crosses a buffer boundary.
	 *
	 *  Copy it chunk by chunk into the sc_saved_line buffer.
	 */
	DBINC(sc, multi);

	res = sc->sc_saved_line;
	res_len = sc->sc_saved_line_len;
	if (res == NULL) {
		res_len = sc->sc_bufsize;
		res = e_malloc(res_len);
		sc->sc_saved_line = res;
		sc->sc_saved_line_len = res_len;
	}
	
	alloc_len = b->b_len - (start - b->b_data);
	memcpy(res, start, alloc_len);

	do {
		int chunk_len;

		if ((b = find_buf(sc, (offset_t)offset + alloc_len)) == NULL)
			break;

		if ((end = memchr(b->b_data, endchar, b->b_len)) != NULL)
			chunk_len = end - b->b_data;
		else
			chunk_len = b->b_len;

		if (alloc_len + chunk_len > res_len) {
			res_len *= 2;
			res = e_realloc(res, res_len + 1);
			sc->sc_saved_line = res;
			sc->sc_saved_line_len = res_len;
		}
		memcpy(res + alloc_len, b->b_data, chunk_len);

		alloc_len += chunk_len;
	} while (end == NULL);

#ifdef STATS
	if (alloc_len >= sc->sc_bufsize * 2) {
		DBINC(sc, gt2);
	}
#endif

	/*  We don't put the terminator character on the end of the string
	 *  if we hit EOF without finding it.
	 */
	res[alloc_len] = (b != NULL) ? endchar : '\0';

	*p_len = alloc_len;
	return res;
}

char *
sc_get_bytes(strcache_id, offset, nbytes, p_len)
strcache_id_t strcache_id;
off_t offset;
int nbytes;
long *p_len;
{
	register buf_t *b;
	strcache_t *sc;
	char *start, *res;
	int res_len, ncopied, remaining;

	sc = (strcache_t *)strcache_id;

	DBINC(sc, calls);
	
	offset += sc->sc_offset;
	if (offset < 0)
		panic("offset < 0 in scb");
	b = sc->sc_last_buf;
	if (b->b_offset > offset || offset >= b->b_lim) {
		if ((b = find_buf(sc, (offset_t)offset)) == NULL) {
			*p_len = 0;
			return NULL;
		}
		sc->sc_last_buf = b;
	}
	else {
		DBINC(sc, ifinds);
	}
	
	/*  Try for the simple case, where the string doesn't cross a
	 *  buffer boundary.
	 */
	start = b->b_data + (offset - b->b_offset);
	if (start + nbytes <= b->b_data + b->b_len) {
		DBINC(sc, onebuf);
		*p_len = nbytes;
		return start;
	}

	/*  Oh dear, we've got a string that crosses a buffer boundary.
	 *
	 *  Copy it chunk by chunk into the sc_saved_line buffer.
	 */
	DBINC(sc, multi);

	res = sc->sc_saved_line;
	res_len = sc->sc_saved_line_len;
	if (res == NULL) {
		res_len = (nbytes > sc->sc_bufsize) ? nbytes : sc->sc_bufsize;
		res = e_malloc(res_len);
		sc->sc_saved_line = res;
		sc->sc_saved_line_len = res_len;
	}
	else if (nbytes > res_len) {
		while (nbytes > res_len)
			res_len *= 2;
		free(res);
		res = e_malloc(res_len);
		sc->sc_saved_line = res;
		sc->sc_saved_line_len = res_len;
	}
	
	ncopied = b->b_len - (start - b->b_data);
	remaining = nbytes - ncopied;
	memcpy(res, start, ncopied);

	do {
		int to_copy;

		if ((b = find_buf(sc, (offset_t)offset + ncopied)) == NULL)
			break;
		
		to_copy = (remaining < b->b_len) ? remaining : b->b_len;
		memcpy(res + ncopied, b->b_data, to_copy);
		ncopied += to_copy;
		remaining -= to_copy;
	} while (remaining > 0);

#ifdef STATS
	if (ncopied >= sc->sc_bufsize * 2) {
		DBINC(sc, gt2);
	}
#endif

	*p_len = ncopied;
	return res;
}
