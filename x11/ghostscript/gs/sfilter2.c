/* Copyright (C) 1991, 1992, 1993 Aladdin Enterprises.  All rights reserved.

This file is part of Ghostscript.

Ghostscript is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
to anyone for the consequences of using it or for whether it serves any
particular purpose or works at all, unless he says so in writing.  Refer
to the Ghostscript General Public License for full details.

Everyone is granted permission to copy, modify and redistribute
Ghostscript, but only under the conditions described in the Ghostscript
General Public License.  A copy of this license is supposed to have been
given to you along with Ghostscript so you can know your rights and
responsibilities.  It should be in a file named COPYING.  Among other
things, the copyright notice and this notice must be preserved on all
copies.  */

/* sfilter2.c */
/* Additional stream functions for filters */
#include "stdio_.h"	/* includes std.h */
#include "memory_.h"
#include "scanchar.h"
#include "stream.h"

/* Generic functions in sfilter.c */
extern int s_filter_write_flush(P1(stream *));
extern int s_filter_close(P1(stream *));

/* ------ ASCII85Encode ------ */

/* Flush the buffer */
private int
s_A85E_write_buf(register stream *s)
{	register stream *strm = s->strm;
	register byte *p = s->cbuf;
	register int count = s->cptr + 1 - p;
	while ( count >= 4 )
	  { ulong word =
	      ((ulong)(((uint)p[0] << 8) + p[1]) << 16) +
	      (((uint)p[2] << 8) + p[3]);
	    if ( word == 0 )
	      sputc(strm, 'z');
	    else
	      { ulong q = word / (85L*85*85*85);  /* actually only a byte */
		ushort w1;
		sputc(strm, (byte)q + '!');
		word -= q * (85L*85*85*85);
		q = word / (85L*85*85);
		sputc(strm, (byte)q + '!');
		word -= q * (85L*85*85);
		q = word / (85*85);
		sputc(strm, (byte)q + '!');
		w1 = (ushort)(word - q * (85L*85));
		sputc(strm, (byte)(w1 / 85) + '!');
		sputc(strm, (byte)(w1 % 85) + '!');
		if ( !(count & 60) )
		  sputc(strm, '\n');
	      }
	    count -= 4, p += 4;
	  }
	memcpy(s->cbuf, p, count);
	s->cptr = s->cbuf + count - 1;
	return 0;
}

/* Close the stream, flushing a partial word. */
private int
s_A85E_close(register stream *s)
{	stream *strm = s->strm;
	int count;
	(*s->procs.write_buf)(s);
	count = s->cptr - s->cbuf + 1;
	if ( count > 0 )
	   {	/* Handle leftover bytes.  1 <= count <= 3. */
		/* All the bytes are at the beginning of the buffer. */
		byte ebuf[5];
		stream sst;
		s->cptr[1] = s->cptr[2] = s->cptr[3] = 0;
		s->cptr = s->cbuf + 3;
		swrite_string(&sst, ebuf, 5);
		s->strm = &sst;
		(*s->procs.write_buf)(s);	/* force out final codes */
		if ( ebuf[0] == 'z' )		/* don't use as last code */
			memcpy(ebuf, "!!!!", 4);
		sputs(strm, ebuf, count + 1);
	   }
	sputs(strm, (byte *)"~>", 2);
	return s_std_close(s);
}

/* Stream procedures */
const stream_procs s_A85E_procs =
   {	s_std_noavailable, s_std_noseek, s_filter_write_flush, s_A85E_close,
	NULL, s_A85E_write_buf
   };

/* ------ ASCII85Decode ------ */

/* Refill the buffer */
private int
s_A85D_read_buf(register stream *s)
{	register stream *strm = s->strm;
	register byte *p = s->cbuf;
	byte *limit = p + s->bsize - 4;
	int ccount = 0;
	ulong word = 0;
	for ( ; ; )
	  { int ch = sgetc(strm);
	    uint ccode = ch - '!';
	    if ( ccode < 85 )		/* catches ch < '!' as well */
	      { if ( p >= limit )
		 { sputback(strm);
		   break;
		 }
		word = word * 85 + ccode;
		if ( ++ccount == 5 )
		 { p[0] = word >> 24;
		   p[1] = (byte)(word >> 16);
		   p[2] = (byte)((uint)word >> 8);
		   p[3] = (byte)word;
		   p += 4;
		   word = 0;
		   ccount = 0;
		 }
	      }
	    else if ( ch == 'z' && ccount == 0 )
	     { if ( p >= limit )
		{ sputback(strm);
		  break;
		}
	       p[0] = p[1] = p[2] = p[3] = 0,
	       p += 4;
	     }
	    else if ( scan_char_decoder[ch] == ctype_space )
	      ;
	    else if ( ch == '~' && sgetc(strm) == '>' )
	     { /* Handle odd bytes.  We always have room for them, */
	       /* because limit is 4 bytes before the end of the buffer. */
	       s->end_status = EOFC;
	       switch ( ccount )
		   {
		case 0:
			break;
		case 1:			/* syntax error */
			s->end_status = ERRC;
			break;
		case 2:			/* 1 odd byte */
			word = word * (85L*85*85) + 0xffffffL;
			goto o1;
		case 3:			/* 2 odd bytes */
			word = word * (85L*85) + 0xffffL;
			goto o2;
		case 4:			/* 3 odd bytes */
			word = word * 85 + 0xffL;
			p[2] = (byte)(word >> 8);
o2:			p[1] = (byte)(word >> 16);
o1:			p[0] = (byte)(word >> 24);
			p += ccount - 1;
		   }
	       break;
	     }
	    else			/* syntax error or exception */
	     { s->end_status = (ch < 0 ? ch : ERRC);
	       break;
	     }
	  }
	s->cptr = s->cbuf - 1;
	s->endptr = p - 1;
	return 0;
}

/* Stream procedures */
const stream_procs s_A85D_procs =
   {	s_std_noavailable, s_std_noseek, s_std_read_flush, s_filter_close,
	s_A85D_read_buf, NULL
   };

/* ------ RunLengthEncode ------ */

/* Initialize */
void
s_RLE_init(register stream *s, uint rec_size)
{	s->record_size = (rec_size == 0 ? max_uint : rec_size);
	s->record_left = s->record_size;
}

/* Empty the buffer */
private int
s_RLE_write_buf(register stream *s)
{	register stream *strm = s->strm;
	register byte *p = s->cbuf;
	while ( p <= s->cptr )
	   {	byte *beg = p, *q;
		uint count = s->cptr - p + 1;
		if ( count > s->record_left)
			count = s->record_left;
		if ( count > 127 )
			count = 127;
		q = p + count;
		if ( count > 2 && p[1] == p[0] )
		   {	/* Recognize leading repeated byte */
			do { p++; }
			while ( p + 1 < q && p[1] == p[0] );
			p++;
			sputc(strm, (byte)(257 - (p - beg)));
			sputc(strm, *beg);
		   }
		else
		   {	while ( p + 2 < q && (p[1] != p[0] || p[2] != p[0]) )
				p++;
			if ( p + 2 >= q ) p = q;
			sputc(strm, (byte)(p - beg - 1));
			sputs(strm, beg, p - beg);
		   }
		s->record_left -= p - beg;
		if ( s->record_left == 0 )
			s->record_left = s->record_size;
	   }
	s->cptr = s->cbuf - 1;
	return 0;
}

/* Close */
private int
s_RLE_close(register stream *s)
{	(*s->procs.write_buf)(s);
	sputc(s->strm, 128);
	return s_std_close(s);
}

/* Stream procedures */
const stream_procs s_RLE_procs =
   {	s_std_noavailable, s_std_noseek, s_filter_write_flush, s_RLE_close,
	NULL, s_RLE_write_buf
   };

/* ------ RunLengthDecode ------ */

/* Initialize */
void
s_RLD_init(register stream *s)
{	s->odd = -1;
}

/* Refill the buffer */
private int
s_RLD_read_buf(register stream *s)
{	register stream *strm = s->strm;
	register byte *p = s->cbuf;
	byte *limit = p + s->bsize;
	int b = s->odd;
	if ( b < 0 ) b = sgetc(strm);
	for ( ; ; )
	   {	uint count;
		if ( b < 0 ) break;	/* EOF/ERR */
		if ( b < 128 )
		   {	if ( b >= limit - p )	/* data won't fit */
				break;
			count = sgets(strm, p, b + 1);
			p += count;
			b = -1;
			if ( count == 0 && strm->end_status )	/* EOF/ERR */
				break;
		   }
		else if ( b == 128 )	/* end of data */
		   {	s->end_status = EOFC;
			b = -1;
			break;
		   }
		else if ( (count = 257 - b) > limit - p )
			break;		/* won't fit */
		else
		   {	b = sgetc(strm);
			if ( b < 0 ) break;	/* EOF/ERR */
			memset(p, b, count);
			p += count;
		   }
		b = sgetc(strm);
	   }
	s->cptr = s->cbuf - 1;
	s->endptr = p - 1;
	s->odd = b;
	return 0;
}

/* Stream procedures */
const stream_procs s_RLD_procs =
   {	s_std_noavailable, s_std_noseek, s_std_read_flush, s_std_close,
	s_RLD_read_buf, NULL
   };
