/* $Header: /a/cvs/386BSD/ports/editor/point/buffers.c,v 1.1 1994/02/15 22:12:35 jkh Exp $ */

#include <stdio.h>
#include "pt.h"

#define hashFn(x,y,z) ((13*(x)+(int)((y)&0x7FFF))%(z))

/* the file block buffers */
struct diskBuffer sBuffers[NBUFFERS];
struct diskBuffer *buffers = sBuffers;
char *bufferSpace;
int nextBuffer;		/* the next buffer to reuse */

/* buffer hash tables */
struct diskBuffer *bufHash[NBUFHASH];

void
unlink1(bp)
	register struct diskBuffer *bp;
{
	extern struct diskBuffer *bufHash[];
	
	int h2;
	
	/* unlink it from its old hash chain */
	if( bp->forwardHash != NULL )
		bp->forwardHash->backwardHash = bp->backwardHash;
	if( bp->backwardHash != NULL )
		bp->backwardHash->forwardHash = bp->forwardHash;
	else {	/* first in the hash chain */
		h2 = hashFn(bp->handle, bp->blockNumber, NBUFHASH);
		bufHash[h2] = bp->forwardHash;
	}
}

struct diskBuffer *
getBuffer(handle, blockNumber)
	int handle;
	int blockNumber;
{
	extern char msgBuffer[];
	extern struct diskBuffer *buffers;
	extern struct diskBuffer *bufHash[];
	extern int nextBuffer;
	extern struct openFile *files;
	extern int nBuffers;
	extern int maxFiles;
	extern int hashChainBuffersScanned;
	extern int buffersRequested;
	extern int buffersNotFound;
	extern int buffersInUse;
	extern int buffersWritten;

	register int i;
	register struct diskBuffer *bp;
	int h;
	unsigned char *fp;
	unsigned int bufferBits, fileBits;


	/* see if the block is already in a buffer */
	h = hashFn(handle, blockNumber, NBUFHASH);
	bp = bufHash[h];

	while( bp != NULL ) {
++hashChainBuffersScanned;
		if( bp->handle==handle && bp->blockNumber==blockNumber ) {
			break;
		}
		bp = bp->forwardHash;
	}

++buffersRequested;
	if( bp == NULL ) {
++buffersNotFound;

		/* if not, assign it a buffer and fill it with data */
/* LATER: use LRU buffer replacment instead of FIFO */
		if( ++nextBuffer >= nBuffers )
			nextBuffer = 0;
		bp = &buffers[nextBuffer];

		if( bp->handle != -1 ) {	/* is the buffer in use? */
++buffersInUse;

			/* unlink it from its old hash chain */
			unlink1(bp);

			/* Invalidate any possible buffer caches. */
			bufferBits = (unsigned int)bp->bufferAddress
							>> BUFFERSHIFT;
			for(i = 0; i < maxFiles; i++) {
				/* get the buffer block number */
				fileBits = (unsigned int)files[i].logBuf
							>> BUFFERSHIFT;
				/* see if the block numbers match */
				if( bufferBits == fileBits ) {
					/* if so invalidate the cache */
					files[i].hiLogBuffer = -1;
				}
			}

			/* write the buffer out to disk */
			if( bp->written ) {
++buffersWritten;
				i = lseek(bp->handle,
					(bp->blockNumber)<<BUFFERSHIFT, 0);
 				i = write(bp->handle, bp->bufferAddress,
					BUFFERSIZE);
				if( i < BUFFERSIZE ) {
					perror("getBuffer");
				}
			}
		}
		bp->handle = handle;
		bp->blockNumber = blockNumber;
		bp->written = 0;

		/* read in the new buffer contents */
		lseek(handle, blockNumber<<BUFFERSHIFT, 0);
		i = read(handle, (char *)(bp->bufferAddress), BUFFERSIZE);
		/* read zeros for non-existing chararacters */
		/* this will occur in the add file only */
		if( i <= 0 ) {
			if( i < 0 ) {
				sprintf(msgBuffer,
"getBuffer: read error, ret=%d, handle=%d", i, handle);
				msg(msgBuffer, 1 );
			}
			/* zero out the buffer */
			fp = bp->bufferAddress;
			for(i = 0; i < BUFFERSIZE; ++i)
				*fp++ = '\0';
		}

		bp->backwardHash = NULL;
		bp->forwardHash = bufHash[h];
		if( bufHash[h] != NULL )
			bufHash[h]->backwardHash = bp;
		bufHash[h] = bp;
	}
#ifdef SELF_ORGANIZING
	else if( bp->backwardHash != NULL ) {
		/* make the hash lists self-organizing by moving this */
		/* buffer header to the front of the list */
		unlink1( bp );
		bp->backwardHash = NULL;
		bp->forwardHash = bufHash[h];
		if( bufHash[h] != NULL )
			bufHash[h]->backwardHash = bp;
		bufHash[h] = bp;
	}
#endif
	return bp;
}

/*ARGSUSED*/
void
fidInvalid(handle, fid)
	int handle;
	int fid;
{
	extern struct diskBuffer *buffers;
	extern int nBuffers;
	
	int i;

	/* invalidate the buffer cache */
	for(i = 0; i < nBuffers; i++)
		if( buffers[i].handle == handle ) {
			unlink1(&buffers[i]);
			buffers[i].handle = -1;
		}
}

int
getFileByte( fileId, logicalByte )
	int fileId;
	Offset logicalByte;
{
	extern int getSpanSize;
	extern int fileBytesRequested;

	int n;
	unsigned char *firstByte, *lastByte;

++fileBytesRequested;
	n = getSpan(fileId, logicalByte, &firstByte, &lastByte, 0);
++getSpanSize;
	if( n != 0 ) {
		return BLOCK_EOF;
	} else {
		return *firstByte;
	}
}

static unsigned char *firstByte = (unsigned char *)1;
static unsigned char *lastByte = 0;
static Offset logFirstByte = 1;
static int logLength = -1;
static int logN;

void
ClearByteCache()
{
	logLength = -1;
}

int
getCachedFileByte( fileId, logicalByte )
	int fileId;
	Offset logicalByte;
{
	unsigned char *addr;
	int offset =  logicalByte - logFirstByte;

	if( offset < logLength ) {
		addr = firstByte + offset;
	} else {
		logN = getSpan(fileId, logicalByte, &firstByte, &lastByte, 0);
		if( logN ) {
			logLength = -1;
			return BLOCK_EOF;
		}
		logFirstByte = logicalByte;
		logLength = lastByte - firstByte + 1;
		addr = firstByte;
	}
	return *addr;
}

int
getSpan( fileId, logicalByte, firstByte, lastByte, reversed )
	int fileId, reversed;
	Offset logicalByte;
	unsigned char * *firstByte;
	unsigned char * *lastByte;
{
	extern char msgBuffer[];
	extern struct openFile *files;
	extern int getSpansRequested;
	extern int spansOutOfRange;
	extern int spansInBufferCache;
	extern int spansInPieceCache;
	extern int cacheBufferSizes;
	extern int trace_file;

register struct openFile *ff;
	struct diskBuffer *buf;
	Offset physicalByte, blockNumber, nn, bp;
	int handle;
	Offset offset;
	Piece pp;

++getSpansRequested;
	/* for efficiency, keep some addresses */
	ff = &files[fileId];
	
	/* special case for an xterm window emulation */
	if( ff->screen_image != NULL ) {
		offset = (ff->screen_rows)*(ff->screen_cols+1);
		if( logicalByte < 0 || logicalByte >= offset ) {
			return 1;
		}
		*firstByte = (unsigned char *)(ff->screen_image + logicalByte);
		*lastByte = (unsigned char *)(ff->screen_image + offset - 1);
		return 0;
	}

	/* if file is not open print an error message */
	if( fileId == -1 || ff->origHandle == -1 ) {
		sprintf(msgBuffer, "getSpan: file %d is not open", fileId);
		msg(msgBuffer, 1 );
		return 1;
	}

	/* see if the logical byte number is invalid */
	if( logicalByte < 0 || logicalByte >= ff->fileSize  ) {
++spansOutOfRange;
		return 1;
	}

	/* check for optimized special cases */
	if( ff->loLogBuffer<=logicalByte && logicalByte<=ff->hiLogBuffer ) {
		unsigned char *temp;

++spansInBufferCache;
		/* if this logical byte is in the buffer cache then set up */
		/* the addresses using the saved segment and offset fields */
		temp = ff->logBuf+(unsigned int)(logicalByte-ff->loLogBuffer);
		if( reversed ) {
			*firstByte = ff->logBuf;
			*lastByte = temp;
		} else {
			*firstByte = temp;
			*lastByte = ff->logBuf
			  + (unsigned int)(ff->hiLogBuffer - ff->loLogBuffer);
		}
		if( trace_file > 0 ) {
			sprintf( msgBuffer, "S %2d %5d %5d\n",
				fileId, logicalByte, *lastByte - *firstByte + 1 );
			write( trace_file, msgBuffer, strlen(msgBuffer) );
		}

		return 0;
	}

	/* see if we already know what piece it is in */
	/* findPiece checks this but for speed we do it here anyway */
	/* since getFileByte is on the critical path of performance */
	if( ff->loLogPiece<=logicalByte && logicalByte<=ff->hiLogPiece ) {
++spansInPieceCache;
		pp = ff->logPiece;
		physicalByte = pp->position + logicalByte - ff->loLogPiece;
	} else {
		pp = findPiece(logicalByte, ff, &nn);
		physicalByte = pp->position + logicalByte - nn;
		/* remember this piece as the cached piece */
		ff->logPiece = pp;
		ff->loLogPiece = nn;
		ff->hiLogPiece = nn + pp->length - 1;
	}

	/* get the physical file block containing this character */
	blockNumber = physicalByte>>BUFFERSHIFT;

	/* use the appropriate handle */
	handle = pp->file;

	/* get the buffer that this character is in */
	buf = getBuffer(handle, blockNumber);

	/* figure out how many bytes into the buffer logicalByte is */
	offset = physicalByte - (blockNumber<<BUFFERSHIFT);
	*firstByte = buf->bufferAddress + offset;

	/* Remember the logical byte limits in this buffer. */

	/* Is the beginning of the buffer still in this piece? */
	bp = logicalByte - offset;

	/* bp = logical byte number of the first physical byte in buffer */
	/* "buf" ASSUMING all of this buffer is in piece "pp".  Now we */
	/* check this assumption and adjust things if it is false */

	if( bp >= ff->loLogPiece ) {
		/* the first byte in this buffer is still in the piece */
		ff->loLogBuffer = bp;
		ff->logBuf = buf->bufferAddress;
	} else {
		/* the piece begins inside the buffer */
		ff->loLogBuffer = ff->loLogPiece;
		ff->logBuf = buf->bufferAddress + (ff->loLogPiece - bp);
	}

	/* Now check if the last physical byte in this buffer is still */
	/* in piece "pp" */

	/* bp: logical byte at the end of the buffer */
	bp += (BUFFERSIZE - 1);
	if( bp <= ff->hiLogPiece ) {
		/* the last byte of the buffer is in the piece */
		ff->hiLogBuffer = bp;
	} else {
		/* piece ends before the end of the buffer */
		ff->hiLogBuffer = ff->hiLogPiece;
	}

	/* handle the special case of reversed spans to support searching */
	/* backwards */
	if( reversed ) {
		/* lastByte points to the logical character argument */
		*lastByte = *firstByte;
		/* firstByte points to the beginning of the buffer */
		*firstByte = ff->logBuf;
	} else {
		*lastByte = *firstByte
			+ (unsigned int)(ff->hiLogBuffer - logicalByte);
	}
cacheBufferSizes += *lastByte - *firstByte + 1;

	if( trace_file > 0 ) {
		sprintf( msgBuffer, "S %2d %5d %5d\n", fileId, logicalByte,
					*lastByte - *firstByte + 1 );
		write( trace_file, msgBuffer, strlen(msgBuffer) );
	}

	return 0;
}

void
writeChar(ch, physicalByte)
	int ch;
	Offset physicalByte;
{
	extern int addHandle;
	extern int charsWritten;

	struct diskBuffer *buf;
	int blockNumber;
	int offset;

++charsWritten;
	/* get the physical file block containing this character */
	blockNumber = physicalByte>>BUFFERSHIFT;
	buf = getBuffer(addHandle, blockNumber);

	/* mark this buffer as written so it will be flushed before */
	/* it is reused */
	buf->written = 1;

	/* figure out how many bytes into the buffer physicalByte is */
	offset = (int)( physicalByte - (blockNumber<<BUFFERSHIFT) );

	/* now store the byte */
	*(buf->bufferAddress+offset) = (unsigned char)ch;
}
