/* $Header: /a/cvs/386BSD/ports/editor/point/stats.c,v 1.1 1994/02/15 22:12:40 jkh Exp $ */
#include <stdio.h>
/* stuff for edit tracing */
int trace_file = 0;	/* 0 ==> no tracing */

/* some performance measuring counters */
int piecesRequested;
int piecesAllocated;
int getSpanSize;
int piecesFreed;
int piecesSearchedFor;
int piecesScanned;
int scanned0pieces;
int scanned1pieces;
int scanned2pieces;
int scanned3PlusPieces;
int hashChainBuffersScanned;
int buffersRequested;
int buffersNotFound;
int buffersInUse;
int buffersWritten;
int fileBytesRequested;
int getSpansRequested;
int spansOutOfRange;
int spansInBufferCache;
int spansInPieceCache;
int cacheBufferSizes;
int charsWritten;

void
PrintStats( fileId )
	int fileId;
{
	extern char msgBuffer[];
	
	sprintf(msgBuffer,"***** Point caching statistics *****\n");
	write( fileId, msgBuffer, strlen(msgBuffer) );
	sprintf(msgBuffer,"%8d characters accessed\n", getSpansRequested);
	write( fileId, msgBuffer, strlen(msgBuffer) );
	if( getSpansRequested == 0 ) getSpansRequested = 1;
	sprintf(msgBuffer,"     %8d (%5.1f%%) through getFileByte\n", fileBytesRequested,
				100.0 * (float)fileBytesRequested
					/ getSpansRequested);
	write( fileId, msgBuffer, strlen(msgBuffer) );
	sprintf(msgBuffer,"     %8d (%5.1f%%) through getSpan\n",
				getSpansRequested - fileBytesRequested,
				100.0 * (getSpansRequested-fileBytesRequested)
					/ getSpansRequested);
	write( fileId, msgBuffer, strlen(msgBuffer) );
	sprintf(msgBuffer,"%8d calls to getSpan\n", getSpansRequested);
	write( fileId, msgBuffer, strlen(msgBuffer) );
	sprintf(msgBuffer,"     %8.1f%% of getSpans found in buffer cache\n",
				100.0 * (float)spansInBufferCache
					/ getSpansRequested );
	write( fileId, msgBuffer, strlen(msgBuffer) );
	sprintf(msgBuffer,"     %8.1f%% of getSpans found in piece cache\n",
				100.0 * (float)spansInPieceCache
					/ getSpansRequested );
	write( fileId, msgBuffer, strlen(msgBuffer) );
	sprintf(msgBuffer,"     %8.1f bytes provided per span\n",
				(float)cacheBufferSizes / getSpansRequested );
	write( fileId, msgBuffer, strlen(msgBuffer) );
	sprintf(msgBuffer,"     %8.1f bytes USED per span\n",
				(float)getSpanSize / getSpansRequested );
	write( fileId, msgBuffer, strlen(msgBuffer) );
	if( piecesSearchedFor == 0 ) piecesSearchedFor = 1;
	sprintf(msgBuffer,"%8d piece table searches\n", piecesSearchedFor);
	write( fileId, msgBuffer, strlen(msgBuffer) );
	sprintf(msgBuffer,"     %8.1f scans per piece search\n",
				(float)piecesScanned / piecesSearchedFor );
	write( fileId, msgBuffer, strlen(msgBuffer) );
	sprintf(msgBuffer,"     %8d times 0 pieces scanned\n", scanned0pieces);
	write( fileId, msgBuffer, strlen(msgBuffer) );
	sprintf(msgBuffer,"     %8d times 1 pieces scanned\n", scanned1pieces);
	write( fileId, msgBuffer, strlen(msgBuffer) );
	sprintf(msgBuffer,"     %8d times 2 pieces scanned\n", scanned2pieces);
	write( fileId, msgBuffer, strlen(msgBuffer) );
	sprintf(msgBuffer,"     %8d times 3 or more pieces scanned\n", scanned3PlusPieces);
	write( fileId, msgBuffer, strlen(msgBuffer) );
	if( buffersRequested == 0 ) buffersRequested = 1;
	sprintf(msgBuffer,"%8d buffers requested\n", buffersRequested);
	write( fileId, msgBuffer, strlen(msgBuffer) );
	sprintf(msgBuffer,"     %8.1f%% of buffers found in the cache\n", 
				100.0 * (float)(buffersRequested
					- buffersNotFound) / buffersRequested );
	write( fileId, msgBuffer, strlen(msgBuffer) );
	sprintf(msgBuffer,"     %8.1f buffers searched in an average hash chain lookup\n",
				(float)hashChainBuffersScanned
							/ buffersRequested );
	write( fileId, msgBuffer, strlen(msgBuffer) );
	sprintf(msgBuffer,"     %8d buffers had to be written out to disk\n",
				buffersWritten);
	write( fileId, msgBuffer, strlen(msgBuffer) );
}
