/* $Header: /a/cvs/386BSD/ports/editor/point/anaSources.c,v 1.1 1994/02/15 22:12:35 jkh Exp $ */

#ifdef HYPERTEXT
#include <string.h>
#include <stdio.h>
#include "pt.h"

/*********************************************************************/
/* This routines takes the selection (selBegin and selEnd) in terms  */
/* of the view (ff) and translates it to a new selection (selBegin   */
/* and selEnd) that are relative to the underlying (real) file that  */
/* the pieces in ff were clones from.  Along the way it makes some   */
/* checks about the selection and returns a value based on these.    */
/*                                                                   */
/* Return values: the OR of:                                         */
/*    1 if selBegin or selEnd is in synthetic text                   */
/*    2 if selBegin and selEnd are in different blocks               */
/*********************************************************************/
int
GetRealSelection( ff, selBeginOnly )
	struct openFile *ff;
	int selBeginOnly;
{
	extern Offset selBegin, selEnd;
	extern int debug;

	Piece pp, ppBegin, ppEnd;
	Offset offsetBegin, offsetEnd;
	BlockID blockIDBegin;
	int nBeforePPBegin, nBetweenBeginAndEnd;
	int ret_value = 0;
	int beginPieceOffset;

	/* find the cloned piece the selection starts in */
	ppBegin = findPiece( selBegin, ff, &beginPieceOffset );
	offsetBegin = selBegin - beginPieceOffset;

	/* remember this so we can check it later */
	blockIDBegin = ppBegin->blockID;

	/* do not allow the selection to begin in synthetic text since */
	/* it has no analog in the real file */
	if( ppBegin->flags & IS_DECORATION ) {
		ret_value |= 1;
		return ret_value;
	}

	/* find the real piece in the chain of cloned pieces */
	while( ppBegin->flags & IS_CLONED_PIECE )
		ppBegin = ppBegin->nextClone;

	/* count the number of file bytes in front of this piece */
	pp = ppBegin->prevPiece;
	nBeforePPBegin = 0;
	while( pp != NULL ) {
		nBeforePPBegin += pp->length;
		pp = pp->prevPiece;
	}
printf("selBegin=%d, ", selBegin);
	selBegin = nBeforePPBegin + offsetBegin;
printf("real selBegin=%d\n", selBegin);

	if( selBeginOnly )
		return ret_value;

	/* now do the same stuff with selEnd */

	/* find the cloned piece the selection ends in */
	ppEnd = findPiece( selEnd, ff, &beginPieceOffset );
	offsetEnd = selEnd - beginPieceOffset;

	/* check if the selection begins and ends in the same block */
	if( blockIDBegin != ppEnd->blockID )
		ret_value |= 2;

	/* do not allow the selection to end in synthetic text since */
	/* it has no analog in the real file */
	if( ppEnd->flags & IS_DECORATION ) {
		ret_value |= 1;
		return ret_value;
	}

	/* find the real piece in the chain of cloned pieces */
	while( ppEnd->flags & IS_CLONED_PIECE )
		ppEnd = ppEnd->nextClone;

	/* figure out how many bytes between selBegin and selEnd */
	if( ppBegin == ppEnd ) {
		/* the selection begins and ends in the same real piece */
printf("selEnd=%d, ", selEnd);
		selEnd = nBeforePPBegin + offsetEnd;
printf("real selEnd=%d\n", selEnd);
	} else {
		pp = ppEnd->prevPiece;
		nBetweenBeginAndEnd = 0;
		while( pp != ppBegin ) {
			nBetweenBeginAndEnd += pp->length;
			pp = pp->prevPiece;
		}
 printf("selEnd=%d, ", selEnd);
		selEnd = nBeforePPBegin + ppBegin->length
					+ nBetweenBeginAndEnd + offsetEnd;
printf("real selEnd=%d\n", selEnd);
	}

	return ret_value;
}

void
InitHypertext()
{
	extern char * databaseName;
	extern DBM *currentDB;
	extern Document currentDocument;
	extern AttributeID mainFileBlock;
	extern MapID naturalMap;

	Attribute main_file_block, procedure_header, procedure_body;
	Attribute comment, level0, level1, level2, level3, level4;
	Map block_map;

	if( databaseName == NULL || databaseName[0] == '\0' ) {
		char *m = "NULL or empty hypertext database name\n";
		msg( m, 0 );
		printf( m );
		return;
	}
	currentDB = OpenObjects( databaseName );
	/* the document record always has key == 1 */
	currentDocument = GetDocument( currentDB, 1, ALLOCATE );
	if( currentDocument != NULL ) {
		/* this is an existing document */
		mainFileBlock = LookupAttributeByName( currentDB,
					currentDocument, "Main File Block" );
		if( mainFileBlock == NullObject )
			printf(
"InitHypertext: ERROR! Could not find Main File Block attribute.\n");
		naturalMap = LookupMapByName( currentDB,
					currentDocument, "Natural" );
		if( naturalMap == NullObject )
			printf(
"InitHypertext: ERROR! Could not find Natural map.\n");
		return;
	}

	/* create the document structure and store it */
	currentDocument = CreateDocument( currentDB, databaseName );

	/* create some predefined attributes */
	main_file_block = CreateAttribute( currentDB, currentDocument,
			"Main File Block" );
	mainFileBlock = main_file_block->this_one;
	procedure_header = CreateAttribute( currentDB, currentDocument,
			"Procedure Header" );
	procedure_body = CreateAttribute( currentDB, currentDocument,
			"Procedure Body" );
	comment = CreateAttribute( currentDB, currentDocument,
			"Comment" );
	level0 = CreateAttribute( currentDB, currentDocument,
			"Level 0" );
	level1 = CreateAttribute( currentDB, currentDocument,
			"Level 1" );
	level2 = CreateAttribute( currentDB, currentDocument,
			"Level 2" );
	level3 = CreateAttribute( currentDB, currentDocument,
			"Level 3" );
	level4 = CreateAttribute( currentDB, currentDocument,
			"Level 4" );

	/* create some predefined maps */

	block_map = CreateMap( currentDB, currentDocument, "Natural" );
	/* Set up the initial block map */
	strcpy( block_map->defaultRange, "[$a:$b:$a]" );
	naturalMap = block_map->this_one;
	PutMap( currentDB, block_map, RELEASE );
	
	block_map = CreateMap( currentDB, currentDocument, "Block numbers" );
	/* Set up the initial block map */
	strcpy( block_map->defaultRange, "[$i:$b:$i]" );
	PutMap( currentDB, block_map, RELEASE );

	block_map = CreateMap( currentDB, currentDocument, "Verbose" );
	/* Set up the initial block map */
	strcpy( block_map->defaultRange, "[$a,$n,$i:$b:$a,$n,$i]" );
	PutMap( currentDB, block_map, RELEASE );

	block_map = CreateMap( currentDB, currentDocument,
						"No procedure bodies" );
	/* Set up the initial block map */
	block_map->domain[0] = procedure_body->this_one;
	strcpy( block_map->range[0], "...Body..." );
	strcpy( block_map->defaultRange, "[$a:$b:$a]" );
	PutMap( currentDB, block_map, RELEASE );

	block_map = CreateMap( currentDB, currentDocument, "No comments" );
	/* Set up the initial block map */
	block_map->domain[0] = comment->this_one;
	strcpy( block_map->range[0], "/*...*/" );
	strcpy( block_map->defaultRange, "[$a:$b:$a]" );
	PutMap( currentDB, block_map, RELEASE );

	block_map = CreateMap( currentDB, currentDocument, "Level 0-3 only" );
	/* Set up the initial block map */
	block_map->domain[0] = level4->this_one;
	strcpy( block_map->range[0], "...4..." );
	strcpy( block_map->defaultRange, "[$a:$b:$a]" );
	PutMap( currentDB, block_map, RELEASE );

	block_map = CreateMap( currentDB, currentDocument, "Level 0-2 only" );
	/* Set up the initial block map */
	block_map->domain[0] = level4->this_one;
	strcpy( block_map->range[0], "...4..." );
	block_map->domain[0] = level3->this_one;
	strcpy( block_map->range[0], "...3..." );
	strcpy( block_map->defaultRange, "[$a:$b:$a]" );
	PutMap( currentDB, block_map, RELEASE );

	block_map = CreateMap( currentDB, currentDocument, "Level 0-1 only" );
	/* Set up the initial block map */
	block_map->domain[0] = level4->this_one;
	strcpy( block_map->range[0], "...4..." );
	block_map->domain[0] = level3->this_one;
	strcpy( block_map->range[0], "...3..." );
	block_map->domain[0] = level2->this_one;
	strcpy( block_map->range[0], "...2..." );
	strcpy( block_map->defaultRange, "[$a:$b:$a]" );
	PutMap( currentDB, block_map, RELEASE );

	block_map = CreateMap( currentDB, currentDocument, "Level 0 only" );
	/* Set up the initial block map */
	block_map->domain[0] = level4->this_one;
	strcpy( block_map->range[0], "...4..." );
	block_map->domain[0] = level3->this_one;
	strcpy( block_map->range[0], "...3..." );
	block_map->domain[0] = level2->this_one;
	strcpy( block_map->range[0], "...2..." );
	block_map->domain[0] = level1->this_one;
	strcpy( block_map->range[0], "...1..." );
	strcpy( block_map->defaultRange, "[$a:$b:$a]" );
	PutMap( currentDB, block_map, RELEASE );

	/* free the attribute structures */
	PtFree( (char *)main_file_block );
	PtFree( (char *)procedure_header );
	PtFree( (char *)procedure_body );
	PtFree( (char *)comment );
	PtFree( (char *)level0 );
	PtFree( (char *)level1 );
	PtFree( (char *)level2 );
	PtFree( (char *)level3 );
	PtFree( (char *)level4 );

	/* LATER: create some predefined views */
	PutDocument( currentDB, currentDocument, NO_RELEASE );
printf("InitHypertext: currentDB=0X%X  currentDocument=0X%X\n");
printf("*********** Dump of database opened **************\n");
DumpDB( currentDB );
}

void
CloseHypertext()
{
	extern DBM *currentDB;

	CloseObjects( currentDB );
}

#define EN_MAX 1000
static unsigned long en_addrs[EN_MAX];
static int last_en = 0;

static int
EncodeAddress( addr )
	unsigned long addr;
{
	int i;

	/* a 1 resets the table */
	if( addr == 1 ) {
		last_en = 0;
		return 0;
	}
	
	/* search for this address */
	for( i = 0; i < last_en; ++i ) {
		if( en_addrs[i] == addr )
			return i;
	}
	/* have we overflowed the table? */
	if( i >= EN_MAX ) {
		return addr;
	}
	/* else install it in the table */
	en_addrs[last_en] = addr;
	return last_en++;
}

void
DumpPieces( w )
	struct window *w;
{
	extern struct openFile *files;
	int fid = w->fileId;
	struct openFile *ff = &(files[fid]);
	
	PrintPieceChain( "************ fileId pieces ************",
		ff->pieceList, fid );
}
void
DumpRealPieces( w )
	struct window *w;
{
	extern struct openFile *files;
	int fid = w->realFileId;
	struct openFile *ff = &(files[fid]);
	
	PrintPieceChain( "************ realFileId pieces ************",
		ff->pieceList, fid );
}

void
DumpTables()
{
	extern struct window *selWindow;
	
	DumpPieces( selWindow );
	DumpRealPieces( selWindow );
}

static char *
ReadViewText( start, len, fid )
	int start;
	int len;
	int fid;
{
	int i;
	static char buffer[100];
	char *p = buffer;
	char ch;

	if( len > 49 )
		len = 49;
	for( i = 0; i < len; ++i ) {
		ch = getFileByte( fid, start++ );
		if( ch == '\n' ) {
			*p++ = '\\';
			ch = 'n';
		} else if( ch == '\t' ) {
			*p++ = '\\';
			ch = 't';
		}
		*p++ = ch;
	}
	*p = '\0';
	return buffer;
}

void
PrintPieceChain( title, pp, fid )
	char *title;
	Piece pp;
	int fid;
{
	int offset = 0;

	printf("%s\n", title);
	printf("%7s%7s%3s%3s%4s%5s%4s%5s %s\n",
			"addr  ", "clone>", "fi", "fl",
			"bID", "pos", "len", "offs", "Contents" );
	while( pp != NULL ) {
		printf( "%7x%7x%3d%3x%4d%5d%4d%5d %s\n",
			EncodeAddress((unsigned int)pp),
			EncodeAddress((unsigned int)pp->nextClone),
			pp->file, pp->flags,
			pp->blockID, pp->position, pp->length, offset,
			ReadViewText(offset,pp->length,fid)
		);
		offset += pp->length;
		pp = pp->nextPiece;
	}
}

static Piece first_pp, last_pp;
static int orig_handle;

static void
AddPieceToChain( pos, len )
	Offset pos;
	int len;
{
	Piece pp;

	pp = getFreePiece();
	pp->file = orig_handle;
	pp->position = pos;
	pp->length = len;
	/* put at the end of the list of pieces */
	if( last_pp == NULL ) {
		first_pp = pp;
		last_pp = pp;
	} else {
		pp->prevPiece = last_pp;
		last_pp->nextPiece = pp;
		last_pp = pp;
	}
}

void
SeparateBlockMarkers( w )
	struct window *w;
{
	extern unsigned char beginMarkerChar;
	extern unsigned char endMarkerChar;
	extern struct openFile *files;

	Offset pos, begin_pos;
	int fid = w->realFileId;
	struct openFile *ff = &files[fid];
	int flags;
	BlockID blockID;
	int uch, len;

	/* get the file handle for the file */
	/* we ASSUME that there is only one piece in the piece table */
	orig_handle = ff->pieceList->file;
	begin_pos = pos = 0;
	last_pp = NULL;
	while( 1 ) {
		uch = getFileByte(fid, pos);
		if( uch == BLOCK_EOF || uch == beginMarkerChar ) {
			/* adjust pos if a character was not really read */
			/* (that is, if BLOCK_EOF was read) */
			if( uch == BLOCK_EOF )
				--pos;
			len = pos - begin_pos;
			if( len > 0 ) {
				AddPieceToChain( begin_pos, len );
				begin_pos = pos;
			}
			if( uch == BLOCK_EOF )
				break;
			if( uch == beginMarkerChar ) {
				begin_pos = pos;
				pos = ReadBlockMarker( fid, pos+1, &blockID,
					(unsigned int *)&flags );
				/* LATER: verify and save blockID and flags */
				AddPieceToChain( begin_pos, MARKER_SIZE );
				begin_pos = pos;
			}
		}
		++pos;
	}
	/* install the new piece table */
	freePieces( ff->pieceList );
	ff->pieceList = first_pp;
	/* initialize the optimization fields */
	ff->loLogPiece = 0;
	ff->hiLogPiece = first_pp->length - 1;
	ff->logPiece = first_pp;
	ff->hiLogBuffer = -1;
	ff->loLogBuffer = -1;
	ff->logBuf = NULL;
}

Offset
ReadBlockMarker( fid, pos, blockID, flags )
	int fid;
	Offset pos;
	BlockID *blockID;
	unsigned int *flags;
{
	extern unsigned char endMarkerChar;

	unsigned int n;
	unsigned char uch;
	int i;

	/* read the block number */
	n = 0;
	for( i = 0; i < 6; ++i )
		n = 10*n + getFileByte(fid, pos++) - '0';
	*blockID = (BlockID)n;
	++pos;	/* skip the ',' */

	/* read the flags */
	*flags = (int)getFileByte( fid, pos++ );

	/* move past the end of marker character */
	uch = (unsigned char)getFileByte( fid, pos++ );
	if( uch != endMarkerChar )
		printf("ReadBlockMarker: marker ends with %x not %X\n",
			uch, endMarkerChar);
	return pos;
}

Offset
FindBlock( blockID, fid )
	BlockID blockID;
	int fid;
{
	extern unsigned char beginMarkerChar;

	Offset offset = 0;
	int uch;
	unsigned int blockNumber, flags;

	while( 1 ) {
		uch = getFileByte( fid, offset++ );
		if( uch == BLOCK_EOF ) {
			printf("FindBlock: blockID %d not found in file\n",
				blockID);
			return -1;
		}
		if( (unsigned char)uch == beginMarkerChar ) {
			/* read the block number */
			offset = ReadBlockMarker( fid, offset, &blockNumber,
								&flags );
			if( flags == (int)BeginBlockFlag
						&& blockNumber == blockID )
				return offset - MARKER_SIZE;
		}
	}
	/*NOTREACHED*/
}

Offset
SkipToEndOfBlock( fid, offset, endBlockID )
	int fid;
	Offset offset;
	BlockID endBlockID;
{
	extern unsigned char beginMarkerChar;

	int uch;
	unsigned int flags;
	BlockID blockID;
	
	while( 1 ) {
		uch = getFileByte( fid, offset++ );
		if( uch == BLOCK_EOF ) {
			printf(
"SkipToEndOfBlock: end of block %d not found in file\n",
				endBlockID );
			/* and end of file automatically ends any blocks */
			break;
		}
		if( uch == beginMarkerChar ) {
			offset = ReadBlockMarker( fid, offset, &blockID, &flags );
			if( (endBlockID == 0 || blockID == endBlockID) ) {
				if( flags != EndBlockFlag )
					printf(
"SkipToEndOfBlock: found another beginning for block %d (%d)\n",
						endBlockID, blockID );
				break;
			}
			/* else keep looking */
		}
		/* else keep looking */
	}
	return offset;
}

void
CreateViewPieceTable( w, ff )
	struct window *w;
	struct openFile *ff;
{
	extern int debug;

	Offset offset;
	Piece pp;
	int length;

	offset = FindBlock( w->block->this_one, w->realFileId );
	(void)CreatePieceTableForBlock( w, offset, &(ff->pieceList), NULL );
	
	/* find out the length of the file */
	length = 0;
	pp = ff->pieceList;
	while( pp != NULL ) {
		length += pp->length;
		pp = pp->nextPiece;
	}
	ff->fileSize = length;
	ff->origFileSize = length;
}

Offset
ProcessOneBlock( blockID, w, offset, firstPiece, lastPiece )
	BlockID blockID;
	struct window *w;
	Offset offset;
	Piece *firstPiece;
	Piece *lastPiece;
{
	extern unsigned char beginMarkerChar;
	extern int debug;

	Piece firstpp = NULL;
	Piece lastpp = NULL;
	Piece fp, lp;
	Offset beginOffset;
	int uch;
	unsigned int id, flags;
	int fid = w->realFileId;

	while( 1 ) {	/* loop through each run of characters or subblock */
		beginOffset = offset;	/* remember for later */
		/* scan characters until EOF or a block marker is found */
		while( 1 ) {
			uch = getFileByte( fid, offset++ );
			if( uch == BLOCK_EOF || uch == beginMarkerChar )
				break;
		}
		CreateSpanPieces(blockID, fid, beginOffset, offset-2, &fp, &lp);
			/* CreateSpanPieces handles empty runs */
		if( fp != NULL ) {
			if( lastpp == NULL ) {
				firstpp = fp;
				lastpp = lp;
			} else {
				fp->prevPiece = lastpp;
				lastpp->nextPiece = fp;
				lastpp = lp;
			}
		}
		if( uch == BLOCK_EOF ) {
			printf("ProcessOneBlock: hit EOF looking for block\n");
			break;
		}
		offset = ReadBlockMarker( fid, offset, &id, &flags );
		if( id == blockID ) {
			/* same id -- should be the end of the block */
			if( flags != EndBlockFlag )
				printf("ProcessOneBlock: wrong block marker\n");
			break;
		}
		if( flags != BeginBlockFlag ) {
			printf( "ProcessOneBlock: block end should be begin\n");
			break;
		}
		offset = CreatePieceTableForBlock( w, offset-MARKER_SIZE, &fp,
									&lp );
		if( fp != NULL ) {
			if( lastpp == NULL ) {
				firstpp = fp;
				lastpp = lp;
			} else {
				fp->prevPiece = lastpp;
				lastpp->nextPiece = fp;
				lastpp = lp;
			}
		}
	}
	*firstPiece = firstpp;
	*lastPiece = lastpp;
	return offset;
}

void
CreateSpanPieces( blockID, fid, begin, end, fp, lp )
	BlockID blockID;
	int fid;
	Offset begin, end;
	Piece *fp, *lp;
{
	extern struct openFile *files;
	extern int debug;

	Piece firstpp = NULL;
	Piece lastpp = NULL;
	Piece newpp, oldpp;
	struct openFile *ff;
	Offset beginPiece;
	Offset pieceOffset;
	Offset pieceLength;
	Offset size;

	ff = &files[fid];
	while( begin <= end ) {
		oldpp = findPiece( begin, ff, &beginPiece );
		pieceOffset = begin - beginPiece;
		newpp = getFreePiece();
		newpp->flags = IS_CLONED_PIECE;
		newpp->blockID = blockID;
		newpp->position = oldpp->position + pieceOffset;
		pieceLength = oldpp->length - pieceOffset;
		size = end - begin + 1;
		if( pieceLength > size )
			pieceLength = size;
		begin += pieceLength;
		newpp->length = pieceLength;
		newpp->file = oldpp->file;
		/* link into the chain of cloned pieces */
		newpp->nextClone = oldpp->nextClone;
		oldpp->nextClone = newpp;
		/* link onto the chain of pieces for this span */
		if( lastpp == NULL ) {
			lastpp = newpp;
			firstpp = newpp;
		} else {
			lastpp->nextPiece = newpp;
			newpp->prevPiece = lastpp;
			lastpp = newpp;
		}
	}
	*fp = firstpp;
	*lp = lastpp;
}

Offset
CreatePieceTableForBlock( w, offset, firstPiece, lastPiece )
	struct window *w;
	Offset offset;
	Piece *firstPiece;
	Piece *lastPiece;
{
	extern struct openFile *files;
	extern unsigned char beginMarkerChar;
	extern unsigned char endMarkerChar;
	extern Offset addPosition;
	extern int addHandle;
	extern int debug;
	extern char msgBuffer[];

	int uch;
	char ch2;
	Block block;
	ID attrID;
	unsigned char *s;
	int i;
	int blockSkipped;
	Map blockMap;
	Attribute attribute;
	unsigned int flags;
	char *p;
	Offset stringBegin, size;
	Piece firstpp = NULL;
	Piece lastpp = NULL;
	Piece pp, fp, lp;
	BlockID blockID;
	int fid = w->realFileId;
	DBM *db = w->db;

	uch = getFileByte( fid, offset++ );	/* read beginMarkerChar */
	if( uch != beginMarkerChar ) {
		printf("CreatePieceTableForBlock: no block marker\n");
		return NULL;
	}

	offset = ReadBlockMarker( fid, offset, &blockID, &flags );
		/* get the block to get the attribute IDs */
	block = GetBlock( db, blockID, NO_ALLOCATE);
	if( block != NULL ) {
		attrID = block->attribute[0];
	} else {
		printf("Cannot find block %d\n", blockID);
		attrID = 0;
	}

	/* get the block map and look for our attribute */
	blockMap = w->blockMap;
	s = (unsigned char *)blockMap->defaultRange;
	for( i = 0; i < MAP_SIZE; ++i ) {
		if( blockMap->domain[i] == attrID ) {
			s = (unsigned char *)blockMap->range[i];
			break;
		}
	}
	
	blockSkipped = 0;
	stringBegin = addPosition;

	/* process the format string */
	while( (ch2 = *s++) != '\0' ) {
		if( ch2 != FormatMarker ) {
			writeChar( ch2, addPosition++ );
			continue;
		}
		/* else it is a format character, so get the control char */
		ch2 = *s++;
		/* this is to guard against a malformed format */
		if( ch2 == '\0' ) {
			printf(
"CreatePieceTableForBlock: format char (%c) at end of format string\n",
				FormatMarker );
			break;
		}
		switch( ch2 ) {
		case PutBlockContents:
			/* create a piece for the inital block string */
			size = addPosition - stringBegin;
			if( size > 0 ) {
				pp = getFreePiece();
				pp->file = addHandle;
				pp->position = stringBegin;
				pp->length = size;
				pp->flags |= IS_DECORATION;
				pp->blockID = blockID;
				if( lastpp == NULL ) {
					/* still empty */
					firstpp = pp;
					lastpp = pp;
				} else {
					pp->prevPiece = lastpp;
					lastpp->nextPiece = pp;
					lastpp = pp;
				}
			}
			/* put in the pieces for the block */
			offset = ProcessOneBlock(blockID, w, offset, &fp, &lp);
			if( fp != NULL ) {
				if( lastpp == NULL ) {
					firstpp = fp;
					lastpp = lp;
				} else {
					fp->prevPiece = lastpp;
					lastpp->nextPiece = fp;
					lastpp = lp;
				}
			}
			/* indicate we have passed over the block */
			blockSkipped = 1;
			/* start a new string for the stuff after */
			/* the block contents */
			stringBegin = addPosition;
			break;
		case PutBlockID:
			sprintf( msgBuffer, "%d", blockID );
			goto InsertString;
		case PutBlockName:
			block = GetBlock( db, blockID, NO_ALLOCATE);
			sprintf( msgBuffer, "%s", block->name );
			goto InsertString;
		case PutAttributeName:
			block = GetBlock( db, blockID, NO_ALLOCATE);
			attribute = GetAttribute( db, block->attribute[0],
								NO_ALLOCATE);
			sprintf( msgBuffer, "%s", attribute->name );
		InsertString:
			p = msgBuffer;
			while( (ch2 = *p++) != '\0' )
				writeChar(ch2, addPosition++);
			break;
		default:
			printf("Unknown format (%c) in block format string\n",
				ch2);
			break;
		}
	}
	size = addPosition - stringBegin;
	if( size > 0 ) {
		pp = getFreePiece();
		pp->file = addHandle;
		pp->position = stringBegin;
		pp->flags |= IS_DECORATION;
		pp->blockID = blockID;
		pp->length = size;
		if( lastpp == NULL ) {
			/* still empty */
			firstpp = pp;
			lastpp = pp;
		} else {
			pp->prevPiece = lastpp;
			lastpp->nextPiece = pp;
			lastpp = pp;
		}
	}

	if( !blockSkipped )
		offset = SkipToEndOfBlock( fid, offset, blockID );
	if( firstPiece != NULL )
		*firstPiece = firstpp;
	if( lastPiece != NULL )
		*lastPiece = lastpp;
	return offset;
}

void
FreeOldViewPieces( ff )
	struct openFile *ff;
{
	Piece pp = ff->pieceList;
	Piece pp_prev;
	
	while( pp != NULL ) {
		/* go around the circular list to find the previous piece */
		pp_prev = pp->nextClone;
		while( pp_prev->nextClone != pp )
			pp_prev = pp->nextClone;
		if( pp_prev != pp ) {	/* check this to be safe */
			pp_prev->nextClone = pp->nextClone;
		}
		pp = pp->nextPiece;
	}
	freePieces( ff->pieceList );
	ff->pieceList = NULL;	/* just to be safe */
}

int
CreateViewFile( w )
	struct window *w;
{
	extern struct openFile *files;
	extern int maxFiles;

	int fileNumber;
	struct openFile *ff;

	/* find a free file structure */
	for(fileNumber = 0; fileNumber < maxFiles; fileNumber++) {
		if( files[fileNumber].origHandle == -1 )
			break;
	}
	if( fileNumber == maxFiles ) {
		msg("openFile: out of file structures", 0 );
		return -1;
	}
	ff = &files[fileNumber];
	strncpy( ff->origName, w->file->name, FILENAMESIZE );
	ff->origHandle = -2;	/* invalid handle -- not used */
	ff->isView = 1;
	ff->useCount = 1;
	ff->flags = 0;

	/* now build the pieces of this view */
	CreateViewPieceTable( w, ff );

	/* initialize the optimization fields to first piece */
	ff->loLogPiece = 0;
	ff->hiLogPiece = ff->pieceList->length - 1;
	ff->logPiece = ff->pieceList;
	ff->hiLogBuffer = -1;
	ff->loLogBuffer = -1;
	ff->logBuf = NULL;

	/* return the fileId */
	return fileNumber;
}

void
AddFileToDocument( w )
	struct window *w;
{
	extern DBM *currentDB;
	extern Document currentDocument;
	extern Offset selBegin, selEnd;
	extern struct openFile *files;
	extern AttributeID mainFileBlock;
	extern MapID naturalMap;

	char *name, *long_name;

	if( w->db != NULL ) {
		printf("db is not NULL, is this already a Anasazi document?\n");
		return;
	}
	w->db = currentDB;
	w->document = currentDocument;

	long_name = files[w->fileId].origName;
	name = &(files[w->fileId].origName[w->nameOffset]);

	/* create the file and store it */
	w->file = CreateFile( w->db, w->document, long_name );
	
	/* create a block out of the whole file (and store it) */
	w->block = CreateBlock( w->db, w->document, name, mainFileBlock, 0,
							w->file->this_one );
	/* select the whole file */
	selBegin = 0;
	selEnd = fileSize( w->fileId ) - 1;
	InsertBlock( w->block->this_one );
	
	/* use the default (Natural) map */
	w->blockMap = GetMap( w->db, naturalMap, ALLOCATE );

	/* the link maps are initially empty */
	w->fromLinkMap = NULL;
	w->toLinkMap = NULL;

	/* create the initial view and store it */
	w->view = CreateView( w->db, w->document, name, w->block->this_one,
				NullObject, NullObject, w->blockMap->this_one );
	
	/* set up the cloned piece table */
	w->realFileId = w->fileId;	/* save the real fid */
	w->fileId = CreateViewFile( w );

	/* finish up */
	w->posTopline = 0;
	selBegin = 0;
	selEnd = 0;
	AssertSelectionOwnership();

	/* set up the last row cache */
	w->posCurLast = 0;
	w->lastPosTop = 0;
	w->rowCurLast = 0;
}

int
InsertBlock( n )
	unsigned int n;
{
	extern Offset selBegin, selEnd;
	extern unsigned char beginMarkerChar;
	extern unsigned char endMarkerChar;
	extern struct openFile *files;
	extern char msgBuffer[];
	extern int hypertextOn;

	Offset saveSelBegin = selBegin;
	int i;
	int saveHypertextOn = hypertextOn;

	hypertextOn = 0;
	
	/* insert the end marker */
	/* move one character past the selection for the insertion */
	selBegin = ++selEnd;

	insertChar( beginMarkerChar );
	sprintf( msgBuffer, "%06d", n );
	for( i = 0; i < 6; ++i )
		insertChar( msgBuffer[i] );
	insertChar( ',' );
	insertChar( EndBlockFlag );
	insertChar( endMarkerChar );

	/* insert the begin marker */
	selEnd = selBegin = saveSelBegin;
	insertChar( beginMarkerChar );
	for( i = 0; i < 6; ++i )
		insertChar( msgBuffer[i] );
	insertChar( ',' );
	insertChar( BeginBlockFlag );
	insertChar( endMarkerChar );

	hypertextOn = saveHypertextOn;
	
	return 1;
}
#endif

