/* $Header: /a/cvs/386BSD/ports/editor/point/fileio.c,v 1.1 1994/02/15 22:12:37 jkh Exp $ */

#include <ctype.h>
#include <string.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#ifndef SYSV
# include <sys/timeb.h>
#else
#include <sys/time.h>
#endif /* SYSV */
#include <time.h>

#if defined(uts)
#include <fcntl.h>
#endif /* uts */

#if defined(SYSV)
#include <fcntl.h>
#include <unistd.h>
#endif /* SYSV */
#include "pt.h"

/* the open files */
struct openFile *files;

extern struct diskBuffer *buffers;
extern struct diskBuffer *bufHash[];
extern nextBuffer;

void
initFileio()
{
	extern Piece freePList;
	extern int nBuffers;
	extern unsigned int piecesLeft;
	extern int maxFiles;
	
	register int i;
	unsigned int size;
	unsigned char *bs;

	for(i = 0; i < maxFiles; i++) {
		files[i].origHandle = -1;
	}

	/* set up the buffer hash scheme */
	for(i = 0; i < NBUFHASH; i++)
		bufHash[i] = NULL;
	nextBuffer = -1;

	/* allocate the space (out of this address space) for the buffers */
retryAllocation:
	size = BUFFERSIZE * nBuffers;
	bs = (unsigned char *)PtMalloc(size, "IO buffer");
	if( bs == NULL ) {
		/* if there is not enough space, try fewer buffers */
		if( nBuffers >= 25 ) {
			nBuffers -= 20;
			goto retryAllocation;
		}
		size -= 16*i;
		printf(
		    "NOT ENOUGH MEMORY! (%u bytes short) Use fewer buffers\n",
		    size);
		exit(1);
	}

	/* set up each buffer header to the address of the */
	/* allocated buffer. Buffer i is at buffers[i].bufferAddress  */
	for(i = 0; i < nBuffers; i++) {
		buffers[i].handle = -1;
		buffers[i].bufferAddress = bs;
		bs += BUFFERSIZE;
	}
	
	/* set up the free piece list */
	freePList = NULL;
	piecesLeft = 0;
}

static void
InitCommandHistory( ff )
	struct openFile *ff;
{
	/* initialize the command history */
	ff->cmdHistory = (struct changeItem *)
			PtMalloc( sizeof(struct changeItem), "change item" );
	ff->cmdHistory->type = CNULL;

	/* be SURE that this change is not used */
	ff->cmdHistory->flags = CHANGE_WAS_UNDONE
				| FILE_WAS_CLOSED
				| BLOCK_UNDO_BEGIN;

	/* make this the only item in the command history list */
	ff->cmdHistory->prev = NULL;
	ff->cmdHistory->next = NULL;
}

int
getFileId(origName)
	char *origName;
{
	extern char msgBuffer[];
	extern char scratchFileName[];
	extern int addHandle;
	extern int readOnly;
	extern int maxFiles;
	extern int trace_file;

	int fileNumber, origHandle;
	int freeFileId;
	Offset size;
	Piece pp;
	struct openFile *ff;
	char *fullFilename;

	if( origName == NULL ) {
		scratchFileName[0] = '\0';
		fullFilename = &scratchFileName[0];
	} else
		fullFilename = makeFullPathname(origName);

	/* find a free file structure */
	freeFileId = -1;
	for(fileNumber = 0; fileNumber < maxFiles; fileNumber++) {
		if( files[fileNumber].origHandle == -1 ) {
			if( freeFileId == -1 )
				freeFileId = fileNumber;
		} else if(
			strcmp(files[fileNumber].origName, fullFilename)
								== 0 ) {
			++(files[fileNumber].useCount);
			return fileNumber;
		}
	}
	if( freeFileId == -1 ) {
		msg("openFile: out of file structures", 1 );
		return -1;
	} else
		fileNumber = freeFileId;

	/* open the original file */
	ff = &files[fileNumber];
	strncpy(ff->origName, fullFilename, FILENAMESIZE);
	
	if( fullFilename[0] == '\0' ) {
		origHandle = addHandle;
	} else {
		struct stat statbuf;

		origHandle = open(fullFilename, O_RDONLY, 0);

		if( origHandle < 0 ) {
			sprintf(msgBuffer, "Cannot open %s", fullFilename);
			msg(msgBuffer, 1 );
			return -1;
		}
		
		/* get the time of last modification of the file */
		if( fstat(origHandle,&statbuf)==0 ) {
			ff->lastModified = statbuf.st_mtime;
			ff->lastChanged  = statbuf.st_ctime;
		} else {
			ff->lastModified = 0;
			ff->lastChanged  = 0;
			sprintf(msgBuffer, "stat of %s failed", ff->origName);
			msg(msgBuffer, 0);
		}
	}
	ff->origHandle = origHandle;
	if( trace_file > 0 ) {
		sprintf( msgBuffer, "# file id of %2d is file `%s'\n",
			fileNumber, fullFilename );
		write( trace_file, msgBuffer, strlen(msgBuffer) );
	}

	/* initialize the other fields */
	if( origHandle != addHandle ) {
		size = lseek(origHandle, 0L, 2);
		lseek(origHandle, 0L, 0);
	} else
		size = 0;
	ff->fileSize = size;
	ff->origFileSize = size;
	ff->isView = 0;
	ff->screen_image = NULL;
	ff->useCount = 1;
	ff->flags = 0;
	if( readOnly )
		ff->flags |= READ_ONLY;
	else if( origHandle == addHandle ) {
		ff->flags &= ~READ_ONLY;
	} else {
		/* check for read and write permissions (6 => RW) */
		/* edit in readOnly mode if the UNIX file permissions */
		/* do not allow writing the file */
		if( access(ff->origName, R_OK | W_OK) == 0)
			ff->flags &= ~READ_ONLY;
		else
			ff->flags |= READ_ONLY;
	}

	/* initialize the piece table */
	pp = getFreePiece();
	pp->file = origHandle;
	pp->position = 0;
	pp->length = size;
	ff->pieceList = pp;

	InitCommandHistory( ff );

	/* initialize the optimization fields */
	ff->loLogPiece = 0;
	ff->hiLogPiece = size - 1;
	ff->logPiece = pp;
	ff->hiLogBuffer = -1;
	ff->loLogBuffer = -1;
	ff->logBuf = NULL;

	/*return the fileId */
	return fileNumber;
}

static void
MakeBackups(ff, tempFile )
	struct openFile *ff;
	char *tempFile;
{
	extern int backupDepth;
	extern char *backupNameFormat;
	extern char msgBuffer[];
	extern char textBuffer[];
	extern Tcl_Interp * pointsMainInterp;

	int i, version = -1;
	char *end, *f, *p, *q;
	char *backupName;
	char ch, ch2;
	Tcl_DString ds;

if( !(ff->flags&BAK_MADE) && backupDepth > 0 ) {
	p = textBuffer;		/* create backup file name here */
	f = backupNameFormat;	/* printf-like name format */
	while( 1 ) {
		ch = *f++;
		if( ch == '\0' )
			break;
		if( ch != '%' ) {
			*p++ = ch;
			continue;
		}
		ch = *f++;
		switch( ch ) {

		case 'N':	/* last component of the file name */
			q = &((ff->origName)[getBaseName(ff->origName)]);
			while( 1 ) {
				ch = *q++;
				if( ch == '\0' )
					break;
				*p++ = ch;
			}
			break;

		case 'D':	/* directory of the file name */
			/* terminate the directory part of the name */
			i = getBaseName(ff->origName) - 1;
			if( i >= 0 ) {
				/* check i, just to be safe */
				ch2 = (ff->origName)[i];
				(ff->origName)[i] = '\0';
			}
			q = ff->origName;
			while( 1 ) {
				ch = *q++;
				if( ch == '\0' )
					break;
				*p++ = ch;
			}
			if( i >= 0 ) {
				(ff->origName)[i] = ch2;
			}
			break;

		case 'n':	/* original file name (full path name) */
			q = ff->origName;
			while( 1 ) {
				ch = *q++;
				if( ch == '\0' )
					break;
				*p++ = ch;
			}
			break;

		case 'b':	/* base name of original file */
			q = ff->origName;
			end = q + strlen(q) - 1;
			while( end > q && *end != '.' )
				--end;
			if( end == q )
				end = q + strlen(q);
			while( q < end )
				*p++ = *q++;
			break;

		case 'v':	/* version number -- must be 1 to 9 */
			version = p - textBuffer;
			*p++ = backupDepth + '0';
			break;
		default:
			*p++ = ch;
			break;
		}
	}
	*p = '\0';
	/* now handle ~ processing */
	/* this just copies the pointer if there is no initial ~ */
	backupName = Tcl_TildeSubst( pointsMainInterp, textBuffer, &ds );
	if( textBuffer[0] == '~' &&  version >= 0 ) {
		/* adjust for the new location of the version digit */
		version += strlen(backupName) - strlen(textBuffer);
	}
	/* Delete the last backup. */
	/* If file backupName does not exist, this will return an error code */
	/* but is is easier just to do it than to ask if it exists first. */
	(void)unlink( backupName );
	strcpy( msgBuffer, backupName ); /* a copy to modify systematically */
	if( version >= 0 ) {
		/* move all the other existing backups down one slot */
		for(i = backupDepth - 1; i > 0; --i ) {
			backupName[version] = i + '0';
			msgBuffer[version] = i + 1 + '0';
			(void)rename( backupName, msgBuffer );
		}
	}
#ifdef LATERLATER
	if( backupByCopy ) {
		sprintf( msgBuffer, "cp %s %s", ff->origName, backupName );
		(void)system(msgBuffer);
		/* OR
		Tcl_VarExec( mainBrowser->pointsMainInterp, "copy ",
					ff->origName, backupName, NULL );
		*/
	} else
#endif
	i = rename(ff->origName, backupName);
	if( i < 0 ) {
		sprintf( msgBuffer, "Point could not create the backup file \
named %s, probably because of a nonexistent directory in the path.",
			backupName );
		msg( msgBuffer, 1 );
	}
	ff->flags |= BAK_MADE;
	Tcl_DStringFree( &ds );
} else {
	if( unlink(ff->origName) ) {
		strcpy( backupName, ff->origName );
		p = tildefyFilename( backupName );
		sprintf(msgBuffer,
			"Delete of old version of %s failed; new version in %s",
			p, tempFile );
		msg(msgBuffer, 1 );
	}
}
if( rename(tempFile, ff->origName) ) {
	strcpy( backupName, ff->origName );
	p = tildefyFilename( backupName );
	sprintf(msgBuffer,
		"Rename of %s [new version] to %s [old version] failed",
		tempFile,p, backupName, tempFile );
	msg(msgBuffer, 1 );
}
}

static void
FreeCommandHistory( ff )
	struct openFile *ff;
{
	struct changeItem * change, * change_to_free;

	/* first find the end of the list */
	change = ff->cmdHistory;
	if( change == NULL )
		/* this should not happen */
		return;
	while( change->next != NULL )
		change = change->next;

	/* now walk down the chain and free each one */
	while( change != NULL ) {
		change_to_free = change;
		change = change->prev;
		PtFree( (char *)change_to_free );
	}
	/* just to be neat */
	ff->cmdHistory = NULL;
}

void
saveFile(w)
	struct window *w;
{
	extern char msgBuffer[];
	extern char textBuffer[];
	extern struct openFile *files;

	struct openFile *ff;
	char *p, *tempFile;
	int fid;
	struct stat statbuf;
	int statReturn;

#ifdef HYPERTEXT
	if( hypertextOn && w->document != NULL )
		fid = w->realFileId;
	else
#endif
		fid = w->fileId;
	ff = &files[fid];
	
	/* Do not save if the file is read only */
	if( ff->flags & READ_ONLY ) {
		strcpy( textBuffer, ff->origName );
		p = tildefyFilename( textBuffer );
		sprintf( msgBuffer, "File %s is read only", p );
		msg(msgBuffer, 1 );
		return;
	}

	/* get the file status */
	statReturn = fstat( ff->origHandle, &statbuf );

	/* check if the file has changed since we last read it */
	/* get the time of last modification of the file */
	if( statReturn == 0 ) {
		int msg = 0;
		char *msgText = NULL;
		if( ff->lastModified != statbuf.st_mtime )
			msg |= 1;
		if( ff->lastChanged  != statbuf.st_ctime )
			msg |= 2;
		switch( msg ) {
		case 0:	/* no changes since last read */
			break;
		case 1:	/* written since last break */
		case 3: /* file status changed since last read */
			msgText =
				"has been modified since Point last read it.";
			break;
		case 2: /* file status changed since last read */
			msgText =
"has had its status information changed since Point last read it.";
			break;
		}
		if( msgText != NULL ) {
			sprintf( msgBuffer,
				"MakeModalYesNo {%s} {%s %s %s} {%s} {%s}",
				"Save over changed file?",
				"File", ff->origName, msgText,
				"Write over modified file",
				"Cancel write" );
			/* use msgText as a convenient string point to hold */
			/* the pointer to the returned string */
			msgText = ExecTclCommand( msgBuffer, NULL );
			if( msgText[0] != 'y' )
				return;
		}
	} else {
		sprintf( msgBuffer, "Stat of %s failed", ff->origName );
		msg(msgBuffer, 0);
	}

	/* get a temporary name and write the new version of the file to it */
	tempFile = makeTempFor(ff->origName);
	strcpy( textBuffer, ff->origName );
	p = tildefyFilename( textBuffer );
	sprintf(msgBuffer, "Writing file %s [%ld bytes]...", p, ff->fileSize);
	msg(msgBuffer, 0);

	if( !doWrite( fid, tempFile ) )
		return;

	/* get the file status again to get the new modified dates */
	statReturn = fstat( ff->origHandle, &statbuf );

	/* change the permissions on the new file to match the old one */
	if( statReturn == 0 ) {
		chown( tempFile, statbuf.st_uid, statbuf.st_gid );
		chmod( tempFile, statbuf.st_mode );
	}

	/* write out the message telling the user the file was written */
	strcpy( textBuffer, ff->origName );
	p = tildefyFilename( textBuffer );
	sprintf(msgBuffer, "%s written...%ld bytes", p, ff->fileSize);
	msg(msgBuffer, 0 );

#ifdef OLDSAVE
	/* invalidate any buffers allocated to this open file */
	fidInvalid(ff->origHandle, fid);
	ff->hiLogBuffer = -1;
	ff->loLogBuffer = -1;
#endif

	/* reset the change flag */
	ff->flags &= ~IS_CHANGED;
	NewOpenList();

#ifdef OLDSAVE
	/* close the original file for this open file */
	if( ff->origHandle != addHandle ) {
		i = close(ff->origHandle);
		if( i != 0 ) {
			strcpy( textBuffer, ff->origName );
			tildefyFilename( textBuffer );
			p = (char *)sprintf(msgBuffer,
					"saveFile: close of %s failed, ret=%d", p, i);
			msg(msgBuffer, 1 );
		}
	}

	/* for now free the command history */
	/* LATERLATER: fix things up so we can keep the command history */
	/* even over a save.  It is just a matter of changing the */
	/* file you consider the original file */
	FreeCommandHistory( ff );
	InitCommandHistory( ff );
#endif

	MakeBackups( ff, tempFile );

#ifdef OLDSAVE
	/* open it up again */
	if( ff->origHandle != addHandle ) {
		ff->origHandle = open(ff->origName, O_RDONLY, 0644);

		/* get the file status again to get the new modified dates */
		statReturn = fstat( ff->origHandle, &statbuf );
		if( statReturn == 0 ) {
			/* remember the last modified and changed times */
			ff->lastModified = statbuf.st_mtime;
			ff->lastChanged  = statbuf.st_ctime;
		}
	}

	/* re-initialize the other fields */
	size = lseek(ff->origHandle, 0L, 2);
	ff->fileSize = size;
	ff->origFileSize = size;
	lseek(ff->origHandle, 0L, 0);
	
	/* free all the pieces */
	freePieces(ff->pieceList);

	/* re-initialize the piece table */
	pp = getFreePiece();
	ff->pieceList = pp;
	pp->file = ff->origHandle;
	pp->position = 0;
	pp->length = size;

	/* re-initialize the optimization fields */
	ff->loLogPiece = 0;
	ff->hiLogPiece = size - 1;
	ff->logPiece = pp;
	ff->hiLogBuffer = -1L;
	ff->loLogBuffer = -1L;
	ff->logBuf = NULL;
#else
	/* get the file status again to get the new modified dates */
	statReturn = fstat( ff->origHandle, &statbuf );
	if( statReturn == 0 ) {
		/* remember the last modified and changed times */
		ff->lastModified = statbuf.st_mtime;
		ff->lastChanged  = statbuf.st_ctime;
	}
#endif

	/* redraw the banner to remove the file modified `*' */
	banner( w, 0 );	/* 0 ==> don't redo the slider */
}

void
writeFile(w)
	struct window *w;
{
	extern struct openFile *files;
	extern char msgBuffer[];
	
	char * ret;
	char buffer[MSGBUFFERSIZE];

	sprintf( msgBuffer,
"MakeModalEntry {Save As} {Filename to write to} {Okay} {Cancel write}");
	ret = ExecTclCommand( msgBuffer, NULL );
	if( strcmp(ret,"XXXcancelXXX")==0 ) {
	cancel:
		msg("Write file cancelled", 0);
		return;
	}

	strcpy( buffer, ret );	/* save the file name */
	/* check if the file already exists */
	if( access(buffer, F_OK) == 0 ) {
		sprintf( msgBuffer,
				"MakeModalYesNo {%s} {%s %s %s} {%s} {%s}",
				"Write over file?",
				"File", buffer, "already exists.",
				"Write over it",
				"Cancel write" );
		ret = ExecTclCommand( msgBuffer, NULL );
		if( ret[0] != 'y' )
			goto cancel;
	}

	sprintf(msgBuffer, "Writing file %s [%ld bytes]...",
				buffer, files[w->fileId].fileSize);
	msg(msgBuffer, 0);

	if( !doWrite(w->fileId, buffer) )
		return;

	sprintf(msgBuffer, "%s written...%ld bytes",
				buffer, files[w->fileId].fileSize);
	msg(msgBuffer, 0 );
}

int
doWrite(fileId, tempFile)
	int fileId;
	char *tempFile;
{
	extern char msgBuffer[];
	extern int getSpanSize;

	Offset cp, fSize;
	int fid, iBuffer, lineSize;
	unsigned char *firstByte;
	unsigned char *lastByte;
	unsigned char *sBuffer;
	unsigned int sizeOfBuffer;

fSize = fileSize(fileId);

/* open the output file */
fid = open(tempFile, O_WRONLY | O_CREAT | O_TRUNC, 0644);
if( fid < 0 ) {
	perror("Create temp file");
	sprintf( msgBuffer, "Could not create %s, ret=%d", tempFile, fid );
	msg(msgBuffer, 1 );
	return 0;
}

/* allocate the IO buffer */
sizeOfBuffer = 8192;
while( 1 ) {
	sBuffer = (unsigned char *)PtMalloc(sizeOfBuffer, "output buffer");
	if( sBuffer == NULL ) {
		extern char textBuffer[];
		/* try again with a buffer half as large */
		sizeOfBuffer >>= 1;
		if( sizeOfBuffer <= MSGBUFFERSIZE ) {
			sBuffer = (unsigned char *)textBuffer;
			break;
		}
		continue;
	} else
		break;
}

cp = 0;
iBuffer = 0;
while( 1 ) {
	/* special case for files of length 0 */
	if( fSize == 0 )
		fSize = 1;
	if( getSpan(fileId, cp, &firstByte, &lastByte, 0) )
		break;
	lineSize = (int)(lastByte - firstByte + 1);
getSpanSize += lineSize;
	cp = cp + lineSize;

	if( (iBuffer+lineSize) > sizeOfBuffer ) {/* will it fit? */
		/* no, so write out the buffer to empty it */
		int writeRet;
		writeRet = write(fid, sBuffer, iBuffer);
		if( writeRet < iBuffer ) {
error:
			perror("write to temp file");
			PtFree((char *)sBuffer);
			/* avoid recursive calls if msg tries to write disk */
printf("doWrite: trying to write %d bytes but only wrote %d bytes\n",
				iBuffer, writeRet);
			close(fid);
			unlink(tempFile);
			return 0;
		} else
			iBuffer = 0;
	}
	/* move lineSize bytes from firstByte to sBuffer+iBuffer */
	bcopy( firstByte, sBuffer + iBuffer, lineSize );
	iBuffer += lineSize;
}
if( write(fid, sBuffer, iBuffer) < iBuffer )
	goto error;
close(fid);

/* all is ok */
PtFree((char *)sBuffer);
return 1;
}

char *
makeTempFor(name)
	char *name;
{
	static char tname[256];
	register int len;
	int pathEnd;
	char ch;

	len = 0;
	pathEnd = -1;
	while( 1 ) {
		if( (ch = name[len]) == '\0' )
			break;
		tname[len] = ch;
		switch( ch ) {
		case '/':
			pathEnd = len;
		}
		++len;
	}
	tname[pathEnd+1] = '\0';
	strcat(tname, "tempfile.pt");
	return &tname[0];
}

char *
noWhiteSpace(fileName)
	register char *fileName;
{
	register int n;

	if( fileName != NULL || fileName[0] == '\0' ) {
		/* eliminate white space around fileName */

		/* first white space in the beginning */
		while( isspace(*fileName) )
			++fileName;

		/* now white space at the end */
		n = strlen(fileName) - 1;
		while( isspace(fileName[n]) )
			--n;
		fileName[n+1] = '\0';
	}
	return fileName;
}

int
makeName(s)
	register char *s;
{
	register int l;
	int nTries;
	char ch;

	/* try to create a new name by changing the last letter */
	l = strlen(s) - 1;
	nTries = 0;
	ch = s[l];	/* save first character */
	if( isupper(ch) )
		ch = tolower(ch);
	while( access(s, 0) == 0 ) {
		/* the file name exists so try another */
		++nTries;
		if( s[l] == 'z' )
			s[l] = 'a';
		else
			++s[l];
		if( s[l] == ch )
			return -1;	/* could not find an unused name */
	}
	return nTries;
}

int
getBaseName(s)
	char *s;
{
	int n;

	n = strlen(s) - 1;
	while( 1 ) {
		if( s[n] == '/' )
			break;
		if( --n < 0 )
			break;
	}
	return n+1;
}

Offset
fileSize(fileNumber)
	int fileNumber;
{
	return files[fileNumber].fileSize;
}

int
closeFile(fileId, ask)
	int fileId, ask;
{
	extern char msgBuffer[];
	extern char textBuffer[];
	extern struct changeItem scrapBuffer;
	extern int addHandle;
	extern int trace_file;

	int i, writing;
	char *p, *tempFile;
	register struct openFile *ff;
	struct stat statbuf;

	if( fileId == -1 )
		return 0;

	if( trace_file > 0 ) {
		sprintf( msgBuffer, "# close file id %2d\n", fileId );
		write( trace_file, msgBuffer, strlen(msgBuffer) );
	}

	ff = &files[fileId];

	/* check for 'files' that are really views */
	if( ff->isView )
		goto ViewOnly;

	/* is this the last close? */
	if( --(ff->useCount) > 0 )
		return 0;

	/* see if we need to write the file */
	writing = 1;
	/* has the file changed? */
	if( ff->pieceList->nextPiece == NULL
	 && ff->pieceList->file == ff->origHandle ) {
		if( ff->fileSize == ff->origFileSize ) 
			writing = 0;
	}
	if( ask == 2 )	/* quit and discard mode */
		writing = 0;
	/* do not write the messages file into itself */
	if( ff->origHandle == addHandle )
		writing = 0;
	/* see if we already saved these changes */
	if( !(ff->flags & IS_CHANGED) )
		writing = 0;
	if( writing && (ff->flags & READ_ONLY) ) {
		char * ret;
		strcpy( textBuffer, ff->origName );
		p = tildefyFilename( textBuffer );
		sprintf( msgBuffer,
			"MakeModalYesNo {%s} {%s %s %s} {%s} {%s}",
			"Read only file",
			"File", p, "is read only but changed.",
			"Close file without writing",
			"Cancel close" );
		ret = ExecTclCommand( msgBuffer, NULL );
		if( ret[0] != 'y' )
			return -1;
		writing = 0;
	}
	if( writing ) {
		/* verify the write if ask is true */
		if( ask ) {
			char * ret;
			strcpy( textBuffer, ff->origName );
			p = tildefyFilename( textBuffer );
			sprintf( msgBuffer,
			"MakeModalYesMaybeNo {%s} {%s %s %s} {%s} {%s} {%s}",
				"Save file?",
				"Save file", p, "?",
				"Yes, save the changes",
				"Do not close window",
				"No, discard changes" );
			ret = ExecTclCommand( msgBuffer, NULL );
			if( ret[0] == 'm' )
				return -1;
			writing = (ret[0] == 'y');
		}
		if( writing ) {
			tempFile = makeTempFor(ff->origName);
			strcpy( textBuffer, ff->origName );
			p = tildefyFilename( textBuffer );
			sprintf(msgBuffer, "Writing file %s [%ld bytes]...",
				p, ff->fileSize);
			msg(msgBuffer, 0);
			if( doWrite(fileId, tempFile) == 0 )
				return -1;
			/* change the permissions on the new file to match */
			/* the old one */
			i = stat( ff->origName, &statbuf );
			if( i != 0 ) {
				printf("Stat of %s failed\n", ff->origName);
			} else {
				chown(tempFile,statbuf.st_uid,statbuf.st_gid);
				chmod(tempFile,statbuf.st_mode);
			}
			strcpy( textBuffer, ff->origName );
			p = tildefyFilename( textBuffer );
			sprintf(msgBuffer, "%s written...%ld bytes",
				p, ff->fileSize);
			msg(msgBuffer, 0);
		}
	}

	/* do the close */
	if( ff->origHandle == -1 ) {
		strcpy( textBuffer, ff->origName );
		p = tildefyFilename( textBuffer );
		sprintf(msgBuffer, "closeFile: file %s is not open", p);
		msg(msgBuffer, 1 );
		return 0;
	}
	
	/* see if the scrap points into this file */
	if( ff->origHandle == scrapBuffer.fileId && scrapBuffer.type == 0 )
		/* this call only copies the scrap text to the add file */
		/* it does not really do any insert (just using the code) */
		insScrap( 0, 0 );

	/* invalidate any buffers allocated to this open file */
	if( ff->origHandle != addHandle )
		fidInvalid(ff->origHandle, fileId);
		
	/* free the command history items */
	FreeCommandHistory( ff );
	
	/* close the original file for this open file */
	if( ff->origHandle != addHandle ) {
		i = close(ff->origHandle);
		if( i != 0 ) {
			strcpy( textBuffer, ff->origName );
			p = tildefyFilename( textBuffer );
			sprintf(msgBuffer,
				"closeFile: close of %s failed, ret=%d", p, i);
			msg(msgBuffer, 1 );
		}
	}

	/* rename the saved file */
	if( writing == 1) {
		MakeBackups( ff, tempFile );
	}

ViewOnly:
	/* free all the pieces */
	freePieces(ff->pieceList);

	/* indicate the file structure is free */
	ff->origHandle = -1;

	return 0;	/* all ok */
}
