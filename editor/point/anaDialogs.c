/* $Header: /a/cvs/386BSD/ports/editor/point/anaDialogs.c,v 1.1 1994/02/15 22:12:34 jkh Exp $ */

#ifdef HYPERTEXT
#include <string.h>
#include "pt.h"

DBM *currentDB;
Document currentDocument;
AttributeID mainFileBlock;
MapID naturalMap;

PickListItem *
GenerateIDList( id_in, magic )
	ID id_in;
	MagicNumber magic;
{
	extern DBM *currentDB;

	PickListItem * itemListFront;
	PickListItem * itemListBack = NULL;
	ID id = id_in;
	AnaObject ana_object;
	int len;
	
	while( id != NullObject ) {
		/* allocate a new struct to hold the next item */
		PickListItem * new_item =
			(PickListItem *)PtMalloc(sizeof(PickListItem),"list");
		ana_object = GetObject( currentDB, magic, id, NO_ALLOCATE );
		/* copy the name and id into the struct */
		len = strlen(ana_object->name) + 1;
		new_item->name = (char *)PtMalloc( len, "name" );
		strcpy( new_item->name, ana_object->name );
		new_item->id = id;
		new_item->next = NULL;
		/* link it on the back of the list */
		if( itemListBack == NULL ) {
			itemListFront = new_item;
			itemListBack = new_item;
		} else {
			itemListBack->next = new_item;
			itemListBack = new_item;
		}
		/* and move to the next item */
		id = ana_object->next;
	}
	return itemListFront;
}

void
FreeIDList( itemList )
	PickListItem * itemList;
{
	while( itemList != NULL ) {
		PickListItem * toGo = itemList;
		itemList = itemList->next;
		PtFree( toGo->name );
		PtFree( (char *)toGo );
	}
}

static ID ID_chosen;

ID
AnaPickBox( id_in, magic, label_string )
	ID id_in;
	MagicNumber magic;
	String label_string;
{
	extern DBM *currentDB;
	extern char msgBuffer[];
	extern char * returnString;

	char *box_name, *s;
	PickListItem * itemList, *item;

	/* get the list of items */
	itemList = GenerateIDList( id_in, magic );
        /* Create the list box */
	(void)ExecTclCommand( "update", NULL );
	sprintf( msgBuffer, "MakePickBox {%s}", label_string );
	s = ExecTclCommand( msgBuffer, NULL );
	/* make a copy of the name */
	box_name = (char *)PtMalloc( strlen(s)+1, "name" );
	strcpy( box_name, s );
	/* fill the list box */
	item = itemList;
	while( item != NULL ) {
		sprintf( msgBuffer, "%s.slist.items insert end {%s}",
						box_name, itemList->name );
		(void)ExecTclCommand( msgBuffer, NULL );
		item = item->next;
	}

printf("FIX THIS UP LATER!\n"
#ifdef XXXXXXXXXXXXXXX
	/*  run a local event loop until the box is acted on */
	command( FWAITFORRETURNSTRING, "", "", "", "", "", "" );
	if( strcmp(returnString,"XXXcancelXXX") != 0 ) {
		int index = atoi(returnString);
		while( index-- > 0 )
			item = item->next;
		ID_chosen = item->id;
printf( "Picked item %d, id %d, name <%s>\n", index, item->id, item->name );
	} else
		ID_chosen = -1;
#endif

	FreeIDList( itemList );
	
	return ID_chosen;
}

AttributeID
PickAttribute( document, label_string )
	Document document;
	String label_string;
{
	return (AttributeID) AnaPickBox( (ID)(document->firstAttribute),
					AttributeMagic, label_string );
}

LinkID
PickLink( document, label_string )
	Document document;
	String label_string;
{
	return (LinkID) AnaPickBox( (ID)(document->firstLink),
					LinkMagic, label_string );
}

BlockID
PickBlock( document, label_string )
	Document document;
	String label_string;
{
	return (BlockID) AnaPickBox( (ID)(document->firstBlock),
					BlockMagic, label_string );
}

FileID
PickFile( document, label_string )
	Document document;
	String label_string;
{
	return (FileID) AnaPickBox( (ID)(document->firstFile),
					FileMagic, label_string );
}

TextID
PickText( document, label_string )
	Document document;
	String label_string;
{
	return (TextID) AnaPickBox( (ID)(document->firstText),
					TextMagic, label_string );
}

MapID
PickMap( document, label_string )
	Document document;
	String label_string;
{
	return (MapID) AnaPickBox( (ID)(document->firstMap),
					MapMagic, label_string );
}

ViewID
PickView( document, label_string )
	Document document;
	String label_string;
{
	return (ViewID) AnaPickBox( (ID)(document->firstView),
					ViewMagic, label_string );
}
#endif

