#include "seahaven.h"

struct HashBucket {
  struct {
    Stack s;
    int y;		// hack, really want position w/in stack as an index
  } cardpos[52];
  HashBucket *next;
};

static const int NUMHASHSLOTS = 337;

HashBucket *hashtable[NUMHASHSLOTS] = {NULL};

void
InitHashTable()
{
    for (int i=0; i< NUMHASHSLOTS; i++) {
	if (hashtable[i]) {
	    HashBucket *t = hashtable[i], *n;
	    do {
		n = t->next;
		delete t;
	    } while (t = n);
	    hashtable[i] = NULL;
	}
    }
}

#ifdef notdef
// the single stacks are symmetric, so store them as singlestack[0] and y=0
inline void FoldStack(Stack reals, int realy, Stack &s, int &y)
{
	extern Stack singlestack[];
	if (reals == singlestack[0] || reals == singlestack[1]
		|| reals == singlestack[2] || reals == singlestack[3]) {
	  s = singlestack[0];
	  y = 0;
	} else {
	  s = reals;
	  y = realy;
	}
}
#endif

// retruns true if found it
Bool
hashlookup(int key)
{
    if (!hashtable[key]) return False;

    HashBucket *t = hashtable[key];
    do {
	int i;
	Card *c;
	Bool match;

	for (i=0, c = &cards[0][0]; i < 52; i++, c++) {
	    match = True;
	    if ( (*c)->getFoldedY() != t->cardpos[i].y ||
		(*c)->getFoldedStack() != t->cardpos[i].s ) {
		match = False;
		break;
	    }
	}
	if (match)
	    return True;
    } while (t = t->next);

    return False;

}

// returns true if new state
Bool
CheckNewState()
{
    unsigned int hashkey = 0;

    // compute key
    int i;
    Card *c;
    for (i=0, c = &cards[0][0]; i < 52; i++, c++) {
	hashkey += ((int) (*c)->getFoldedStack() << (*c)->getValue());
    }
    
    if (hashlookup(hashkey % NUMHASHSLOTS))
	return False;
    else
	return True;
    
}

void
hashinsert(int key, HashBucket *pail)
{
    pail->next = hashtable[key];
    hashtable[key] = pail;
}

void
SaveState()
{
    
    HashBucket *pail = new HashBucket;
    unsigned int hashkey = 0;
    
    int i;
    Card *c;
    for (i=0, c = &cards[0][0]; i < 52; i++, c++) {
	pail->cardpos[i].s = (*c)->getFoldedStack();
	pail->cardpos[i].y = (*c)->getFoldedY();
	hashkey += ((int) pail->cardpos[i].s << (*c)->getValue());
    }
    
    hashinsert(hashkey % NUMHASHSLOTS, pail);
    
}
