/*
 * Author:  Terry Weissman
 *          weissman@sgi.com
 */



#include "seahaven.h"
#include "auto.h"

#include <sys/time.h>		// For time()
#include <stdlib.h>
#include <X11/cursorfont.h>

Stack playstack[NUMPLAYSTACK];
Stack singlestack[NUMSINGLESTACK];
Stack donestack[NUMSUITS];


static Stack allstacks[100];
static int numstacks = 0;

static int undoing = False;

class StackPlayRec : public StackRec {
  public:
    StackPlayRec(int x, int y);
    virtual Bool addCard(Card, AnimType animate = Normal);
    virtual StackType getType();
  private:
};

// really want to make this static member of StackPlayRec
static Bool initializing = False;

// make this friend of StackPlayRec
void SetInitializing(Bool onoff) {
    initializing = onoff;
}

class StackSingleRec : public StackRec {
  public:
    StackSingleRec(int x, int y);
    virtual Bool addCard(Card, AnimType animate = Normal);
    virtual StackType getType();
  private:
};


class StackDoneRec : public StackRec {
  public:
    StackDoneRec(int x, int y);
    virtual Bool addCard(Card, AnimType animate = Normal);
    virtual int getStackingHeight();
    virtual StackType getType();
  private:
};


UndoListRec::UndoListRec() {
    max = 100;
    cards = (Card *) malloc(max * sizeof(Card));
    stacks = (Stack *) malloc(max * sizeof(Stack));
    clear();
}


void UndoListRec::clear() {
    num = 0;
}

void UndoListRec::add(Card c, Stack s) {
    if (num >= max) {
	max += 100;
	cards = (Card *) realloc((char *) cards, max * sizeof(Card));
	stacks = (Stack *) realloc((char *) stacks, max * sizeof(Stack));
    }
    cards[num] = c;
    stacks[num] = s;
    num++;
}

void UndoListRec::addBoundary() {
    if (num > 0 && cards[num - 1] != NULL) add(NULL, NULL);
}


void UndoListRec::doUndo() {
    SetInitializing(True);		// Hack to fool StackPlayRec::addCard.
    if (num > 0 && cards[num - 1] == NULL) num--;
    while (num > 0 && cards[num - 1] != NULL) {
	num--;
	stacks[num]->addCard(cards[num]);
    }
    SetInitializing(False);
    for (int i=0 ; i<NUMPLAYSTACK ; i++) playstack[i]->repositionAll();
}

Bool UndoListRec::isEmpty() {
    for (int i = 0 ; i<num ; i++) if (cards[i]) return False;
    return True;
}
	
UndoListRec undostack;
UndoListRec redostack;

void AddUndo(Card c, Stack s) {
    UndoList list = undoing ? &redostack : &undostack;
    list->add(c, s);
}


void UndoBoundary() {
    undostack.addBoundary();
    redostack.clear();
}


void DoUndo() {
    undoing = True;
    undostack.doUndo();
    undoing = False;
    redostack.addBoundary();
}

void DoRedo() {
    redostack.doUndo();
    undostack.addBoundary();
}


void DoRestart() {
    while (!undostack.isEmpty()) DoUndo();
}

StackRec::StackRec(int newx, int newy) {
    x = newx;
    y = newy;
    numcards = 0;
    allstacks[numstacks++] = this;
}

void StackRec::clear() {
    numcards = 0;
}

int StackRec::getX() {
    return x;
}

int StackRec::getY() {
    return y;
}

Card StackRec::getTopCard() {
    if (numcards == 0) return NULL;
    return cards[numcards - 1];
}

Card StackRec::getCardAbove(Card c) {
    int i;
    for (i=0 ; i<numcards - 1 ; i++) {
	if (cards[i] == c) return cards[i + 1];
    }
    return NULL;
}


static int Abs(int x) {
    return (x < 0) ? -x : x;
}


static int Max(int a, int b) {
    return (a > b) ? a : b;
}


void StackRec::repositionAll() {
    for (int i=0 ; i<numcards ; i++) {
	cards[i]->setLoc(x, y + i * getStackingHeight());
    }
}

Card StackRec::popCard() {
    if (numcards == 0) return NULL;
    numcards--;
    return cards[numcards];
}

Bool StackRec::addCard(Card c, AnimType animate) {
    if (c->getStack() == this) Punt("Error in StackRec::addCard!");
    if (animate == Animate) {
	c->raise();
	int oldx = c->getX();
	int oldy = c->getY();
	int newx = x;
	int newy = y + numcards * getStackingHeight();
	int numsteps = Max(Abs(oldx - newx), Abs(oldy - newy)) / speedup;
	float curx = (float) oldx;
	float cury = (float) oldy;
	for (int i = 0 ; i<numsteps ; i++) {
	    curx += ((float) (newx - oldx)) / numsteps;
	    cury += ((float) (newy - oldy)) / numsteps;
	    c->setLoc((int) curx, (int) cury);
	    XFlush(dpy);
	}
    }
    if (animate != DontMove) {
	if (numcards > 0) c->raiseAbove(cards[numcards - 1]);
	c->setLoc(x, y + numcards * getStackingHeight());
	if (inautoplay) XFlush(dpy);
    }
    Stack old = c->getStack();
    if (old && old->popCard() != c) {
	Punt("Couldn't find card in old stack in StackRec::addCard!");
    }
    AddUndo(c, c->getStack());
    c->setStack(this);
    cards[numcards++] = c;
    return True;
}


int StackRec::getStackingHeight() {
    return 22;
}

StackType StackRec::getType() {
    Punt("Invalid call to base class: StackRec::getType()");
    return Single;
}

StackPlayRec::StackPlayRec(int x, int y) : (x, y) {
}


Bool StackPlayRec::addCard(Card c, AnimType animate) {
    if (!initializing) {
	if (numcards == 0) {
	    if (c->getValue() != NUMVALUES - 1) return False;
	} else {
	    if (c->getSuit() != cards[numcards - 1]->getSuit() ||
		   c->getValue() != cards[numcards - 1]->getValue() - 1)
		return False;
	}
    }
    return StackRec::addCard(c, animate);
}

StackType StackPlayRec::getType() {
    return Play;
}


StackSingleRec::StackSingleRec(int x, int y) : (x, y) {
    XSetWindowAttributes attributes;
    long valuemask = CWBackPixel | CWBorderPixel;
    attributes.background_pixel = GetColor("darkgreen",
					   WhitePixel(dpy, screen));
    attributes.border_pixel = GetColor("green", BlackPixel(dpy, screen));
    Window w = XCreateWindow(dpy, toplevel, x - 1, y - 1,
			     CARDWIDTH, CARDHEIGHT, 1, (int) CopyFromParent,
			     InputOutput, (Visual *) CopyFromParent,
			     valuemask, &attributes);
    XLowerWindow(dpy, w);
    XMapWindow(dpy, w);
}

StackSingleRec::addCard(Card c, AnimType animate) {
    if (numcards) return False;
    return StackRec::addCard(c, animate);
}

StackType StackSingleRec::getType() {
    return Single;
}


StackDoneRec::StackDoneRec(int x, int y) : (x, y) {
}

StackDoneRec::addCard(Card c, AnimType animate) {
    if (numcards == 0) {
	if (c->getValue() != 0) return False;
    } else {
	if (c->getSuit() != cards[numcards - 1]->getSuit() ||
	      c->getValue() != cards[numcards - 1]->getValue() + 1)
	    return False;
    }
    return StackRec::addCard(c, animate);
}

StackDoneRec::getStackingHeight() {
    return 0;
}

StackType StackDoneRec::getType() {
    return Done;
}


void AutoMoves() {
    Bool found;
    int i;
    do {
	found = False;
	for (i=0 ; i<numstacks ; i++) {
	    Card card = allstacks[i]->getTopCard();
	    if (card) {
		Card dcard = donestack[card->getSuit()]->getTopCard();
		if (card->getValue() == (dcard ? dcard->getValue() + 1 : 0)) {
		    donestack[card->getSuit()]->addCard(card,
							inautoplay ? Normal
							: Animate);
		    found = True;
		}
	    }
	}
    } while (found);
    for (i=0 ; i<NUMSUITS ; i++) {
	Card card = donestack[i]->getTopCard();
	if (card == NULL ||
	    donestack[i]->getTopCard()->getValue() < NUMVALUES - 1) return;
    }
    score->wonGame();
}


int NumAvailableSingles() {
    int result = 0;
    for (int i=0 ; i<NUMSINGLESTACK ; i++) {
	if (singlestack[i]->getTopCard() == NULL) result++;
    }
    return result;
}


Stack GetAvailableSingle() {
    for (int i=0 ; i<NUMSINGLESTACK ; i++) {
	if (singlestack[i]->getTopCard() == NULL) return singlestack[i];
    }
    return NULL;
}


Stack StackFromPoint(int x, int y) {
    Stack *checklist;
    int num;
    if (y < playstack[0]->getY() - 20) {
	checklist = singlestack;
	num = NUMSINGLESTACK;
    } else {
	checklist = playstack;
	num = NUMPLAYSTACK;
    }
    for (int i=0 ; i<num ; i++) {
	if (x >= checklist[i]->getX() - CARDWIDTH / 2 &&
	    x < checklist[i]->getX() + CARDWIDTH / 2) return checklist[i];
    }
    return NULL;
}


void ClearAllStacks() {
    for (int i=0 ; i<numstacks ; i++) {
	allstacks[i]->clear();
    }
}


void LoadStacks(Card deck[52]) {
    int i, j, k, w;
    SetInitializing(True);
    for (i=0 ; i<numstacks ; i++) {
	allstacks[i]->clear();
    }
    for (int s = 0 ; s < NUMSUITS ; s++) {
	for (int v = 0 ; v < NUMVALUES ; v++) {
	    cards[s][v]->setStack(NULL);
	}
    }
    w = 0;
    for (k=0 ; k<CARDSPERPLAYSTACK ; k++) {
	for (j=0 ; j<NUMPLAYSTACK ; j++) {
	    playstack[j]->addCard(deck[w++]);
	}
    }
    for (j=0 ; j<2 ; j++) {
	singlestack[j + 1]->addCard(deck[w++]);
    }
    SetInitializing(False);

    AutoMoves();
    undostack.clear();
    redostack.clear();
}


class StackNoticeExitRec {
  public:
    StackNoticeExitRec() {}
    ~StackNoticeExitRec();
};


StackNoticeExitRec::~StackNoticeExitRec() {
    if (score->getGameWonOrLost()) score->NewGame(True);	// Force save.
}


StackNoticeExitRec noticeexit;

void StackInit() {
    int i;
    srandom(int(time(NULL)));

    for (i=0 ; i<NUMPLAYSTACK ; i++) {
	playstack[i] = new StackPlayRec(i * 70 + 10, 100);
    }
    for (i=0 ; i<NUMSINGLESTACK ; i++) {
	singlestack[i] = new StackSingleRec(i * 70 + 220, 10);
    }
    donestack[0] = new StackDoneRec(10, 10);
    donestack[1] = new StackDoneRec(80, 10);
    donestack[2] = new StackDoneRec(570, 10);
    donestack[3] = new StackDoneRec(640, 10);
}




void DoAutoPlay() {
    static Cursor waitcursor = (Cursor) NULL;
    if (waitcursor == (Cursor) NULL) waitcursor = XCreateFontCursor(dpy, XC_watch);
    XDefineCursor(dpy, toplevel, waitcursor);
    DoRestart();
    XFlush(dpy);
    inautoplay = True;
    int solvable = AutoPlay();
    if (solvable) {
    score->lostGame();
	score->setMessage("Solvable.");
	while (solutionhead) {
	    SolutionLog temp = solutionhead;
	    solutionhead = solutionhead->next;
	    Card card = cards[temp->suit][temp->rank];
	    if (temp->dest == -999) {
		AutoMoves();
		UndoBoundary();
	    } else if (temp->dest == -99) {
		GetAvailableSingle()->addCard(card);
	    } else if (temp->dest <= NUMPLAYSTACK) {
		playstack[temp->dest - 1]->addCard(card);
	    } else {
		if (temp->rank == NUMVALUES - 1) { // King to empty stack.
		    for (int i=0 ; i<NUMPLAYSTACK ; i++) {
			if (playstack[i]->getTopCard() == NULL) {
			    playstack[i]->addCard(card);
			    break;
			}
		    }
		} else {
		    cards[temp->suit][temp->rank+1]->getStack()->addCard(card);
		}
	    }
	    delete temp;
	    XFlush(dpy);	// For debugging.
	}
    } else {
      score->wonGame();
	score->setMessage("Unsolvable.");
    }
    inautoplay = False;
    XUndefineCursor(dpy, toplevel);
}
