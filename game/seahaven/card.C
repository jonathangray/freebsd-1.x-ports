/*
 * Author:  Terry Weissman
 *          weissman@sgi.com
 */

#include "seahaven.h"

extern char *card_bits[52];

struct WindowCardRec {
    Window window;
    Card card;
};

typedef struct WindowCardRec *WindowCard;

static WindowCardRec windowcard[100];
static int numwindowcard = 0;

static GC highlightgc;

static void RegisterWindow(Card card, Window window) {
    int i;
    if (numwindowcard == 0 || window > windowcard[numwindowcard - 1].window) {
	windowcard[numwindowcard].window = window;
	windowcard[numwindowcard].card = card;
	numwindowcard++;
	return;
    }
    for (i=0 ; i<numwindowcard ; i++) {
	if (windowcard[i].window > window) {
	    for (int j = numwindowcard ; j > i ; j--) {
		windowcard[j] = windowcard[j-1];
	    }
	    windowcard[i].window = window;
	    windowcard[i].card = card;
	    numwindowcard++;
	    return;
	}
    }
    Punt("Bug in RegisterWindow!");
}


Card WindowToCard(Window w) {
    int l, h, i;
    l = 0;
    h = numwindowcard;
    while (l < h) {
	i = (l + h) / 2;
	if (w == windowcard[i].window) return windowcard[i].card;
	if (w > windowcard[i].window) l = i + 1;
	else h = i;
    }
    if (w == windowcard[i].window) return windowcard[i].card;
    return NULL;
}



CardRec::CardRec(int s, int v, unsigned long fore, unsigned long back,
		 char *bitmap) {
    suit = s;
    value = v;
    XSetWindowAttributes attributes;
    long valuemask = CWEventMask | CWBackPixmap;
    attributes.event_mask =
	ButtonPressMask | ButtonReleaseMask | ButtonMotionMask;
    attributes.background_pixmap =
	XCreatePixmapFromBitmapData(dpy, toplevel, bitmap,
				    CARDWIDTH, CARDHEIGHT, fore, back,
				    DefaultDepth(dpy, screen));
    window = XCreateWindow(dpy, toplevel, v * CARDWIDTH, s * CARDHEIGHT,
			   CARDWIDTH, CARDHEIGHT, 0, (int) CopyFromParent,
			   InputOutput, (Visual *) CopyFromParent,
			   valuemask, &attributes);
    XMapWindow(dpy, window);
    RegisterWindow(this, window);
}


Stack CardRec::getStack() {
    return stack;
}

// single stacks are symmetric for saving purposes
Stack CardRec::getFoldedStack() {
    if (stack ==singlestack[0] || stack == singlestack[1]
		    || stack == singlestack[2] || stack == singlestack[3])
      return singlestack[0];
    else
      return stack;
}

int CardRec::getSuit() {
    return suit;
}

int CardRec::getValue() {
    return value;
}

int CardRec::getX() {
    return x;
}

int CardRec::getY() {
    return y;
}

// single stacks are symmetric for saving purposes
int CardRec::getFoldedY() {
    if (stack ==singlestack[0] || stack == singlestack[1]
		    || stack == singlestack[2] || stack == singlestack[3])
      return 0;
    else
      return y;
}

void CardRec::setLoc(int newx, int newy) {
    if (x != newx || y != newy) {
	x = newx;
	y = newy;
	XMoveWindow(dpy, window, x, y);
    }
}


void CardRec::raise() {
    XRaiseWindow(dpy, window);
}


void CardRec::raiseAbove(Card c) {
    XWindowChanges values;
    values.sibling = c->window;
    values.stack_mode = Above;
    XConfigureWindow(dpy, window, CWSibling | CWStackMode, &values);
}


void CardRec::raiseBelow(Card c) {
    XWindowChanges values;
    values.sibling = c->window;
    values.stack_mode = Below;
    XConfigureWindow(dpy, window, CWSibling | CWStackMode, &values);
}

void CardRec::setStack(Stack s) {
    stack = s;
}


void CardRec::highlight() {
    XFillRectangle(dpy, window, highlightgc, 0, 0, CARDWIDTH, CARDHEIGHT);
}

void CardRec::repaint() {
    XClearWindow(dpy, window);
}



Bool CardHandleEvent(XEvent *event) {
    Card card = WindowToCard(event->xany.window);
    if (!card || event->type == KeyPress) return False;
    if (event->type == ButtonPress) {
	if (event->xbutton.button > 1) {
	    int suit = card->getSuit();
	    int value =
		card->getValue() + ((event->xbutton.button == 3) ? 1 : -1);
	    if (value < 0 || value >= NUMVALUES) return True;
	    cards[suit][value]->highlight();
	    XEvent ev;
	    do {
		GetInterestingEvent(&ev);
	    } while (ev.type != ButtonRelease);
	    cards[suit][value]->repaint();
	    return True;
	}
	Card list[20];
	int origx[20], origy[20];
	int num = 0;
	int i;
	for (; card ; card = card->getStack()->getCardAbove(card)) {
	    origx[num] = card->getX();
	    origy[num] = card->getY();
	    list[num++] = card;
	}
	list[num-1]->raise();
	for (i=num-2 ; i>=0 ; i--) {
	    list[i]->raiseBelow(list[i+1]);
	}
	Bool abort = False;
	for (;;) {
	    XEvent ev;
	    GetInterestingEvent(&ev);
	    if (ev.type == ButtonPress) {
		abort = True;
		break;
	    }
	    while (ev.type == MotionNotify && XPending(dpy)) {
		XEvent ev2;
		XPeekEvent(dpy, &ev2);
		if (ev2.type == Expose) {
		    XNextEvent(dpy, &ev2);
		    if (ev2.xexpose.count == 0) score->repaint();
		    continue;
		}
		if (ev2.type != MotionNotify) break;
		GetInterestingEvent(&ev);
	    }
	    if (ev.type == MotionNotify || ev.type == ButtonRelease) {
		for (i=0 ; i<num ; i++) {
		    list[i]->setLoc
			(origx[i] + ev.xbutton.x_root - event->xbutton.x_root,
			 origy[i] + ev.xbutton.y_root - event->xbutton.y_root);
		}
		if (ev.type == ButtonRelease) {
		    Stack stack =
			StackFromPoint(list[0]->getX(), list[0]->getY());
		    if (!stack || stack == list[0]->getStack()) {
			abort = True;
		    }
		    if (!abort && stack->getType() == Single) {
			if (NumAvailableSingles() < num) abort = True;
			if (!abort && !stack->addCard(list[num-1])) {
			    abort = True;
			}
			if (!abort) {
			    for (i=num-2 ; i>=0 ; i--) {
				GetAvailableSingle()->addCard(list[i]);
			    }
			    AutoMoves();
			    UndoBoundary();
			    break;
			}
		    }
		    for (i=1 ; i<num ; i++) {
			if (list[i]->getSuit() != list[i-1]->getSuit() ||
			   list[i]->getValue() != list[i-1]->getValue() - 1) {
			    abort = True;
			    break;
			}
		    }
		    if (NumAvailableSingles() < num - 1) abort = True;
		    if (!abort && num > 1) {
			Card top = stack->getTopCard();
			if ((top == NULL &&
			     list[0]->getValue() != NUMVALUES - 1)
			    || (top != NULL &&
				(top->getSuit() != list[0]->getSuit() ||
				 top->getValue() !=
				     list[0]->getValue() + 1))) {
			    abort = True;
			} else {
			    for (i=num-1 ; i>0 ; i--) {
				GetAvailableSingle()->addCard(list[i],
							      DontMove);
			    }
			}
		    }
		    if (!abort && !stack->addCard(list[0])) abort = True;

		    if (!abort) {
			for (i=1 ; i<num ; i++) {
			    if (!stack->addCard(list[i])) {
				Punt("Bug in CardHandleEvent!");
			    }
			}
			AutoMoves();
			UndoBoundary();
		    }
		    break;
		}
	    }
	}
	if (abort) {
	    for (i=0 ; i<num ; i++) list[i]->setLoc(origx[i], origy[i]);
	}
    }
    return True;
}
    
    


void CardInit() {
    unsigned long red = GetColor("red", BlackPixel(dpy, screen));
    unsigned long white = GetColor("white", WhitePixel(dpy, screen));
    unsigned long black = GetColor("black", BlackPixel(dpy, screen));

    for (int s=0 ; s<NUMSUITS ; s++) {
	for (int v=0 ; v<NUMVALUES ; v++) {
	    cards[s][v] = new CardRec(s, v, (s >= 2) ? black : red, white,
				      card_bits[s * 13 + v]);
	}
    }

    highlightgc = XCreateGC(dpy, toplevel, 0, NULL);
    XSetForeground(dpy, highlightgc,
		   GetColor("blue", BlackPixel(dpy, screen)));
}
