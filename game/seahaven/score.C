/*
 * Author:  Terry Weissman
 *          weissman@sgi.com
 */

#include "seahaven.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>		// For getenv()
#include <math.h>		// For random()

void ScoreInit() {
    char savefile[512];
    score = new ScoreRec();
    sprintf(savefile, "%s/.seahavensave", getenv("HOME"));
    score->loadGame(savefile);
}

void ScoreRec::loadGame(char *filename){
    (void) strcpy(savefile, filename);
    FILE *fid = fopen(savefile, "r");
    if (fid) {
	fscanf(fid, "%d %d %d %d %d %d\n", &wins, &losses, &streak,
	       &maxwinstreak, &maxlosestreak, &wonlast);
	for (int i=0 ; i<52 ; i++) {
	    int suit, value;
	    fscanf(fid, "%d %d\n", &suit, &value);
	    deck[i] = cards[suit][value];
	}
	fclose(fid);
	LoadStacks(deck);
    } else {
	wins = 0;
	streak = 0;
	losses = 0;
	maxwinstreak = 0;
	maxlosestreak = 0;
	wonlast = True;
	NewGame(False);
    }
}


static int Random(int i) {
    return random() % i;
}

void ScoreRec::NewGame(Bool losegame) {
    int i, j, k, w;
    if (losegame) {
	lostGame();	// Has no effect if wonGame() already called.
	newGame();
    }
    SetInitializing(True);
    ClearAllStacks();
    Card deck[NUMSUITS * NUMVALUES];
    i = 0;
    for (int s = 0 ; s < NUMSUITS ; s++) {
	for (int v = 0 ; v < NUMVALUES ; v++) {
	    deck[i++] = cards[s][v];
	    cards[s][v]->setStack(NULL);
	}
    }
    saveGameBegin();
    for (k=0 ; k<CARDSPERPLAYSTACK ; k++) {
	for (j=0 ; j<NUMPLAYSTACK ; j++) {
	    w = Random(i);
	    playstack[j]->addCard(deck[w]);
	    saveGameCard(deck[w]);
	    deck[w] = deck[--i];
	}
    }
    for (j=0 ; j<i ; j++) {
	singlestack[j + 1]->addCard(deck[j]);
	saveGameCard(deck[j]);
    }
    saveGameEnd();
    SetInitializing(False);

    AutoMoves();
    extern UndoListRec undostack;
    extern UndoListRec redostack;
    undostack.clear();
    redostack.clear();
}

ScoreRec::ScoreRec() {
    int x = 10;
    int y = GAMEHEIGHT - 20 - 5 * (font->ascent + font->descent);
    XSetWindowAttributes attributes;
    long valuemask = CWEventMask | CWBackPixel | CWWinGravity;
    attributes.event_mask = ExposureMask;
    attributes.background_pixel = GetColor("forestgreen",
					   WhitePixel(dpy, screen));
    attributes.win_gravity = SouthWestGravity;
    window = XCreateWindow(dpy, toplevel, x, y,
			   GAMEWIDTH, GAMEHEIGHT, 0, (int) CopyFromParent,
			   InputOutput, (Visual *) CopyFromParent,
			   valuemask, &attributes);
    XLowerWindow(dpy, window);
    XMapWindow(dpy, window);

    gc = XCreateGC(dpy, window, 0, NULL);
    XSetForeground(dpy, gc, GetColor("black", BlackPixel(dpy, screen)));
    XSetFont(dpy, gc, font->fid);
    woncurgame = lostcurgame = False;
    message = "";
}



void ScoreRec::printLine(char *str, int value, char *plural) {
    char buf[200];
    cury += font->ascent + font->descent;
    sprintf(buf, str, value, value == 1 ? "" : plural);
    if (buf[0]) XDrawString(dpy, window, gc, curx, cury, buf, strlen(buf));
}



void ScoreRec::repaint(Bool erasefirst) {
    curx = 0;
    cury = 0;
    if (erasefirst) XClearWindow(dpy, window);
    printLine("%5d win%s", wins);
    printLine("%5d loss%s", losses, "es");
    printLine("%5d game%s played", wins + losses);
    curx = GAMEWIDTH / 2;
    cury = 0;
    if (streak > 0) {
	if (wonlast) printLine("%5d game winning streak", streak);
	else printLine("%5d game losing streak", streak);
    }
    if (maxwinstreak > 0) {
	printLine("%5d game%s in longest winning streak", maxwinstreak);
    }
    if (maxlosestreak > 0) {
	printLine("%5d game%s in longest losing streak", maxlosestreak);
    }
    curx /= 2;
    printLine(message);
}


void ScoreRec::wonGame() {
    if (!woncurgame && !lostcurgame) {
	woncurgame = True;
	wins++;
	if (!wonlast) streak = 0;
	streak++;
	if (maxwinstreak < streak) maxwinstreak = streak;
	wonlast = True;
	message = "";
	repaint(True);
    }
}



void ScoreRec::lostGame() {
    if (!woncurgame && !lostcurgame) {
	lostcurgame = True;
	losses++;
	if (wonlast) streak = 0;
	streak++;
	if (maxlosestreak < streak) maxlosestreak = streak;
	wonlast = False;
	message = "";
	repaint(True);
    }
}


void ScoreRec::newGame() {
    woncurgame = lostcurgame = False;
    setMessage("");
}


Bool ScoreRec::getGameWonOrLost() {
    return woncurgame || lostcurgame;
}


void ScoreRec::saveGameBegin() {
    curx = 0;
}

void ScoreRec::saveGameCard(Card c) {
    deck[curx++] = c;
}

void ScoreRec::saveGameEnd() {
    if (curx != 52) Punt("Bug in ScoreRec::saveGameEnd!");
    FILE *fid = fopen(savefile, "w");
    if (!fid) Punt("Can't open file in ScoreRec::saveGameEnd!");
    fprintf(fid, "%d %d %d %d %d %d\n", wins, losses, streak,
	    maxwinstreak, maxlosestreak, wonlast);
    for (int i=0 ; i<52 ; i++) {
	fprintf(fid, "%d %d\n", deck[i]->getSuit(), deck[i]->getValue());
    }
    fclose(fid);
}


void ScoreRec::setMessage(char *msg) {
    if (strcmp(message, msg)) {
	char *oldmsg = message;
	message = msg;
	repaint(*oldmsg != 0);
    }
}
