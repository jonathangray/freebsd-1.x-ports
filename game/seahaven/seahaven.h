/*
 * Author:  Terry Weissman
 *          weissman@sgi.com
 */

#define NeedFunctionPrototypes 1


#include <X11/Xlib.h>

//#ifndef NULL
//#define	NULL 0
//#endif NULL

#ifdef MAIN
#define global
#else
#define global extern
#endif


typedef class StackRec *Stack;
typedef class CardRec *Card;

class CardRec {
  public:
    CardRec(int suit, int value, unsigned long fore, unsigned long back,
	    char *bitmap);
    Window getWindow();
    Stack getStack();
    int getSuit();
    int getValue();
    int getX();
    int getY();
    Stack getFoldedStack();
    int getFoldedY();
    void setLoc(int, int);
    void raise();
    void raiseAbove(Card);
    void raiseBelow(Card);
    void setStack(Stack);	// To be called only by StackRec::addCard
    void highlight();
    void repaint();
  private:
    int suit;
    int value;
    Window window;
    int x, y;
    Stack stack;
};


enum StackType {Single, Play, Done};
enum AnimType {Normal, Animate, DontMove};

class StackRec {
  public:
    StackRec(int x, int y);
    virtual void clear();
    virtual int getX();
    virtual int getY();
    virtual Card getTopCard();
    virtual Card getCardAbove(Card);
    virtual Card popCard();
    virtual Bool addCard(Card, AnimType animate = Normal);
				// Return TRUE iff successful.
    virtual void repositionAll();
    virtual int getStackingHeight();
    virtual StackType getType();
  protected:
    int x, y;
    int numcards;
    Card cards[20];
  private:
    friend class Stack_Iterator;
};

class Stack_Iterator {
  public:
    Stack_Iterator(Stack s) { stack = s; i = s->numcards; }
    Card operator()() {
      if (--i < 0)
        return 0;
      else
        return stack->cards[i];
    }
  private:
    Stack stack;
    int i;	// current index
};


class ScoreRec {
  public:
    ScoreRec();
    void repaint(Bool erasefirst = False);
    void wonGame();
    void lostGame();
    void newGame();
    Bool getGameWonOrLost();
    void loadGame(char *);
    void NewGame(Bool losegame = True);
    void saveGameBegin();
    void saveGameCard(Card);
    void saveGameEnd();
    void setMessage(char *);
  private:
    void printLine(char *, int value = 0, char *plural = "s");
    Window window;
    GC gc;
    int wins, losses, streak, maxwinstreak, maxlosestreak;
    int wonlast;		// Should be Bool, but this makes save easier
    int curx, cury;
    char savefile[512];
    Card deck[52];
    Bool woncurgame;
    Bool lostcurgame;
    char *message;
};

typedef struct ScoreRec *Score;

class UndoListRec {
  public:
    UndoListRec();
    void clear();
    void add(Card, Stack);
    void addBoundary();
    void doUndo();
    Bool isEmpty();
  private:
    int num;
    int max;
    Card *cards;
    Stack *stacks;
};

typedef class UndoListRec *UndoList;

static const int NUMSUITS = 4;
static const int NUMVALUES = 13;

static const int CARDWIDTH = 48;
static const int CARDHEIGHT = 66;

static const int GAMEWIDTH = 700;
static const int GAMEHEIGHT = 550;

static const int NUMPLAYSTACK = 10;
static const int NUMSINGLESTACK = 4;
static const int CARDSPERPLAYSTACK = 5;



global char *progname;
global Display *dpy;
global Window toplevel;
global int screen;
global Colormap cmap;
global Bool hascolor;
global Card cards[NUMSUITS][NUMVALUES];
global int speedup;
global Score score;
global XFontStruct *font;
global unsigned long backpixel;
global Bool inautoplay;

// From main.C

void Punt(char *);

// From score.C
void ScoreInit();

// From util.C

extern unsigned long GetColor(char *, unsigned long);
extern void GetInterestingEvent(XEvent *);
extern Window CreateButtonWindow(char *, int, int, int, int *);



// From card.C

void CardInit();
Bool CardHandleEvent(XEvent *);


// From stack.C

void StackInit();
void AutoMoves();
void SetInitializing(Bool);
int NumAvailableSingles();
Stack GetAvailableSingle();
Stack StackFromPoint(int x, int y);
void ClearAllStacks();
void DoUndo();
void DoRedo();
void DoRestart();
void UndoBoundary();
void LoadStacks(Card deck[52]);
extern Stack playstack[NUMPLAYSTACK];
extern Stack singlestack[NUMSINGLESTACK];
extern Stack donestack[NUMSUITS];
void DoAutoPlay();
