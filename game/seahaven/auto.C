
#include <stream.h>
#include <fstream.h>

#include <strings.h>
#include <stdlib.h>

#include <sys/types.h>
#include <sys/time.h>
#include <fcntl.h>
#include <stdio.h>


#include "auto.h"

#undef BATCH
#undef INTERACTIVE
#undef VERBOSE
#undef DYNAMIC
#undef AUTO

#define AUTO

#ifdef BATCH
int solved_count = 0;
int failed_count = 0;
int solved_moves = 0;
int solved_moves_max = 0;
int failed_moves = 0;
int failed_moves_max = 0;
int solved_time = 0;
int failed_time = 0;
#endif


struct timezone tz;
struct timeval start_time;
struct timeval end_time;

typedef int Boolean;

typedef enum Suit {
    SuitFirst, Hearts=SuitFirst, Diamonds, Clubs, Spades, SuitLast=Spades
    } Suit;

const int NumSuits = SuitLast+1;

typedef enum Rank {
    RankFirst, Ace=RankFirst, Two, Three, Four, Five, Six, Seven,
    Eight, Nine, Ten, Jack, Queen, King, RankLast=King
    } Rank;

const int NumRanks = RankLast+1;

void Error(char *s1 = "", char *s2 = "", char *s3 = "") {
    cerr << "\n" << s1 << s2 << s3 << "\n";
    exit(-1);
}



class Tableaux;

inline unsigned short Fold(long l) { return ((l>>16)^(l&0xffff)); }

class Hash {
    long l1;
    long l2;
    long l3;
  public:
    Hash(Tableaux&);
    Hash() { l1 = l2 = l3 = 0; }
    Hash(int i) { l1 = l2 = l3 = i; } // semi-private
    Boolean operator== (Hash& h) {
	return ((l1 == h.l1) && (l2 == h.l2) && (l3 == h.l3));
    }
    unsigned short Short()  { return Fold(l1)^Fold(l2)^Fold(l3); }
    Boolean EmptyP() { return (l1 == 0) && (l2 == 0) && (l3 == 0); }
    Boolean BadP() { return (l3 < 0); }
    void Print(ostream &os) { os << hex(l1, 8) << hex(l2, 8) << hex(l3,8); }
};

ostream& operator<< (ostream &os, Hash& h) { h.Print(os); return os; }

class HashList;

class HashTable {
    int size;
    int count;
    int hits;
    int maxDepth;
    HashList *ht;
  public:
    HashTable(int n);
    ~HashTable();
    int Count() { return count; }
    void Clear();
    Boolean CheckAndAdd(Hash& h);
/*
 {
	const unsigned short s = h.Short();
	Boolean ret = ht[s].InP(h);
	if (!ret) {
	    int depth = ht[s].Add(h);
	    if (depth > maxDepth) maxDepth = depth;
	    if (depth) hits++;
	    count++;
//	    if (count%1000 == 0) PrintStats();
#ifdef INTERACTIVE
	    if (count%1000 == 0) cerr << ".";
#endif
	}
	return ret;
    }
*/
    void Print(ostream &os);
    void PrintStats(ostream& os) {
#ifdef INTERACTIVE
	os << "\nHash table stats - "
	    << "size " << size
		<< " count " << count
		    << " hits " << hits
			<< " max depth " << maxDepth;
#endif
    }
};

class HashList {
    HashList *next;
    Hash hash;
    int depth;
    friend void HashTable::Clear();
  public:
    HashList(Hash& h, HashList* n) { hash = h; next = n; depth++; }
    HashList() { hash = Hash(); next = NULL; depth = 0; }
    ~HashList();
    Boolean InP(Hash& h);
    int Add(Hash &h); // returns depth
    Hash HashVal() { return hash; }
    int Depth() { return depth; }
    void Print(ostream& os);
    Boolean EmptyP() { return hash.EmptyP(); }
};

HashTable::HashTable(int n) {
    ht = new HashList[n]; size = n; count = maxDepth = hits = 0;
}

HashTable::~HashTable() {
    delete[] ht;
}

HashList::~HashList() {
    if (next) delete next;

}

void HashList::Print(ostream &os) {
    if (EmptyP()) os << "NULL";
    else {
	char *sep = "";
	for (HashList* hlt = this; hlt != NULL; hlt = hlt->next) {
	    os << sep << hlt->hash;
	    sep = " ";
	}
    }
}

ostream& operator<< (ostream &os, HashList& hl) { hl.Print(os); return os; }

Boolean HashList::InP(Hash& h) {
    for (HashList *hlt = this; hlt != NULL; hlt = hlt->next) {
	if (hlt->HashVal() == h) return 1;
    }
    return 0;
}

int HashList::Add(Hash& h) {
    if (!this) Error("Add to NULL HashList.");
    if (HashVal().EmptyP()) hash = h;
    else next = new HashList(h, next);
    return depth++;
}

ostream& operator<< (ostream& os, HashTable& ht) { ht.Print(os); return os; }

void HashTable::Clear() {
    for (int i=0; i<size; i++) {
	if (ht[i].next) delete ht[i].next;
	ht[i] = HashList();
    }
    count = maxDepth = hits = 0;
}

Boolean HashTable::CheckAndAdd(Hash& h) {
    const unsigned short s = h.Short();
    Boolean ret = ht[s].InP(h);
    if (!ret) {
	int depth = ht[s].Add(h);
	if (depth > maxDepth) maxDepth = depth;
	if (depth) hits++;
	count++;
#ifdef INTERACTIVE
	if (count%1000 == 0) cerr << ".";
#endif
    }
    return ret;
}

void HashTable::Print(ostream &os) {
    HashList* hll = &ht[size];
    for (HashList* hlt = ht; hlt != hll; hlt++)
	if (! hlt->EmptyP())
	    os << "\n" << dec(hlt-ht,5) << ": " << *hlt;
}

static HashTable hashTable(65536);

// typedef unsigned char CardVal;
typedef int CardVal;

class Card;
class CardRange;

class Dest {
  public:
    void MoveTo(Card&);
    void MoveTo(CardRange&);
};

void Dest::MoveTo(Card&) { Error("Dest::MoveTo(Card&)"); }
void Dest::MoveTo(CardRange&) { Error("Dest::MoveTo(CardRange&)"); }

typedef void (Dest::* MoveCardToMemberFunction)(Card&);
typedef void (Dest::* MoveCardRangeToMemberFunction)(CardRange&);

class Card : public Dest {
    Suit thisSuit;
    Rank thisRank;
    Boolean empty;
  public:
    Card(Suit suit, Rank rank) {
	if ((suit < SuitFirst) || (suit > SuitLast))
	    Error("Bad suit ", dec(suit), " in Card constructor.");
	if ((rank < RankFirst) || (rank > RankLast))
	    Error("Bad rank ", dec(rank), " in Card constructor.");
	thisSuit = suit; thisRank = rank; empty = 0;
    }
    Card(char *st);
    Card() { // create an "empty" card
	thisSuit = SuitFirst; thisRank = RankFirst; empty = 1;
    } 
    Suit suit() const { return thisSuit; }
    Rank rank() const { return thisRank; }
    Card Next(int delta = 1) const { return Card(suit(), Rank(rank()+delta)); }
    Card Prev(int delta = 1) const { return Card(suit(), Rank(rank()-delta)); }
    Boolean NextP(Card c) const {
	return ((suit() == c.suit()) && rank()+1 == c.rank());
    }
    Boolean operator== (Card &c) {
	return (EmptyP()
		? c.EmptyP()
		: ((! c.EmptyP())
		   && (suit() == c.suit())
		   && (rank() == c.rank())));
    }
    Boolean operator!= (Card &c) { return (! operator==(c)); }
    Boolean EmptyP() const { return empty; }
    CardVal Value() const { return thisSuit*NumRanks+thisRank; }//semi-private.
    void Print(ostream& os) const;
};

static char *rankName = "A23456789TJQK";
static char *suitName = "HDCS";

void Card::Print(ostream &os) const {
    if (EmptyP())
	os << "--";
    else
	os << char(rankName[rank()]) << char(suitName[suit()]);
}

Card::Card(char* st) {
    char *c;
    c = index(suitName, st[1]);
    if (! c)
	Error("Bad suit ", st+1, " in Card string constructor.");
    thisSuit = Suit(c-suitName);
    c = index(rankName, st[0]);
    if (! c)
	Error("Bad rank ", st, " in Card string constructor.");
    thisRank = Rank(c - rankName);
    empty = 0;
}

ostream& operator<< (ostream& os, Card c) { c.Print(os); return os; }

typedef Card Deck[NumSuits*NumRanks];

// typedef unsigned char Count;
typedef int Count;

class CardRange : public Dest {
    Card thisCard;
    Count thisCount;
    void Init(Card c, Count count) { thisCard = c; thisCount = count; }
  public:
    CardRange(Suit s, Rank rank1, Rank rank2) {
	if (rank2 < rank1) Error("rank2 < rank1 in CardRange constructor.");
	Init(Card(s, rank1), rank2-rank1+1);
    }
    CardRange(Suit s, Rank rank, Count count = 1) {
	Init(Card(s, rank), count);
    }
    CardRange(Card c, Count count = 1) { Init(c, count); }
    CardRange() { Init(Card(), 0); }
//    CardRange(char *st) { Init(Card(st), 1); }
    Suit suit() const { return thisCard.suit(); }
    Card First() const { return thisCard; }
    Card Last() const { return thisCard.Next(thisCount-1); }
    Card Next() const { return thisCard.Next(thisCount); }
    Count Count() const { return thisCount; }
    void Prepend(const CardRange& cr) {
	if (!cr.NextP(First())) Error("CardRange::Prepend, not next.");
	thisCount += cr.Count();
	thisCard = cr.First();
    }
    Boolean NextP(Card c) const {
	return ((suit() == c.suit()) && (First().rank()+Count() == c.rank()));
    }
    Boolean EmptyP() const { return thisCard.EmptyP(); }
    void Print(ostream& os) const;
};

void CardRange::Print(ostream &os) const {
    if (EmptyP()) os << "--   ";
    else {
	os << First();
	if (Count() == 1) os << "   ";
	else os << "-" << Last();
    }
}

ostream& operator<< (ostream& os, CardRange c) { c.Print(os); return os; }


#ifdef AUTO
SolutionLog solutionhead = NULL;

//overload LogSolution();

static void LogOneSolution(Card card, int dest) {
    SolutionLog temp = new SolutionLogRec;
    temp->rank = card.rank();
    temp->suit = card.suit();
    temp->dest = dest;
    temp->next = solutionhead;
    solutionhead = temp;
}

static void LogBoundary() {
    SolutionLog temp = new SolutionLogRec;
    temp->rank = 0;
    temp->suit = 0;
    temp->dest = -999;
    temp->next = solutionhead;
    solutionhead = temp;
}    

static void LogSolution(Card card, int dest) {
    LogBoundary();
    LogOneSolution(card, dest);
}

static void LogSolution(CardRange range, int dest) {
    if (range.EmptyP()) return;
    LogBoundary();
    Suit suit = range.suit();
    Rank first = range.First().rank();
    Rank last = range.Last().rank();
    Rank i;
    if (dest != -99) {
	for (i=first ; i<last ; i++) LogOneSolution(Card(suit, i), dest);
    }
    LogOneSolution(Card(suit, last), dest);
    for (i=Rank(last-1) ; i>=first ; i--) LogOneSolution(Card(suit, i), -99);
}
#endif


const int NumCardRangesInPile = 5;

class Pile : public Dest {
    CardRange data[NumCardRangesInPile];
    int ti;
  public:
    Pile() { ti = 4; }
    CardRange& operator[] (int i) { return data[i]; }
    void Set(int i, const CardRange cr) { data[i] = cr; }
    CardRange& top() { return data[ti]; }
    int topIndex() { return ti; }
    CardRange& pop() { return data[ti--]; }
    Boolean EmptyP() { return (ti < 0); }
    void Test(CardRange& from);
    void MoveToPile(CardRange& from) {
	top().Prepend(from);
	from = CardRange();
    }
    void MoveToPile(Card& from) {
	top().Prepend(from);
	from = Card();
    }
    void MoveCardRangeToPile(CardRange& from);
    void MoveTo(CardRange& from);
    void MoveTo(Card& from);
};

void Pile::MoveTo(CardRange& from) { MoveToPile(from); }

void Pile::MoveCardRangeToPile(CardRange& from) { MoveToPile(from); }

void (Pile::* foo)(CardRange&) = &Pile::MoveCardRangeToPile;
//MoveCardRangeToMemberFunction foo = &Pile::MoveCardRangeToPile;

const int NumSpaces = 4;

class Spaces : public Dest {
    Card data[NumSpaces];
  public:
    Card& operator[] (int i) { return data[i]; }
    Card* FirstFree() {
	return (data[0].EmptyP() ? &data[0] :
		data[1].EmptyP() ? &data[1] :
		data[2].EmptyP() ? &data[2] :
		data[3].EmptyP() ? &data[3] :
		NULL);
    }
    void Spaces::MoveToSpace(CardRange& from) {
	// move all of the cards in range to spaces
	for (int i = 0 ; i < from.Count(); i++) {
	    *FirstFree() = from.First().Next(i);
	}
    from = CardRange();
    }
    void Spaces::MoveTo(CardRange& from);
    void Spaces::MoveTo(Card&);
};

void Spaces::MoveTo(CardRange& from) { MoveToSpace(from); }
void Spaces::MoveTo(Card&) { Error("Spaces::MoveTo(Card&)"); };

const int NumPiles = 10;

class Aces : public Dest {
    Card data[NumSuits];
  public:
    Card& operator[] (int i) { return data[i]; }
    void MoveToAce(Card& from) {
	data[from.suit()] = from;
	from = Card();
    }
    void MoveToAce(CardRange& from) {
	data[from.suit()] = from.Last();
	from = CardRange();
    }
    void MoveTo(Card& from) { MoveToAce(from); }
    void MoveTo(CardRange& from) { MoveToAce(from); }
};

class Kings : public Dest {
    Card data[NumSuits];
  public:
    Card& operator[] (int i) { return data[i]; }
    void MoveToKing(Card& from) {
	data[from.suit()] = from;
	from = Card();
    }
    void MoveToKing(CardRange& from) {
	data[from.suit()] = from.First();
	from = CardRange();
    }
    void MoveTo(Card& from) { MoveToKing(from); }
    void MoveTo(CardRange& from) { MoveToKing(from); }
};

class Tableaux {
  private:
    Hash hashVal;
    void CanonicalForm();
  public:
    Pile piles[NumPiles];
    Spaces spaces;
    Aces aces;
    Kings kings;
    Boolean Solve();
    Hash HashVal() { return hashVal; }
    Boolean WonP();
    void Print(ostream& os);
    void Dump();
    void Read(istream& is);
    Tableaux(Card *deck);
    Tableaux(char *st);
    Tableaux(istream& is) { Read(is); }
    Boolean CheckAces(Card &c) {
	return (((c.rank() == Ace)
		|| (c.rank() == aces[c.suit()].Next().rank()))
		? FillAces()
		: 0);
    }
    Boolean FillAces(); // move all cards to aces that can be
};

class Move {
  protected:
    Dest& dest;
  public:
    Move(Dest& d) : dest(d) { }
    virtual void DoIt() { Error("Move::DoIt()"); };
};

class MoveCard : public Move {
    Card& from;
    MoveCardToMemberFunction mcmf;
  public:
    MoveCard(Card& f, Dest& d, MoveCardToMemberFunction mf)
	: (d), from(f), mcmf(mf) { }
    void DoIt() { dest.MoveTo(from); }
//    void MoveCard::DoIt() { dest.*mcmf(from); }
};

class MoveCardRange : public Move {
    CardRange& from;
  public:
    MoveCardRange(CardRange& f, Dest& d) : (d), from(f) { }
    void DoIt() {dest.MoveTo(from);} 
};

ostream& operator<< (ostream& os, Tableaux &t) { t.Print(os); return os; }
istream& operator>> (istream& is, Tableaux &t) { t.Read(is); return is; }

void Tableaux::Dump() { Print(cout); }

Hash::Hash(Tableaux& t) {

    // there are 5 possible tops, and 13 possible values for the number of
    // cards in the top "group". These two values uniquely determine what's in
    // that pile. Each pile starts with five groups, that number never grows.
    // You can only move a group by either removing it (to a space or an ace)
    // or moving it to the end of another group (which merely lengthens that
    // group). So the number of groups in a pile is monotone non-increasing.
    // An implication of this is that the *suit* of each position in the pile
    // stays fixed, and so is derivable from the initial position (i.e.  adds
    // no information) thus we can ignore the suit in computing the hash. This
    // means that there are 66 possible hash values for each pile. 5*13
    // possible top+count values + empty.
    // 66^5 < 2^32 so we can store the hash for 10 piles in two 32 bit numbers
    // We also need to store information about the kings. I'm lazy so I just
    // store it in one long.

    // Note: Hash 0 would imply all empty piles, which will never happen!

    l3 = (((((t.kings[0].Value() << 8)
	     + t.kings[1].Value()) << 8)
	   + t.kings[2].Value()) << 8)
	+ t.kings[3].Value();

    l1 = 0;
    Pile *p;
    for (p=t.piles; p < t.piles+5; p++) {
	const int count = p->top().Count();
	if (count >= 6) {
	    const Suit s = p->top().suit();
	    const Rank r = p->top().First().rank();
	    for (CardRange* cr = &(p->top()); cr >= &((*p)[0]); cr--) {
		if (cr->suit() != s) continue;
		if (cr->First().rank() < r) {
		    l1 = l2 = l3 = -1;
		    return;
		}
	    }
	}
	l1 = l1*66 + (p->EmptyP() ? 0 : (p->topIndex()*13+count-1)+1);
    }

    l2 = 0;
    for (p = t.piles+5 ; p < t.piles+NumPiles; p++) {
	const int count = p->top().Count();
	if (count >= 6) {
	    const Suit s = p->top().suit();
	    const Rank r = p->top().First().rank();
	    for (CardRange* cr = &(p->top()); cr >= &((*p)[0]); cr--) {
		if (cr->suit() != s) continue;
		if (cr->First().rank() < r) {
		    l1 = l2 = l3 = -1;
		    return;
		}
	    }
	}
	l2 = l2*66 + (p->EmptyP() ? 0 : (p->topIndex()*13+count-1)+1);
    }

    if ((l1==0)&&(l2==0)&&(l3==0))
	Error("Tableaux hashes to NULL.");
}

Boolean Tableaux::WonP() {
    return ((aces[0].rank() == King)
	    && (aces[1].rank() == King)
	    && (aces[2].rank() == King)
	    && (aces[3].rank() == King));
}

Boolean Tableaux::FillAces() {
    int found;
    do {
	found = 0;
	int dests[NumSuits];

	int numFull = 0;
	for (Suit s = SuitFirst; s <= SuitLast; s++) {
	    if (aces[s].EmptyP()) dests[s] = Ace;
	    else {
		numFull += (aces[s].rank() == King);
		dests[s] = aces[s].rank()+1;
	    }
	}
	if (numFull == NumSuits) return 1;

	// check piles
	for (Pile* p = piles; p < piles+NumPiles; p++) {
	    if (p->EmptyP()) continue;
	    Card& first = p->top().First();
	    if (dests[first.suit()] == first.rank()) {
		aces.MoveToAce(p->pop());
		found = 1;
#ifdef VERBOSE
		cerr << "+";
#endif
	    }
	}
	
	// check spaces
	for (Card *c = &spaces[0]; c < &spaces[NumSpaces]; c++) {
	    if (c->EmptyP()) continue;
	    if (dests[c->suit()] == c->rank()) {
		aces.MoveToAce(*c);
		found = 1;
#ifdef VERBOSE
		cerr << "+";
#endif
	    }
	}
	
	// check kings
	for (Suit s1 = SuitFirst; s1 <= SuitLast; s1++) {
	    if (kings[s1].EmptyP()) continue;
	    if (dests[s1] == kings[s1].rank()) {
		Card tempKing = Card(s1, King);
		aces.MoveToAce(tempKing);
		kings[s1] = Card();
		found = 1;
#ifdef VERBOSE
		cerr << "!";
#endif
	    }
	}
    } while (found);
    return 0;
}

Boolean Tableaux::Solve() {

    hashVal = Hash(*this);

    if (hashTable.CheckAndAdd(HashVal())) return 0;

    Tableaux savedTab = *this;

#ifdef VERBOSE
    cerr << ".";
#endif
#ifdef DYNAMIC
    cout << "[;H" << *this;
#endif

    // pre-calculate useful bits of info - how many empty spaces are there
    // how many empty piles there are

    int empty_space_count = 0;
    Card *c;
    for (c = &spaces[0]; c < &spaces[NumSpaces]; c++)
	empty_space_count += c->EmptyP();

    int empty_pile_count = 0;
    Pile* p;
    for (p = piles; p < piles+NumPiles; p++) empty_pile_count += p->EmptyP();

    Suit s;
    for (s = SuitFirst; s <= SuitLast; s++)
	empty_pile_count -= !kings[s].EmptyP();

    // generate legal moves, check each one
    // all legal moves are either
    // 1) an entire top sequence onto the top of a pile (shortcut 3+2)
    // 2) a card from a space onto the top of a pile
    // 3) an entire top sequence onto a space or spaces
    // 4) an entire king based sequence onto an empty pile (shortcut 3+5+2)
    // 5) a king from a space onto an empty pile

    // another way of looking at it is that all moves are onto one of
    // tops of piles
    // empty piles (kings)
    // spaces

    int destTable[NumSuits*NumRanks];

    memset((char*)destTable,0,sizeof(destTable));

    // enumerate destination piles
    for (p = piles; p < piles+NumPiles; p++) {
	if (p->EmptyP()) continue;
	destTable[p->top().First().Prev().Value()] = (p-piles)+1;
    }

    // enumerate destination kings
    for (s = SuitFirst; s <= SuitLast; s++) {
	if (kings[s].EmptyP()) {
	    if (empty_pile_count) destTable[Card(s,King).Value()] = s+11;
	} else {
	    destTable[kings[s].Prev().Value()] = s+11;
	}
    }

    Move* movesMakingEmptyPiles[10];
    int movesMakingEmptyPilesCount = 0;

    Move* movesMakingUnmovableRange[10];
    int movesMakingUnMovableRange= 0;

    // look for moves from spaces
    for (Card *from = &spaces[0]; from < &spaces[NumSpaces]; from++) {
	if (from->EmptyP()) continue;
	const int dest = destTable[from->Value()];
	if (!dest) continue;
	if (dest < 11) piles[dest-1].MoveToPile(*from);
	else kings.MoveToKing(*from);
	if (Solve()) {
	    *this = savedTab;
#ifdef AUTO
	    LogSolution(*from, dest);
#endif
#ifdef INTERACTIVE
	    cout << "\n" << *from << "(S) -> ";
	    if (dest < 11) cout << piles[dest-1].top();
	    else cout << "(K)" << kings[dest-11];
#endif
	    return 1;
	} else {
	    *this = savedTab;
	    if (dest < 11) return 0; // It never costs to move something from
				     // a space to a pile (unless it's a king)
	}
    }

    // look for moves from piles
    for (p = piles; p < piles+NumPiles; p++) {
	if (p->EmptyP()) continue;
	CardRange& from = p->top();
	if (from.Count()-1 > empty_space_count) continue;
	int const dest = destTable[from.Last().Value()];
	if (!dest) continue;
	// legal move!
/*
	if (p->topIndex() == 0) {
	    if (dest < 11) {
		movesMakingEmptyPiles[movesMakingEmptyPilesCount++] =
		    new Move();
	    } else {
	    }
	} else {
*/
	    if (dest < 11) {
		piles[dest-1].MoveToPile(p->pop());
		// did we uncover a king?
		if ((p->topIndex() == 0) && (p->top().Last().rank() == King))
		    kings.MoveToKing(p->pop());
	    } else {
		kings.MoveToKing(p->pop());
	    }
/*
	}
*/
	if ((!p->EmptyP()) && CheckAces(p->top().First()) || Solve()) {
#ifdef AUTO
	    *this = savedTab;
	    LogSolution(from, dest);
#endif
#ifdef INTERACTIVE
	    *this = savedTab;
	    cout << "\n" << from << " -> ";
	    if (dest < 11) cout << piles[dest-1].top();
	    else cout << "(K)" << kings[dest-11];
#endif
	    return 1;
	}
	*this = savedTab;
    }

    // look for moves into spaces
    for (p = piles; p < piles+NumPiles; p++) {
	if (p->EmptyP()) continue;
	CardRange& tp = p->top();
	if (tp.Count() > empty_space_count) continue;
	spaces.MoveToSpace(p->pop());
	if (((!p->EmptyP()) && CheckAces(p->top().First())) || Solve()) {
#ifdef AUTO
	    *this = savedTab;
	    LogSolution(tp, -99);
#endif 
#ifdef INTERACTIVE
	    *this = savedTab;
	    cout << "\n" << tp << " -> space";
#endif
	    return 1;
	}
	*this = savedTab;
    }

    return 0;
};

void Tableaux::CanonicalForm() {
    // find all sequences and compress them
    for (int i = 0; i < NumPiles; i++) {
	Pile& p = piles[i];
	int k = 0;
	for (int j = 1; j < NumCardRangesInPile; j++)
	    if (p[j].NextP(p[k].Last())) p[k].Prepend(p[j].First());
	    else p.Set(++k, p[j]);

	for (k++; k < NumCardRangesInPile; k++) {
	    p.Set(k,CardRange());
	    p.pop();
	}
    }

/*
    for (Pile* p = piles; p < piles+NumPiles; p++) {
	CardRange *nextPos = &(*p)[0];
	for (CardRange *c = &(*p)[1]; c < &(*p)[NumCardRangesInPile]; c++) {
	    if (c->NextP(nextPos->Last())) nextPos->Prepend(*c);
	    else *(++nextPos) = *c;
	}
	for (nextPos++; nextPos < &(*p)[NumCardRangesInPile]; nextPos++)
	    *nextPos = CardRange();
    }
*/

    FillAces();
}

Tableaux::Tableaux(char* st) {
    static int deck[NumSuits][NumRanks];

    Card c;

    c = st; deck[c.suit()][c.rank()]++;
    spaces[1] = c;
    st += 2;
    c = st; deck[c.suit()][c.rank()]++;
    spaces[2] = c;
    st += 2;


    for (Pile* p = piles; p < piles+NumPiles; p++)
	for (CardRange* cr = &(*p)[0]; cr < &(*p)[NumCardRangesInPile]; cr++) {
	    c = st; deck[c.suit()][c.rank()]++;
	    *cr = c;
	    st += 2;
	}

    for (Suit s = SuitFirst; s <= SuitLast; s++)
	for (Rank r = RankFirst; r <= RankLast; r++)
	    if (deck[s][r] != 1) {
		cerr << "\n" << Card(s, r) << " count is " << deck[s][r];
		Error();
	    }

    CanonicalForm();

}

void Tableaux::Print(ostream& os) {
    os << "\n";
    os << aces[0]   << "    " << aces[1]   << "          "
       << spaces[0] << "    " << spaces[1] << "    "
       << spaces[2] << "    " << spaces[3] << "          "
       << aces[2]   << "    " << aces[3];
    os << "\n";
    for (int j = 0; j < NumCardRangesInPile; j++) {
	char *sep = "\n";
	for (int i = 0; i < NumPiles; i++) {
	    os << sep << piles[i][j];
	    sep = " ";
	}
    }
    os << "\n";
    os << kings[0] << " " << kings[1] << " " << kings[2] << " " << kings[3];
    os << "\n";
}

void Tableaux::Read(istream& is) {
    int won, lost, streak, longest_won, longest_lost, dummy;
    is >> won >> lost >> streak >> longest_won >> longest_lost >> dummy;
    for (int i = 0; i < NumCardRangesInPile; i++)
	for (int j = 0; j < NumPiles; j++) {
	    int s, r;
	    is >> s >> r;
	    piles[j].Set(i, CardRange(Card(Suit(s),Rank(r))));
	}
    int s, r;
    is >> s >> r;
    spaces[1] = Card(Suit(s),Rank(r));
    is >> s >> r;
    spaces[2] = Card(Suit(s),Rank(r));

    CanonicalForm();

}

Tableaux::Tableaux(Card *deck) {
    for (int i = 0; i < NumCardRangesInPile; i++)
	for (int j = 0; j < NumPiles; j++) {
	    piles[j].Set(i, CardRange(*(deck++)));
	}
    spaces[1] = *(deck++);
    spaces[2] = *(deck++);

    CanonicalForm();

}


#ifdef AUTO
int AutoPlay() {

    char savefile[500];

    sprintf(savefile, "%s/.seahavensave", getenv("HOME"));

    ifstream in(savefile);
    Tableaux t(in);

    solutionhead = NULL;
    int result = t.Solve();

    hashTable.Clear();
    return result;
}
#endif
