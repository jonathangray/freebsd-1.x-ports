/*
**
**	X11 Jewel By David Cooper and Jose Guterman 05/92
**
*/

/* default colors */
#define PIECE1_C "red"
#define PIECE2_C "green"
#define PIECE3_C "orange"
#define PIECE4_C "blue"
#define PIECE5_C "cyan"
#define PIECE6_C "yellow"
#define WILD_C "white"
#define FLASH_C "gray"
#define BACKGND_C "gray"


/* The numbers for PIECE1 to PIECE6 should be consecutive */
#define PIECE1 0
#define PIECE2 1
#define PIECE3 2
#define PIECE4 3
#define PIECE5 4
#define PIECE6 5

#	define NUM_REAL_PIECES 6

#define WILD_PIECE 6

#define BACKGND1 7
#define BACKGND2 8
#define BACKGND3 9
#define BACKGND4 10

#	define NUM_BACKGND	4

#define FLASH1 11
#define FLASH2 12
#define FLASH3 13
#define FLASH4 14

#	define NUM_FLASH	4
#define NUM_PIECES 15


#define TEST_PIECE(x)  ( ((x)==PIECE1) || \
						 ((x)==PIECE2) || \
						 ((x)==PIECE3) || \
						 ((x)==PIECE4) || \
						 ((x)==PIECE5) || \
						 ((x)==PIECE6) )
	

#define AVG_BLOCKS_BETWEEN_JEWELS 27

struct rem_piece {
	int x,y;
	int piece;
	};

extern void Redraw_Board();
extern void Init_Logic();
extern void Move_Right();
extern void Move_Left();
extern void Rotate();
extern void Drop();
extern void Logic_Timeout();

