/*
**
**	X11 Jewel By David Cooper and Jose Guterman 05/92
**
*/

#include <stdio.h>
#include <stdlib.h>

#include "general.h"
#include "logic.h"
#include "panel.h"
#include "game.h"

int board[NUM_COLS][NUM_ROWS];
int curr_block[BLOCK_SIZE];
int next_block[BLOCK_SIZE];
int background = BACKGND1;
int curr_block_x, curr_block_y;
BOOL moving_block = FALSE;
struct rem_piece p_remov[NUM_COLS*NUM_ROWS];
int index_rem;

void Redraw_Board()
	{
	int i, j;

	for (i=0; i<NUM_COLS; i++)
		for (j=0; j<NUM_ROWS; j++)
			Draw_Piece(board[i][j], i, j);

	for(i=0; i<BLOCK_SIZE; i++)
		{
		Draw_Piece(next_block[i], -1, i);
		}
	}

void Get_Random_Block(block)
int *block;
	{
	int i;

	if ((random() % AVG_BLOCKS_BETWEEN_JEWELS) == 0) /* time for a jewel? */
		{
		for (i=0; i<BLOCK_SIZE; i++)
			{
			block[i] = WILD_PIECE;
			}
		}
	else
		{
		for (i=0; i<BLOCK_SIZE; i++)
			{
			block[i] = (random() % NUM_REAL_PIECES);
			}
		}
	}

int Get_New_Background(old_background)
int old_background;
	{
	int bgnd;

	/* Random Background */
/*
	while ((bgnd = ((random() % NUM_BACKGND) + BACKGND1)) == old_background)
		{}
*/

	/* Rotate Backgrounds */
	bgnd = old_background+1;
	if (bgnd >= BACKGND1 + NUM_BACKGND)
		{
		bgnd = BACKGND1;
		}
	
	return(bgnd);
	}

void Clear_Board()
	{
	int i, j;

	for (i=0; i<NUM_COLS; i++)
		for (j=0; j<NUM_ROWS; j++)
			{
			board[i][j]=background;
			}
	}

Update_Board(piece, x, y)
int piece, x, y;
	{
	board[x][y] = piece;
	Draw_Piece(piece, x, y);
	}

void Init_Logic()
	{
	/* Initialize the board to be empty */
	background = Get_New_Background(BACKGND1);
	Clear_Board();

	/* Prepare for the firs block */
	Get_Random_Block(next_block);
	moving_block = FALSE;
	curr_block_x = NUM_COLS/2;
	curr_block_y = 0;
	}

void Clear_Remove_List()
	{
	index_rem = 0;
	}

void Add_To_Remove_List(x, y, piece)
int piece, x, y;
	{
	p_remov[index_rem].x = x;
	p_remov[index_rem].y = y;
	p_remov[index_rem].piece = piece;
	index_rem++;
	}

void Handle_Horiz_Line(i, j, in_line)
int i, j, in_line;
	{
	int k;

	for (k=0; k<in_line; k++)
		{
		Add_To_Remove_List(j-k-1, i, board[j-1][i]);
		}
	}

void Handle_Vert_Line(i, j, in_line)
int i, j, in_line;
	{
	int k;

	for (k=0; k<in_line; k++)
		{
		Add_To_Remove_List(i, j-k-1, board[i][j-1]);
		}
	}


void Handle_DiagR_Line(i, j, in_line)
int i, j, in_line;
	{
	int k;

	for (k=0; k<in_line; k++)
		{
		Add_To_Remove_List(j-k-1, i-k-1, board[j-k-1][i-k-1]);
		}
	}


void Handle_DiagL_Line(i, j, in_line)
int i, j, in_line;
	{
	int k;

	for (k=0; k<in_line; k++)
		{
		Add_To_Remove_List(j+k+1, i-k-1, board[j+k+1][i-k-1]);
		}
	}

int Check_Board()
	{
	int add_score;
	int i, j, k, l;
	int in_line;
	int piece_checked;

	add_score = 0;
	Clear_Remove_List();

	/* Check Horizontal */
	for (i=0; i<NUM_ROWS; i++)
		{
		in_line = 1;
		piece_checked = background;
		for (j=0; j<NUM_COLS; j++)
			{
			if (TEST_PIECE(board[j][i]) && (board[j][i] == piece_checked))
				{
				in_line++;
				}
			else
				{
				piece_checked = board[j][i];
				if (in_line > 2)
					{
					add_score = add_score + POINTS(in_line);
					Handle_Horiz_Line(i,j,in_line);
					}
				in_line = 1;
				}
			}

		if (in_line > 2)
			{
			add_score = add_score + POINTS(in_line);
			Handle_Horiz_Line(i,j,in_line);
			}
		}

	/* Check Vertical */
	for (i=0; i<NUM_COLS; i++)
		{
		in_line=1;
		piece_checked=background;
		for (j=0; j<NUM_ROWS; j++)
			{
			if (TEST_PIECE(board[i][j]) && (board[i][j] == piece_checked))
				{
				in_line++;
				}
			else
				{
				piece_checked = board[i][j];
				if (in_line > 2)
					{
					add_score = add_score + POINTS(in_line);
					Handle_Vert_Line(i,j,in_line);
					}
				in_line = 1;
				}
			}

		if (in_line > 2)
			{
			add_score = add_score + POINTS(in_line);
			Handle_Vert_Line(i,j,in_line);
			}
		}

	/* Check Diagonal Right */
	k=NUM_COLS-2;
	l=0;
	while (l<(NUM_ROWS-2))
		{
		i=l;
		j=k;
		in_line=1;
		piece_checked=background;
		while((i<NUM_ROWS) && (j<NUM_COLS))
			{
			if (TEST_PIECE(board[j][i]) && (board[j][i] == piece_checked))
				{
				in_line++;
				}
			else
				{
				piece_checked = board[j][i];
				if (in_line > 2)
					{
					add_score = add_score + POINTS(in_line);
					Handle_DiagR_Line(i,j,in_line);
					}
				in_line = 1;
				}
			i++;
			j++;
			}

		if (in_line > 2)
			{
			add_score = add_score + POINTS(in_line);
			Handle_DiagR_Line(i,j,in_line);
			}

		if (k>0)
			{
			k--;
			}
		else
			{
			l++;
			}
		}

	/* Check Diagonal Left */
	k=3;
	l=0;
	while (l<(NUM_ROWS-2))
		{
		i=l;
		j=k;
		in_line=1;
		piece_checked=background;
		while((i<NUM_ROWS) && (j>=0))
			{
			if (TEST_PIECE(board[j][i]) && (board[j][i] == piece_checked))
				{
				in_line++;
				}
			else
				{
				piece_checked = board[j][i];
				if (in_line > 2)
					{
					add_score = add_score + POINTS(in_line);
					Handle_DiagL_Line(i,j,in_line);
					}
				in_line = 1;
				}
			i++;
			j--;
			}

		if (in_line > 2)
			{
			add_score = add_score + POINTS(in_line);
			Handle_DiagL_Line(i,j,in_line);
			}

		if (k<(NUM_COLS-1))
			{
			k++;
			}
		else
			{
			l++;
			}
		}

	return(add_score);
	}

int Remove_Flash()
	{
	int index;

	for (index=0; index<index_rem; index++)
		{
		if (board[p_remov[index].x][p_remov[index].y] == FLASH1)
			{
			p_remov[index].x = p_remov[index_rem-1].x;
			p_remov[index].y = p_remov[index_rem-1].y;
			p_remov[index].piece = p_remov[index_rem-1].piece;
			index_rem--;
			index--;
			}
		else
			{
			board[p_remov[index].x][p_remov[index].y] = FLASH1;
			}
		}

	Flash_Pieces(p_remov, index_rem, background);
	}

void Drop_Down_After_Flash()
	{
	int i, j, k;

	for (i=0; i<NUM_COLS; i++)
		for (j=0; j<NUM_ROWS; j++)
			{
			if (board[i][j] == FLASH1)
				{
				for (k=j-1; k>=0; k--)
					{
					Update_Board(board[i][k], i, k+1);
					}
				Update_Board(background, i, 0);
				}
			}
	}

void Redraw_Background()
	{
	int i, j;

	for (i=0; i<NUM_COLS; i++)
		for (j=0; j<NUM_ROWS; j++)
			if (!TEST_PIECE(board[i][j])) 
				{
				Update_Board(background, i, j);
				}
	}

void Update_Rest(num_pieces)
int num_pieces;
	{
	if (Dec_Rest(num_pieces))
		{
		background = Get_New_Background(background);
		Redraw_Background();
		}
	}

void Process_Block(special, piece)
BOOL special;
int piece;
	{
	int i, j;
	int add_score;
	
	if (special)
		{
		Clear_Remove_List();
		for (i=0; i<NUM_COLS; i++)
			for (j=0; j<NUM_ROWS; j++)
				if ((board[i][j] == WILD_PIECE) ||
					((board[i][j] == piece) && TEST_PIECE(board[i][j])))
					{
					Add_To_Remove_List(i, j, board[i][j]);
					}

		Remove_Flash();
		Drop_Down_After_Flash();
		Add_Score(JEWELSCORE, 1);
		Update_Rest(index_rem);
		}

	i = 1;
	while ((add_score = Check_Board()) > 0)
		{
		Remove_Flash();
		Drop_Down_After_Flash();
		Add_Score(add_score, i);
		Update_Rest(index_rem);
		i++;
		}

	if (TEST_PIECE(board[NUM_COLS/2][BLOCK_SIZE-1]))
		{
		Melt_Down();
		Clear_Board();
		Redraw_Board();
		Dec_Lives();
		}
	}

void Move_Right()
	{
	int i;

	if (!moving_block) 
		{
		return;
		}

	if (curr_block_x < (NUM_COLS-1))
		{
		for (i=0; i<BLOCK_SIZE; i++)
			{
			if (TEST_PIECE(board[curr_block_x+1][curr_block_y+i]))
				{
				return;
				}
			}

		curr_block_x++;
		for (i=0; i<BLOCK_SIZE; i++)
			{
			Update_Board(curr_block[i], curr_block_x, curr_block_y+i);
			Update_Board(background, curr_block_x-1, curr_block_y+i);
			}

		Key_Bell();
		}
	}

void Move_Left()
	{
	int i;
	if (!moving_block)
		{
		return;
		}

	if (curr_block_x > 0)
		{
		for (i=0; i<BLOCK_SIZE; i++)
			{
			if (TEST_PIECE(board[curr_block_x-1][curr_block_y+i]))
				{
				return;
				}
			}

		curr_block_x--;
		for (i=0; i<BLOCK_SIZE; i++)
			{
			Update_Board(curr_block[i], curr_block_x, curr_block_y+i);
			Update_Board(background, curr_block_x+1, curr_block_y+i);
			}

		Key_Bell();
		}
	}

void Move_Down()
	{
	int i;

	if ((curr_block_y == (NUM_ROWS - BLOCK_SIZE)) ||
		(TEST_PIECE(board[curr_block_x][curr_block_y+BLOCK_SIZE])))
		{
		moving_block = FALSE;
		if (curr_block[0] == WILD_PIECE)
			{
			if (curr_block_y == NUM_ROWS-BLOCK_SIZE)
				{
				Process_Block(TRUE, background);
				}
			else
				{
				Process_Block(TRUE,
					board[curr_block_x][curr_block_y+BLOCK_SIZE]);
				}
			}
		else
			{
			Process_Block(FALSE, NULL);
			}
		}
	else
		{
		for (i=0; i<BLOCK_SIZE; i++)
			{
			Update_Board(curr_block[i], curr_block_x, curr_block_y+i+1);
			}
		Update_Board(background, curr_block_x, curr_block_y);
		curr_block_y++;
		}
	}

void Drop()
	{
	int cycles=(-1);
	int i,j;
	if (!moving_block)
		{
		return;
		}

	Key_Bell();
	/* we have to find the number of steps it falls here
	   so we can show the points first */
	i=curr_block_x;
	j=curr_block_y+BLOCK_SIZE;

	while (j<NUM_ROWS)
		{
		if (TEST_PIECE(board[i][j]))
			{
			break;
			}
		cycles++;
		j++;
		}
	if (cycles>0)
		{
		Add_Raw_Score(DROP_POINTS, cycles);
		}

	while(moving_block)
		{
		Move_Down();
		}
	}

void Rotate()
	{
	int i;
	int tmp_block=curr_block[BLOCK_SIZE-1];
	if (!moving_block) return;
	for (i=BLOCK_SIZE-1; i>0; i--)
		{
		curr_block[i]=curr_block[i-1];
		Update_Board(curr_block[i], curr_block_x, curr_block_y+i);
		}
	curr_block[0]=tmp_block;
	Update_Board(curr_block[0], curr_block_x, curr_block_y);
	Key_Bell();
	}

void Logic_Timeout()
	{
	int i;

	if (!moving_block)
		{
		curr_block_x = NUM_COLS/2;
		curr_block_y = 0;

		for (i=0; i<BLOCK_SIZE; i++)
			{
			curr_block[i] = next_block[i];
			Update_Board(curr_block[i], curr_block_x, curr_block_y+i);
			}

		Get_Random_Block(next_block);
		for(i=0; i<BLOCK_SIZE; i++)
			{
			Draw_Piece(next_block[i], -1, i);
			}
		moving_block = TRUE;
		}
	else
		{
		Move_Down();
		}
	}
