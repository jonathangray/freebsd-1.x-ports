
/*
 * Art Mulder.  arc@cs.ualberta.ca
 *
 * A simple curses program to see if we can identify function keys.  
 *
 * USAGE: f1
 *   Then try pressing function keys.  The program will identify
 *   which key you have pressed, IF IT CAN.  Enter an 'x' to exit
 *   the program.
 */

#include<curses.h>

main()
{
  int c;

/*
 * Initialize Curses
 */
  initscr();
  noecho();
  cbreak();	/* start cbreak mode on terminal */
  nonl();	/* stop mapping return to newline */
  scrollok(stdscr,TRUE);
  idlok(stdscr,TRUE);

  keypad(stdscr, TRUE);
  notimeout(stdscr,TRUE);	/* for slower systems (ie: Sun3) */
  refresh();

/* 
 * begin input/display loop
 */
  move (0,0);
  printw("Enter Function Key, 'x' to exit.\n");

  while ( (c = getch()) != 'x' ) {		/* x = exit */
    clrtoeol();
    printw("%o: ",c);
    switch(c) {
    case KEY_F(1) : printw("F1\n"); break;	/* Function keys */
    case KEY_F(2) : printw("F2\n"); break;
    case KEY_F(3) : printw("F3\n"); break;
    case KEY_F(4) : printw("F4\n"); break;
    case KEY_F(5) : printw("F5\n"); break;
    case KEY_F(6) : printw("F6\n"); break;
    case KEY_F(7) : printw("F7\n"); break;
    case KEY_F(8) : printw("F8\n"); break;
    case KEY_F(9) : printw("F9\n"); break;
    case KEY_F(10) : printw("F10\n"); break;
    case KEY_F(11) : printw("F11\n"); break;
    case KEY_F(12) : printw("F12\n"); break;

    case KEY_LEFT:  printw("Left Arrow\n"); break;	/* Keypad keys */
    case KEY_RIGHT: printw("Right Arrow\n"); break;
    case KEY_UP:    printw("Up Arrow\n"); break;
    case KEY_DOWN:  printw("Down Arrow\n"); break;
    case KEY_HOME:  printw("HOME\n"); break;
    case KEY_END:   printw("END\n"); break;
    case KEY_PPAGE: printw("PgUp\n"); break;
    case KEY_NPAGE: printw("PgDn\n"); break;
    case KEY_IC: printw("Insert\n"); break;
    case KEY_ENTER: printw("Enter\n"); break;
    case KEY_SLEFT: printw("<shift>LEFT\n"); break;
    case KEY_SRIGHT: printw("<shift>RIGHT\n"); break;
    case KEY_SHOME: printw("<shift>HOME\n"); break;
    case KEY_SEND: printw("<shift>END\n"); break;

    default:
      printw("Unkown input.\n");
    }

    refresh();
  }

/*
 * Move cursor to bottom left corner of screen and end curses.
 */
  move((LINES-1),0);
  refresh();
  endwin();
}
