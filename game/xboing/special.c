#include "include/copyright.h"

/*
 *  Include file dependencies:
 */

#include <stdlib.h>

#include "include/init.h"
#include "include/misc.h"
#include "include/stage.h"
#include "include/paddle.h"

#include "include/special.h"

/*
 *  Internal macro definitions:
 */

#define GAP			5

/*
 *  Internal type declarations:
 */

/*
 *  Internal variable declarations:
 */

int multiBall;
int stickyBat;
int fastGun;
int noWalls;
int Killer;
int x2Bonus;
int x4Bonus;

#if NeedFunctionPrototypes
void TurnSpecialsOff(Display *display)
#else
void TurnSpecialsOff(display)
	Display *display;
#endif
{
	/* Turn all specials off */

	ToggleFastGun(display, False);
	ToggleWallsOn(display, False);
	ToggleMultiBall(display, False);
	ToggleStickyBat(display, False);
	Togglex2Bonus(display, False);
	Togglex4Bonus(display, False);
	ToggleKiller(display, False);
}

#if NeedFunctionPrototypes
void Togglex2Bonus(Display *display, int state)
#else
void Togglex2Bonus(display, state)
	Display *display;
	int state;
#endif
{
	/* Set the x2Bonus flag to true or false */
	x2Bonus = state;
}

#if NeedFunctionPrototypes
void ToggleKiller(Display *display, int state)
#else
void ToggleKiller(display, state)
	Display *display;
	int state;
#endif
{
	/* Set the Killer flag to true or false */
	Killer = state;
}

#if NeedFunctionPrototypes
void Togglex4Bonus(Display *display, int state)
#else
void Togglex4Bonus(display, state)
	Display *display;
	int state;
#endif
{
	/* Set the x4Bonus flag to true or false */
	x4Bonus = state;
}

#if NeedFunctionPrototypes
void ToggleStickyBat(Display *display, int state)
#else
void ToggleStickyBat(display, state)
	Display *display;
	int state;
#endif
{
	/* Set the stickyBat flag to true or false */
	stickyBat = state;
}

#if NeedFunctionPrototypes
void ToggleMultiBall(Display *display, int state)
#else
void ToggleMultiBall(display, state)
	Display *display;
	int state;
#endif
{
	/* Set the multiBall flag to true or false */
	multiBall = state;
}

#if NeedFunctionPrototypes
void ToggleFastGun(Display *display, int state)
#else
void ToggleFastGun(display, state)
	Display *display;
	int state;
#endif
{
	/* Set the fastgun flag to true or false */
	fastGun = state;
}

#if NeedFunctionPrototypes
void ToggleWallsOn(Display *display, int state)
#else
void ToggleWallsOn(display, state)
	Display *display;
	int state;
#endif
{
	/* Set the noWalls flag to true or false */
	noWalls = state;

	if (noWalls == True)
	{
		/* Turn the wall colour to green */
		XSetWindowBorder(display, playWindow, green);
	}
	else
	{
		/* Turn the wall colour to red */
		XSetWindowBorder(display, playWindow, red);
	}
}

#if NeedFunctionPrototypes
void DrawSpecials(Display *display)
#else
void DrawSpecials(display)
	Display *display;
#endif
{
	int y = 3;
	int x = 5;
	char string[80];

	/* Clear the special window */
	/*XClearWindow(display, specialWindow);*/

	strcpy(string, "Reverse");
	if (reverseOn == True)
		DrawShadowText(display, specialWindow, copyFont, 
			string, x, y, yellow);
	else
		DrawShadowText(display, specialWindow, copyFont, 
			string, x, y, white);

	y += copyFont->ascent + GAP;

	strcpy(string, "Sticky");
	if (stickyBat == True)
		DrawShadowText(display, specialWindow, copyFont, 
			string, x, y, yellow);
	else
		DrawShadowText(display, specialWindow, copyFont, 
			string, x, y, white);

	x = 55; y = 3;

	strcpy(string, "MultiBall");
	if (multiBall == True)
		DrawShadowText(display, specialWindow, copyFont, 
			string, x, y, yellow);
	else
		DrawShadowText(display, specialWindow, copyFont, 
			string, x, y, white);

	y += copyFont->ascent + GAP;

	strcpy(string, "FastGun");
	if (fastGun == True)
		DrawShadowText(display, specialWindow, copyFont, 
			string, x, y, yellow);
	else
		DrawShadowText(display, specialWindow, copyFont, 
			string, x, y, white);

	x = 110; y = 3;

	strcpy(string, "NoWall");
	if (noWalls == True)
		DrawShadowText(display, specialWindow, copyFont, 
			string, x, y, yellow);
	else
		DrawShadowText(display, specialWindow, copyFont, 
			string, x, y, white);

	y += copyFont->ascent + GAP;

	strcpy(string, "Killer");
	if (Killer == True)
		DrawShadowText(display, specialWindow, copyFont, 
			string, x, y, yellow);
	else
		DrawShadowText(display, specialWindow, copyFont, 
			string, x, y, white);

	x = 155; y = 3;

	strcpy(string, "x2");
	if (x2Bonus == True)
		DrawShadowText(display, specialWindow, copyFont, 
			string, x, y, yellow);
	else
		DrawShadowText(display, specialWindow, copyFont, 
			string, x, y, white);

	y += copyFont->ascent + GAP;

	strcpy(string, "x4");
	if (x4Bonus == True)
		DrawShadowText(display, specialWindow, copyFont, 
			string, x, y, yellow);
	else
		DrawShadowText(display, specialWindow, copyFont, 
			string, x, y, white);
}


#if NeedFunctionPrototypes
void RandomDrawSpecials(Display *display)
#else
void RandomDrawSpecials(display)
	Display *display;
#endif
{
	int n;

	/* Randomly change the specials to flash on and off */
	n = rand() % 100;
	multiBall = n > 50 ? True : False;
	n = rand() % 100;
	stickyBat = n > 50 ? True : False;
	n = rand() % 100;
	Killer = n > 50 ? True : False;
	n = rand() % 100;
	x2Bonus = n > 50 ? True : False;
	n = rand() % 100;
	x4Bonus = n > 50 ? True : False;
	n = rand() % 100;
	fastGun = n > 50 ? True : False;
	n = rand() % 100;
	noWalls = n > 50 ? True : False;
	n = rand() % 100;
	reverseOn = n > 50 ? True : False;

	DrawSpecials(display);
}
