#ifndef _SPECIAL_H_
#define _SPECIAL_H_

#include "copyright.h"

/*
 *  Dependencies on other include files:
 */

#include <X11/Xlib.h>

/*
 *  Constants and macros:
 */

#define REVERSE     1
#define STICKY      2

#define FLASH		500

/*
 *  Type declarations:
 */

/*
 *  Function prototypes:
 */

#if NeedFunctionPrototypes
void DrawSpecials(Display *display);
void ToggleFastGun(Display *display, int state);
void ToggleWallsOn(Display *display, int state);
void TurnSpecialsOff(Display *display);
void ToggleStickyBat(Display *display, int state);
void ToggleMultiBall(Display *display, int state);
void Togglex2Bonus(Display *display, int state);
void Togglex4Bonus(Display *display, int state);
void ToggleKiller(Display *display, int state);
void RandomDrawSpecials(Display *display);
#else
void RandomDrawSpecials();
void ToggleKiller();
void TurnSpecialsOff();
void DrawSpecials();
void ToggleFastGun();
void ToggleWallsOn();
void ToggleStickyBat();
void ToggleMultiBall();
void Togglex2Bonus();
void Togglex4Bonus();
#endif

extern int multiBall;
extern int stickyBat;
extern int fastGun;
extern int noWalls;
extern int Killer;
extern int x2Bonus;
extern int x4Bonus;

#endif
