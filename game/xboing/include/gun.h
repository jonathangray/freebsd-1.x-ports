#ifndef _GUN_H_
#define _GUN_H_

#include "copyright.h"

/*
 *  Dependencies on other include files:
 */

#include <X11/Xlib.h>

/*
 *  Constants and macros:
 */

#define MAX_BULLETS         20

/*
 *  Type declarations:
 */

/*
 *  Function prototypes:
 */

#if NeedFunctionPrototypes
void InitialiseBullet(Display *display, Window window, Colormap colormap);
void FreeBullet(Display *display);
void SetNumberBullets(int num);
void IncNumberBullets(void);
void DecNumberBullets(void);
int GetNumberBullets(void);
void shootBullet(Display *display, Window window);
void DrawTheBullet(Display *display, Window window, int x, int y);
void EraseTheBullet(Display *display, Window window, int x, int y);
void HandleBulletMode(Display *display, Window window);
void ClearBullets(void);
void SetUnlimitedBullets(int state);
#else
void SetUnlimitedBullets();
void InitialiseBullet();
void FreeBullet();
void SetNumberBullets();
void IncNumberBullets();
void DecNumberBullets();
int GetNumberBullets();
void shootBullet();
void DrawTheBullet();
void EraseTheBullet();
void HandleBulletMode();
void ClearBullets();
#endif

#endif
