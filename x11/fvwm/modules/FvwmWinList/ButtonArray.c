/* FvwmWinList Module for Fvwm. 
 *
 *  Copyright 1994,  Mike Finger (mfinger@mermaid.micro.umn.edu or
 *                               Mike_Finger@atk.com)
 *
 * The functions in this source file that are the original work of Mike Finger.
 * 
 * No guarantees or warantees or anything are provided or implied in any way
 * whatsoever. Use this program at your own risk. Permission to use this
 * program for any purpose is given, as long as the copyright is kept intact.
 *
 *  Things to do:  Convert to C++  (In Progress)
 */

#include <stdlib.h>
#include <stdio.h>
#include <X11/Xlib.h>
#include "ButtonArray.h"
#include "Mallocs.h"

#ifndef min
#define min(a,b) (((a)<(b)) ? (a) : (b))
#define max(a,b) (((a)>(b)) ? (a) : (b))
#endif

extern XFontStruct *ButtonFont;
extern Display *dpy;
extern Window win;
extern GC shadow,hilite,graph;

/******************************************************************************
  InitArray - Initialize the arrary of buttons
******************************************************************************/
int InitArray(ButtonArray *array,int x,int y,int w,int h)
{
  array->count=0;
  array->head=array->tail=NULL;
  array->x=x;
  array->y=y;
  array->w=w;
  array->h=h;
}

/******************************************************************************
  UpdateArray - Update the array specifics.  x,y, width, height
******************************************************************************/
void UpdateArray(ButtonArray *array,int x,int y,int w, int h)
{
Button *temp;
  if (x!=-1) array->x=x;
  if (y!=-1) array->y=y;
  if (w!=-1) array->w=w;
  if (h!=-1) array->h=h;
  for(temp=array->head;temp!=NULL;temp=temp->next) temp->needsupdate=1;
}

/******************************************************************************
  AddButton - Allocate space for and add the button to the bottom
******************************************************************************/
int AddButton(ButtonArray *array, char *title, int up)
{
Button *new;
  new=(Button *)safemalloc(sizeof(Button));
  new->title=safemalloc(strlen(title)+1);
  strcpy(new->title,title);
  new->up=up;
  new->tw=XTextWidth(ButtonFont,title,strlen(title));
  new->next=NULL;
  new->needsupdate=1;

  if (array->head==NULL) array->head=array->tail=new;
  else {
    array->tail->next=new;
    array->tail=new;
  }
  array->count++;
  return (array->count-1);
}

/******************************************************************************
  UpdateButton - Change the name/stae of a button
******************************************************************************/
int UpdateButton(ButtonArray *array, int butnum, char *title, int up)
{
Button *temp;
  temp=find_n(array,butnum);
  if (temp!=NULL) {
    if (title!=NULL) {
      temp->title=(char *)realloc(temp->title,strlen(title)+1);
      strcpy(temp->title,title);
      temp->tw=XTextWidth(ButtonFont,title,strlen(title));
    }
    if (up!=-1) temp->up=up;
  } else return -1;
  temp->needsupdate=1;
  return 1;
}

/******************************************************************************
  RemoveButton - Delete a button from the list
******************************************************************************/
void RemoveButton(ButtonArray *array, int butnum)
{
Button *temp,*temp2;
  if (butnum==0) {
    temp2=array->head;
    temp=array->head=array->head->next;
  } else {
    temp=find_n(array,butnum-1);
    if (temp==NULL) return;
    temp2=temp->next;
    temp->next=temp2->next;
  }

  if (array->tail==temp2) array->tail=temp;

  FreeButton(temp2);

  if (temp!=array->head) temp=temp->next;
  for(temp;temp!=NULL;temp=temp->next) temp->needsupdate=1;
}

/******************************************************************************
  find_n - Find the nth button in the list (Use internally)
******************************************************************************/
Button *find_n(ButtonArray *array, int n)
{
Button *temp; 
int i;
  temp=array->head;
  for(i=0;i<n && temp!=NULL;i++,temp=temp->next);
  return temp;
}

/******************************************************************************
  FreeButton - Free space allocated to a button
******************************************************************************/
void FreeButton(Button *ptr)
{
  if (ptr != NULL) {
    if (ptr->title!=NULL) free(ptr->title);
    free(ptr);
  }
}

/******************************************************************************
  FreeAllButtons - Free the whole array of buttons
******************************************************************************/
void FreeAllButtons(ButtonArray *array)
{
Button *temp,*temp2;
  for(temp=array->head;temp!=NULL;) {
    temp2=temp;
    temp=temp->next;
    FreeButton(temp2);
  }
}

/******************************************************************************
  DoButton - Draw the specified button.  (Used internally)
******************************************************************************/
void DoButton(Button *ptr, int x, int y, int w, int h)
{
int up,Fontheight,newx;
  up=ptr->up;

  Fontheight=ButtonFont->ascent+ButtonFont->descent;

  XClearArea(dpy,win,x,y,w,h,False);
  XDrawLine(dpy,win,(up) ? hilite : shadow,x,y,x+w-1,y);
  XDrawLine(dpy,win,(up) ? hilite : shadow,x,y+1,x+w-2,y+1);

  XDrawLine(dpy,win,(up) ? hilite : shadow,x,y,x,y+h-1);
  XDrawLine(dpy,win,(up) ? hilite : shadow,x+1,y,x+1,y+h-2);
  
  XDrawLine(dpy,win,(up) ? shadow : hilite,x,y+h,x+w,y+h);
  XDrawLine(dpy,win,(up) ? shadow : hilite,x+1,y+h-1,x+w,y+h-1);

  XDrawLine(dpy,win,(up) ? shadow : hilite,x+w,y+h,x+w,y);
  XDrawLine(dpy,win,(up) ? shadow : hilite,x+w-1,y+h,x+w-1,y+1);
  newx=(w-ptr->tw)/2;
  XDrawString(dpy,win,graph,x+newx,y+3+ButtonFont->ascent,ptr->title,strlen(ptr->title));
  ptr->needsupdate=0;
}

/******************************************************************************
  DrawButtonArray - Draw the whole array (all=1), or only those that need.
******************************************************************************/
void DrawButtonArray(ButtonArray *array,int all)
{
Button *temp;
int i;
  for(temp=array->head,i=0;temp!=NULL;temp=temp->next,i++) 
    if (temp->needsupdate || all) {
      DoButton(temp,array->x,array->y+(i*(array->h+1)),array->w,array->h);
    }
}

/******************************************************************************
  SwitchButton - Alternate the state of a button
******************************************************************************/
void SwitchButton(ButtonArray *array,int butnum)
{
Button *temp;
  temp=find_n(array,butnum);
  temp->up=!temp->up;
  temp->needsupdate=1;
  DrawButtonArray(array,0);
}

/******************************************************************************
  WhichButton - Based on x,y which button was pressed
******************************************************************************/
int WhichButton(ButtonArray *array,int x, int y)
{
int num;
  num=y/(array->h+1);
  if (x<array->x || x>array->x+array->w || num<0 || num>array->count-1) num=-1;
  return(num);
}

/******************************************************************************
  ButtonName - Return the name of the button
******************************************************************************/
char *ButtonName(ButtonArray *array, int butnum)
{
Button *temp;
  temp=find_n(array,butnum);
  return temp->title;
}

/******************************************************************************
  PrintButtons - Print the array of button names to the console. (Debugging)
******************************************************************************/
void PrintButtons(ButtonArray *array)
{
Button *temp;
  ConsoleMessage("List of Buttons:\n");
  for(temp=array->head;temp!=NULL;temp=temp->next)
    ConsoleMessage("   %s is %s\n",temp->title,(temp->up) ? "Up":"Down");
}

/******************************************************************************
  ButtonArrayMaxWidth - Calculate the width needed for the widest title
******************************************************************************/
int ButtonArrayMaxWidth(ButtonArray *array)
{
Button *temp;
int x=0;
  for(temp=array->head;temp!=NULL;temp=temp->next)
    x=max(temp->tw,x);
  return x;
}
