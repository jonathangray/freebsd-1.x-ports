#ifndef MODULE_H
#define MODULE_H

#ifdef MODULES
extern int npipes;
extern int *readPipes;
extern int *writePipes;
#endif

#define START_FLAG 0xffffffff

#define M_TOGGLE_PAGING     1
#define M_NEW_PAGE          2
#define M_NEW_DESK          3
#define M_ADD_WINDOW        4
#define M_RAISE_WINDOW      5
#define M_LOWER_WINDOW      6
#define M_CONFIGURE_WINDOW  7
#define M_FOCUS_CHANGE      8
#define M_DESTROY_WINDOW    9
#define M_ICONIFY          10
#define M_DEICONIFY        11
#define M_WINDOW_NAME      12
#define M_ICON_NAME        13
#define M_RES_CLASS        14
#define M_RES_NAME         15
#define M_END_WINDOWLIST   16
#define M_ICON_LOCATION    17
#define M_MAP              18
#define MAX_MESSAGES       18

#define HEADER_SIZE         3
#define MAX_PACKET_SIZE    25
#define MAX_BODY_SIZE      (MAX_PACKET_SIZE - HEADER_SIZE)

#endif /* MODULE_H */



