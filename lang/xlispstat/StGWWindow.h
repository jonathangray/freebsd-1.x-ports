#define nil 0L

typedef int ColorCode;

typedef enum {
  MouseClick = 0,
  MouseMove = 1
} MouseEventType;

typedef enum {
  NoModifiers = 0,
  ExtendModifier = 1,
  OptionModifier = 2,
  OptionExtendModifier = 3
} MouseClickModifier;

typedef enum {
  pointInvisible,
  pointNormal,
  pointHilited,
  pointSelected
} PointState;

typedef enum {
  selecting,
  brushing,
  usermode
} MouseMode;

extern char *StCalloc();
extern long StGWGetObject();
extern char *StGWObWinInfo();
extern ColorCode StGWDrawColor();
extern ColorCode StGWBackColor();
extern long StGWGetRefCon();
extern long StGWGetColRefCon();
extern long StGWGetCursRefCon();
extern long StGWGetSymRefCon();

extern long IViewWindowGetObject();
extern long IViewGetRefCon();
extern char *IViewWindowWinInfo();

#define ARROW_CURSOR      0
#define WATCH_CURSOR      1
#define CROSS_CURSOR      2
#define BRUSH_CURSOR      3
#define HAND_CURSOR       4
#define FINGER_CURSOR     5
#define HOUR_GLASS_CURSOR 6
#define TRASH_BAG_CURSOR  7
#define TRASH_CAN_CURSOR  8
