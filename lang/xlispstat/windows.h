typedef struct window_data {
  char *object;
  int idleOn, frontOnly;
  int mouse_x, mouse_y;
} *WindowData;

#define get_window_data(w)    GetWRefCon(w)
#define set_window_data(w, d) SetWRefCon(w, (long) d)
extern char *StCalloc();
