static varargs void call_out(string func, int delay, mixed args...);
static int remove_call_out(string func);
static int set_heart_beat(int flag);

# define INIT_CALL_OUT()	callout = allocate(1)
