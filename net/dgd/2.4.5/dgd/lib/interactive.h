static varargs object interactive(object obj);
static varargs string query_ip_number(object obj);
static varargs string query_ip_name(object obj);
static int query_idle(object obj);
static int exec(object obj, object from);
static void write(mixed str);
static void tell_object(object obj, string str);
static varargs void tell_room(mixed room, string str, mixed avoid);
static varargs void say(string str, mixed avoid);
static void shout(string str);
static varargs int input_to(string func, int flag);
static varargs object snoop(object obj);
static object query_snoop(object obj);
static object *users();

# define INIT_INTERACTIVE()
