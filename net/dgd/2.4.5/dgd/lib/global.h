private void init_global();
private void set_verb(string verb);
private void show_wiz_list();
private void del_object();
private void add_heart_beat();
private void del_heart_beat();
private void add_call_out();
private void del_call_out();
private mixed relay(string func, mixed arg, closure expr);

static void set_this_player(object player);
static varargs object this_player(int flag);
static void set_living_name(string name);
static object find_player(string name);
static object find_living(string name);
static string query_verb();
static varargs closure lambda(mixed func, mixed *def);
static varargs mixed apply(mixed args...);

# define INIT_GLOBAL()	init_global()
