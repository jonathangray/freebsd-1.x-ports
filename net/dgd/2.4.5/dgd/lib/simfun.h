static varargs mixed *
filter_objects(mixed *arr, string func, object obj, mixed arg);
static varargs string file_name(object obj);
static varargs string function_exists(string func, object obj);
static int intp(mixed value);
static int floatp(mixed value);
static int stringp(mixed value);
static int objectp(mixed value);
static int arrayp(mixed value);
static int mappingp(mixed value);
static int pointerp(mixed value);
static int closurep(mixed value);
static varargs void add_worth(int worth, object obj);
static varargs void wizlist(string name);
static int transfer(mixed obstr, mixed tostr);
static varargs object present(mixed item, object obj);
static void destruct(object obj);
static string query_host_name();
static string query_load_average();
static void shutdown();
static string version();
static varargs string create_wizard(string wizard, string domain);
static varargs string crypt(string passwd, string salt);
static void notify_fail(string mesg);

# define INIT_SIMFUN()
