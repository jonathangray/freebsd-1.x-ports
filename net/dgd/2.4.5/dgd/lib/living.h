private void remove_actions(object player, object obj);

static void enable_commands();
static void disable_commands();
static int living(object obj);
static varargs void add_action(string func, string verb, int flag);
static void add_verb(string verb);
static varargs int command(string cmd, object obj);
static void localcmd();

# define INIT_LIVING()	chunks = ({ }); actions = ([ ])
