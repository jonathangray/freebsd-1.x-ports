static int file_size(string file);
static void log_file(string file, string str);
static string *get_dir(string file);
static int mkdir(string file);
static varargs string read_bytes(string file, int start, int size);
static varargs string read_file(string file, int first, int len);
static int rm(string file);
static int rmdir(string file);
static int rename(string from, string to);
static int restore_object(string file);
static void save_object(string file);
static int write_bytes(string file, int start, string str);
static int write_file(string file, string str);
static varargs int cat(string file, int first, int len);
static int tail(string file);
static varargs void ed(string file, string exit_func);
static int cindent(string file);
static int ls(string file);

# define INIT_FILE()
