private void badarg(string func, int arg);

static string capitalize(string str);
static string lower_case(string str);
static string set_bit(string str, int bit);
static string clear_bit(string str, int bit);
static int test_bit(string str, int bit);
static int member_array(mixed elt, mixed *arr);
static varargs mixed *
filter_array(mixed *arr, mixed func, mixed obj, mixed arg);
static varargs mixed *map_array(mixed *arr, mixed func, mixed obj, mixed arg);
static varargs string extract(string str, int first, int last);
static mixed *slice_array(mixed *arr, int first, int last);
static varargs mixed *sort_array(mixed *arr, mixed func, mixed obj);
static varargs mixed *unique_array(mixed *arr, string func, mixed exclude);

# define INIT_TOOL()
# define ARGCHECK(t, f, a)	if (!(t)) badarg(#f, a)
