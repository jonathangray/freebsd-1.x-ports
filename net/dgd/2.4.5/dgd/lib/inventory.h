private object *query_inventory(object obj);
private void remove_inv(object obj, object from);

static varargs object environment(object obj);
static void move_object(mixed obj, mixed dest);
static varargs object *all_inventory(object obj);
static object *deep_inventory(object obj);
static varargs object first_inventory(mixed obj);
static varargs object next_inventory(object obj);

# define INIT_INVENTORY()	inventory = ({ })
