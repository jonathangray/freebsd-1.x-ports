# include <type.h>

/*
 * NAME:	filter_objects()
 * DESCRIPTION:	the same as filter_array()
 */
static varargs mixed *
filter_objects(mixed *arr, string func, object obj, mixed arg)
{
    return filter_array(arr, func, obj, arg);
}

/*
 * NAME:	file_name()
 * DESCRIPTION:	based on object_name()
 */
static varargs string file_name(object obj)
{
    string str;

    if (obj == 0) {
	obj = this_object();
    }
    str = object_name(obj);
    return str[1 ..];
}

/*
 * NAME:	function_exists()
 * DESCRIPTION:	based on function_object()
 */
static varargs string function_exists(string func, object obj)
{
    string str;

    ARGCHECK(func, function_exists, 1);
    if (obj == 0) {
	obj = this_object();
    }
    ARGCHECK(obj, function_exists, 2);

    str = function_object(func, obj);
    if (str != 0) {
	str = str[1 ..];
    }
    return str;
}

/*
 * NAME:	intp()
 * DESCRIPTION:	return 1 if the argument is an integer
 */
static int intp(mixed value)
{
    return typeof(value) == T_INT;
}

/*
 * NAME:	floatp()
 * DESCRIPTION:	return 1 if the argument is a float
 */
static int floatp(mixed value)
{
    return typeof(value) == T_FLOAT;
}

/*
 * NAME:	stringp()
 * DESCRIPTION:	return 1 if the argument is a string
 */
static int stringp(mixed value)
{
    return typeof(value) == T_STRING;
}

/*
 * NAME:	objectp()
 * DESCRIPTION:	return 1 if the argument is an object
 */
static int objectp(mixed value)
{
    return typeof(value) == T_OBJECT;
}

/*
 * NAME:	arrayp()
 * DESCRIPTION:	return 1 if the argument is an array
 */
static int arrayp(mixed value)
{
    return typeof(value) == T_ARRAY;
}

/*
 * NAME:	mappingp()
 * DESCRIPTION:	return 1 if the argument is a mapping
 */
static int mappingp(mixed value)
{
    return typeof(value) == T_MAPPING;
}

/*
 * NAME:	pointerp()
 * DESCRIPTION:	return 1 if the argument is an array and not a closure
 */
static int pointerp(mixed value)
{
    return (typeof(value) == T_ARRAY &&
	    (sizeof(value) == 0 || value[0] != LAMBDA));
}

/*
 * NAME:	closurep()
 * DESCRIPTION:	return 1 if the argument is a closure, 0 otherwise
 */
static int closurep(mixed value)
{
    return (typeof(value) == T_ARRAY && sizeof(value) != 0 &&
	    value[0] == LAMBDA);
}

/*
 * NAME:	add_worth()
 * DESCRIPTION:	dummy function
 */
static varargs void add_worth(int worth, object obj)
{
    /* do nothing */
}

/*
 * NAME:	wizlist()
 * DESCRIPTION:	show the wizlist
 */
static varargs void wizlist(string name)
{
    show_wiz_list();
}

/*
 * NAME:	transfer()
 * DESCRIPTION:	transfer an object
 */
static int transfer(mixed obj, mixed dest)
{
    int weight;
    object from, from_env, dest_env;

    if (!objectp(obj)) {
	call_other(obj = DRIVER->path_object(obj), "???");
	obj = find_object(obj);
    }
    ARGCHECK(objectp(obj), transfer, 1);
    if (!objectp(dest)) {
	call_other(dest = DRIVER->path_object(dest), "???");
	dest = find_object(dest);
    }
    ARGCHECK(objectp(dest), transfer, 2);

    weight = obj->query_weight();
    if (obj == 0) {
	return 3;
    }
    from = environment(obj);
    if (from != 0 && living(from) && (obj->drop() || obj == 0)) {
	return 2;
    }
    if (from != 0 && (from_env=environment(from)) != 0 && !living(from) &&
	(!from->can_put_and_get() || from == 0)) {
	return 3;
    }

    if ((dest_env=environment(dest)) != 0 && !living(dest)) {
	if (obj->prevent_insert() || obj == 0) {
	    return 4;
	}
	if (!dest->can_put_and_get() || dest == 0) {
	    return 5;
	}
    }
    if (living(dest) && (!obj->get() || obj == 0)) {
	return 6;
    }
    if (dest_env != 0 && weight != 0 &&
	(!dest->add_weight(weight) || dest == 0)) {
	return 1;
    }
    if (from != 0 && from_env != 0 && weight != 0) {
	from->add_weight(-weight);
    }

    move_object(obj, dest);
    return 0;
}

/*
 * NAME:	present()
 * DESCRIPTION:	find an object in an inventory
 */
static varargs object present(mixed item, object obj)
{
    object env, *inv;
    int i, sz;

    if (obj == 0) {
	obj = this_object();
	inv = query_inventory(this_object());
	env = environment();
	if (env != 0) {
	    inv += query_inventory(env);
	}
    } else {
	inv = query_inventory(obj);
    }

    if (objectp(item)) {
	for (i = 0, sz = sizeof(inv); i < sz; i++) {
	    if (item == inv[i]) {
		return item;
	    }
	}
    } else {
	string str, dummy;
	int count;
	object invobj;

	ARGCHECK(stringp(item), present, 1);

	if (sscanf(item, "%s %d%s", str, count, dummy) == 3 && dummy == "") {
	    item = str;
	} else {
	    count = 1;
	}

	for (i = 0, sz = sizeof(inv); obj != 0 && i < sz; i++) {
	    invobj = inv[i];
	    if (invobj->id(item) && --count == 0) {
		return invobj;
	    }
	}
    }

    return 0;
}

/*
 * NAME:	_F_destruct()
 * DESCRIPTION:	let an object destruct itself
 */
nomask void _F_destruct()
{
    int i;

    if (PRIVILEGED()) {
	set_heart_beat(0);
	for (i = sizeof(status(this_object())[O_CALLOUTS]); i > 0; --i) {
	    del_call_out();
	}
	del_object();
	::destruct_object(this_object());
    }
}

/*
 * NAME:	destruct()
 * DESCRIPTION:	destruct an object
 */
static void destruct(object obj)
{
    private void move_or_destruct(object obj, object dest);
    object env, item, user;
    int weight;

    ARGCHECK(obj, destruct, 1);
    if (object_name(obj) == GLOBAL) {
	error("Cannot destruct the global object");
    } else if (sscanf(object_name(obj), USER + "#%*s") != 0 &&
	       obj->query_player() != 0) {
	error("Cannot destruct user object");
    }


    env = environment(obj);
    if (env != 0) {
	if (living(obj)) {
	    env->exit(obj);
	}
	weight = obj->query_weight();
	if (weight != 0) {
	    env->add_weight(-weight);
	}
	while ((item=first_inventory(obj)) != 0) {
	    move_or_destruct(item, env);
	}
    } else {
	while ((item=first_inventory(obj)) != 0) {
	    MASTER->destruct_environment_of(item);
	    if (item != 0 && item == first_inventory(obj)) {
		destruct(item);
	    }
	}
    }

    env = environment(obj);
    if (env != 0) {
	lock(remove_inv(obj, env));
    }
    if (function_object("valid_player", obj) == PLAYER) {
	user = obj->_Q_user();
	if (user != 0) {
	    lock(privileged = 1,
		 user->set_player(0),
		 privileged = 0);
	    destruct(user);
	}
    }
    lock(privileged = 1,
	 obj->_F_destruct(),
	 privileged = 0);
}

/*
 * NAME:	move_or_destruct()
 * DESCRIPTION:	move or destruct an object
 */
private void move_or_destruct(object obj, object dest)
{
    int res;
    object env;

    res = transfer(obj, dest);
    if (obj != 0) {
	switch (res) {
	case 0:
	    return;

	case 1:
	case 4:
	case 5:
	    env = environment(dest);
	    if (env != 0) {
		move_or_destruct(obj, env);
		return;
	    }

	default:
	    destruct(obj);
	    break;
	}
    }
}

/*
 * NAME:	destruct_object()
 * DESCRIPTION:	destruct an object
 */
static void destruct_object(object obj)
{
    destruct(obj);
}

/*
 * NAME:	query_host_name()
 * DESCRIPTION:	return the host name
 */
static string query_host_name()
{
    return "fancy.foo";
}

/*
 * NAME:	query_load_average()
 * DESCRIPTION:	return the load average
 */
static string query_load_average()
{
    return "0.00 cmds/s, 0.00 comp lines/s";
}

/*
 * NAME:	dump_state()
 * DESCRIPTION:	dump state
 */
static void dump_state()
{
    lock(privileged = 1,
	 DRIVER->set_users(),
	 ::dump_state(),
	 privileged = 0);
}

/*
 * NAME:	shutdown()
 * DESCRIPTION:	shut down the mud
 */
static void shutdown()
{
    lock(privileged = 1,
	 DRIVER->start_shut_down(),
	 privileged = 0);
}

/*
 * NAME:	version()
 * DESCRIPTION:	return the driver version number
 */
static string version()
{
    return status()[ST_VERSION];
}

/*
 * NAME:	create_wizard()
 * DESCRIPTION:	create a new wizard
 */
static varargs string create_wizard(string wizard, string domain)
{
    return MASTER->master_create_wizard(wizard, domain, this_object());
}

/*
 * NAME:	crypt()
 * DESCRIPTION:	encrypt a password
 */
static varargs string crypt(string passwd, string salt)
{
    ARGCHECK(passwd, crypt, 1);

    return (salt == 0) ? ::crypt(passwd) : ::crypt(passwd, salt);
}

/*
 * NAME:	notify_fail()
 * DESCRIPTION:	set the notify_fail message
 */
static void notify_fail(string mesg)
{
    object player;

    ARGCHECK(mesg, notify_fail, 1);

    player = this_player();
    if (player != 0 && (player=interactive(player)) != 0) {
	player->set_notify_fail(mesg);
    }
}
