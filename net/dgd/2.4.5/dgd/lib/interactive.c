/*
 * NAME:	interactive()
 * DESCRIPTION:	return the interactive user of an object
 */
static varargs object interactive(object obj)
{
    if (obj == 0) {
	obj = this_object();
    }

    return (function_object("valid_player",obj) == PLAYER) ? obj->_Q_user() : 0;
}

/*
 * NAME:	query_ip_number()
 * DESCRIPTION:	based on kfun query_ip_number()
 */
static varargs string query_ip_number(object obj)
{
    if (obj == 0) {
	obj = this_player();
    }
    ARGCHECK(obj, query_ip_number, 1);

    obj = interactive(obj);
    return (obj == 0) ? 0 : ::query_ip_number(obj);
}

/*
 * NAME:	query_ip_name()
 * DESCRIPTION:	based on kfun query_ip_number()
 */
static varargs string query_ip_name(object obj)
{
    if (obj == 0) {
	obj = this_player();
    }
    ARGCHECK(obj, query_ip_name, 1);

    obj = interactive(obj);
    return (obj == 0) ? 0 : HNAME->request_ip_name(::query_ip_number(obj));
}

/*
 * NAME:	query_idle()
 * DESCRIPTION:	return the idle time of a user
 */
static int query_idle(object obj)
{
    ARGCHECK(obj, query_idle, 1);
    if ((obj=interactive(obj)) == 0) {
	error("query_idle() of non-interactive object");
    }

    return obj->query_idle();
}

/*
 * NAME:	exec()
 * DESCRIPTION:	change the player object of a user
 */
static int exec(object obj, object from)
{
    string str;

    str = object_name(this_object())[1 ..];
    sscanf(str, "%s#", str);
    if (!MASTER->valid_exec(str)) {
	return 0;
    }
    ARGCHECK(obj && interactive(obj) == 0 &&
			function_object("valid_player", obj) == PLAYER,
	     exec, 1);
    ARGCHECK(from && (from=interactive(from)) != 0, exec, 2);

    lock(privileged = 1,
	 from->query_player()->_F_user(0),
	 from->set_player(obj),
	 obj->_F_user(from),
	 privileged = 0);
    return 1;
}

/*
 * NAME:	write()
 * DESCRIPTION:	send a message to the current player
 */
static void write(mixed str)
{
    object player;

    player = this_player();
    if (player != 0) {
	if (intp(str) || floatp(str)) {
	    str = str + "";
	} else if (objectp(str)) {
	    str = object_name(str);
	    str = "OBJ(" + str[1 ..] + ")";
	} else if (arrayp(str)) {
	    str = (closurep(str)) ? "<CLOSURE>" : "<ARRAY>";
	} else if (mappingp(str)) {
	    str = "<MAPPING>";
	}
	player->catch_tell(str);
    }
}

/*
 * NAME:	tell_object()
 * DESCRIPTION:	send a message to an object
 */
static void tell_object(object obj, string str)
{
    ARGCHECK(obj, tell_object, 1);
    ARGCHECK(str, tell_object, 2);

    obj->catch_tell(str);
}

/*
 * NAME:	tell_room
 * DESCRIPTION:	send a message to players in a room
 */
static varargs void tell_room(mixed room, string str, mixed avoid)
{
    object *inv, obj;
    int i, sz;

    if (this_object() == 0) {
	return;
    }

    if (stringp(room)) {
	call_other(room = DRIVER->path_object(room), "???");
	room = find_object(room);
    }
    ARGCHECK(objectp(room), tell_room, 1);
    ARGCHECK(str, tell_room, 2);

    inv = query_inventory(room);
    if (avoid != 0) {
	if (objectp(avoid)) {
	    inv -= ({ avoid });
	} else {
	    ARGCHECK(arrayp(avoid), tell_room, 3);
	    inv -= avoid;
	}
    }

    for (i = 0, sz = sizeof(inv); i < sz; i++) {
	obj = inv[i];
	if (obj != 0 && living(obj)) {
	    obj->catch_tell(str);
	}
    }
}

/*
 * NAME:	say()
 * DESCRIPTION:	send a message to all living objects in the same room
 */
static varargs void say(string str, mixed avoid)
{
    object save_player, origin;
    object *list, obj;
    int i, sz;

    ARGCHECK(str, say, 1);
    if (this_object() == 0) {
	return;
    }

    save_player = this_player();
    if (living(this_object())) {
	set_this_player(this_object());
	origin = this_object();
    } else if (save_player == 0) {
	origin = this_object();
    } else {
	origin = save_player;
    }

    list = query_inventory(origin);
    obj = environment(origin);
    if (obj != 0) {
	list = ({ obj }) + query_inventory(obj) + list;
    }

    if (avoid == 0 || avoid == origin) {
	list -= ({ origin });
    } else if (objectp(avoid)) {
	list -= ({ origin, avoid });
    } else {
	ARGCHECK(arrayp(avoid), say, 2);
	list -= ({ origin }) + avoid;
    }

    for (i = 0, sz = sizeof(list); i < sz; i++) {
	obj = list[i];
	if (obj != 0 && living(obj)) {
	    obj->catch_tell(str);
	}
    }

    set_this_player(save_player);
}

/*
 * NAME:	shout()
 * DESCRIPTION:	tell a string to all players but this_player()
 */
static void shout(string str)
{
    object player, user, *usr, obj;
    int i, sz;

    ARGCHECK(str, shout, 1);

    player = this_player();
# ifdef LOG_SHOUT
    if (player != 0 && (user=interactive(player)) != 0) {
	log_file("SHOUTS", player->query_real_name() + ": " + str);
    }
# endif
    usr = ::users();
    for (i = 0, sz = sizeof(usr); i < sz; i++) {
	obj = usr[i];
	if (obj != user && object_name(obj->query_player()) != HNAME) {
	    obj->catch_tell(str);
	}
    }
}

/*
 * NAME:	input_to()
 * DESCRIPTION:	redirect user input to a function
 */
static varargs int input_to(string func, int flag)
{
    object player;

    ARGCHECK(func, input_to, 1);

    player = this_player();
    if (player != 0 && (player=interactive(player)) != 0) {
	int res;

	lock(privileged = 1,
	     res = player->set_input_to(this_object(), func, flag),
	     privileged = 0);
	return res;
    }
    return 0;
}

/*
 * NAME:	snoop()
 * DESCRIPTION:	snoop another player
 */
static varargs object snoop(object obj)
{
    object player, snooper, snooped;
    object *u;
    int i, sz;

    player = this_player();
    if (player == 0 || (player=interactive(player)) == 0) {
	return 0;
    }

    u = ::users();
    for (i = 0, sz = sizeof(u); i < sz; i++) {
	snooped = u[i];
	lock(privileged = 1,
	     snooper = snooped->query_snoop(),
	     privileged = 0);
	if (snooper == player) {
	    lock(privileged = 1,
		 snooped->set_snoop(0),
		 privileged = 0);
	    break;
	}
    }

    if (obj == 0) {
	write("Ok.\n");
	return 0;
    }

    if ((obj=interactive(obj)) != 0 && player != obj) {
	lock(privileged = 1,
	     snooper = obj->query_snoop(),
	     privileged = 0);
	if (snooper != 0 && snooper != player) {
	    write("Busy.\n");
	    return obj;
	}

	snooped = player;
	for (;;) {
	    lock(privileged = 1,
		 snooper = snooped->query_snoop(),
		 privileged = 0);
	    if (snooper == 0) {
		lock(privileged = 1,
		     obj->set_snoop(player),
		     privileged = 0);
		write("Ok.\n");
		return obj;
	    }
	    if (snooper == obj) {
		write("Busy.\n");
		return obj;
	    }
	    snooped = snooper;
	}
    }

    write("Failed.\n");
    return obj;
}

/*
 * NAME:	query_snoop()
 * DESCRIPTION:	query the snooper of a player
 */
static object query_snoop(object obj)
{
    object player, snooper;

    ARGCHECK(obj, query_snoop, 1);

    player = this_player();
    if (player == 0 || (obj=interactive(obj)) == 0 ||
	player->query_level() < 22) {
	return 0;
    }
    lock(privileged = 1,
	 snooper = obj->query_snoop(),
	 privileged = 0);

    if (snooper != 0) {
	return snooper->query_player();
    }
    return 0;
}

/*
 * NAME:	users()
 * DESCRIPTION:	return an array with the player objects
 */
static object *users()
{
    object *usr;
    int i, sz;

    usr = ::users();
    for (i = 0, sz = sizeof(usr); i < sz; i++) {
	usr[i] = usr[i]->query_player();
    }
    return usr - ({ find_object(HNAME) });
}
