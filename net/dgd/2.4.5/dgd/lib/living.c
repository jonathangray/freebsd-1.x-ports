# define A_OBJ		0
# define A_NEXT		1
# define A_VERB		2
# define A_FUNC		3

private int alive;		/* living flag */
private mixed *chunks;		/* array of action chunks */
private mapping actions;	/* objects:actions mapping */
private string saved_action;	/* action saved by add_action with 1 argument */

/*
 * NAME:	_Q_alive()
 * DESCRIPTION:	return the living status of this object
 */
nomask int _Q_alive()
{
    return alive;
}

/*
 * NAME:	_Q_actions()
 * DESCRIPTION:	return the action chunks of this object
 */
nomask mixed *_Q_actions()
{
    if (PRIVILEGED()) {
	return chunks;
    }
}

/*
 * NAME:	_F_add_action()
 * DESCRIPTION:	define an action for this object
 */
nomask void _F_add_action(object obj, string verb, string func, int flag)
{
    if (PRIVILEGED()) {
	mixed chunk;

	if (flag) {
	    chunk = ({ obj, actions[obj], verb, func });
	    chunks = ({ chunk }) + chunks;
	    actions[obj] = chunk;
	} else if (sizeof(chunks) != 0 && mappingp(chunk=chunks[0]) &&
		   chunk[A_OBJ] == obj && chunk[verb] == 0) {
	    chunk[verb] = func;
	} else {
	    chunk = ([ A_OBJ:obj, A_NEXT:actions[obj], verb:func ]);
	    chunks = ({ chunk }) + chunks;
	    actions[obj] = chunk;
	}
    }
}

/*
 * NAME:	_F_remove_actions()
 * DESCRIPTION:	remove the actions defined by obj
 */
nomask void _F_remove_actions(object obj)
{
    if (PRIVILEGED()) {
	mixed *removed, chunk;

	removed = ({ });
	for (chunk = actions[obj]; chunk != 0; chunk = chunk[A_NEXT]) {
	    chunk[A_OBJ] = 0;
	    removed += ({ chunk });
	}
	chunks -= removed;
	actions[obj] = 0;
    }
}

/*
 * NAME:	_F_checkfunc()
 * DESCRIPTION:	check if a function exists
 */
nomask string _F_checkfunc(string func)
{
    if (PRIVILEGED()) {
	return function_object(func, this_object());
    }
}

/*
 * NAME:	_F_call()
 * DESCRIPTION:	handle the calling of a function
 */
nomask int _F_call(string func, string args)
{
    if (previous_object() == this_user()) {
	return call_other(this_object(), func, args);
    }
}

/*
 * NAME:	remove_actions
 * DESCRIPTION:	remove the actions defined for player by obj
 */
private void remove_actions(object player, object obj)
{
    player->_F_remove_actions(obj);
}

/*
 * NAME:	enable_commands()
 * DESCRIPTION:	mark this object as living
 */
static void enable_commands()
{
    alive = 1;
    set_this_player(this_object());
}

/*
 * NAME:	disable_commands()
 * DESCRIPTION:	mark this object as non-living
 */
static void disable_commands()
{
    if (this_player() == this_object()) {
	set_this_player(0);
    }
    alive = 0;
}

/*
 * NAME:	living()
 * DESCRIPTION:	determine if an object is alive
 */
static int living(object obj)
{
    ARGCHECK(obj, living, 1);

    return obj->_Q_alive();
}

/*
 * NAME:	add_action()
 * DESCRIPTION:	add an action for the current player
 */
static varargs void add_action(string func, string verb, int flag)
{
    object player, e1, e2;

    ARGCHECK(func, add_action, 1);

    player = this_player();
    if (player == 0) {
	return;
    }
    if (this_object() != player && (e1=environment()) != player &&
	e1 != (e2=environment(player)) && this_object() != e2) {
	error("add_action from object that was not present");
    }
    if (func == "exit") {
	error("Illegal to define a command to the exit() function");
    }
    if (verb == 0) {
	saved_action = func;
    } else {
	lock(privileged = 1,
	     player->_F_add_action(this_object(), verb, func, flag),
	     privileged = 0,
	     saved_action = 0);
    }
}

/*
 * NAME:	add_verb()
 * DESCRIPTION:	add a verb for the current player (most follow add_action()
 *		with one argument)
 */
static void add_verb(string verb)
{
    object player;

    ARGCHECK(verb, add_verb, 1);

    player = this_player();
    if (player != 0 && saved_action != 0) {
	lock(privileged = 1,
	     player->_F_add_action(this_object(), verb, saved_action, 0),
	     privileged = 0,
	     saved_action = 0);
    }
}

/*
 * NAME:	command()
 * DESCRIPTION:	force a given player to execute a command
 */
static varargs int command(string cmd, object obj)
{
    mixed *list, chunk;
    string arg, verb, func, prog;
    object player, save_player;
    int i, sz, exec_cost;

    ARGCHECK(cmd, command, 1);

    if (this_object() == 0) {
	return 0;
    }

    player = (obj != 0) ? obj : this_object();
    save_player = this_player();
    if (player != save_player && !player->_Q_alive()) {
	return 0;
    }
    set_this_player(player);
    notify_fail("What ?\n");

    lock(privileged = 1,
	 list = player->_Q_actions(),
	 privileged = 0);

    for (i = strlen(cmd); --i >= 0 && cmd[i] == ' '; ) ;
    if (i < 0) {
	return 0;
    }
    cmd = cmd[0 .. i];

    switch (cmd) {
    case "n": cmd = "north"; break;
    case "s": cmd = "south"; break;
    case "e": cmd = "east"; break;
    case "w": cmd = "west"; break;
    case "ne": cmd = "northeast"; break;
    case "nw": cmd = "northwest"; break;
    case "se": cmd = "southeast"; break;
    case "sw": cmd = "southwest"; break;
    case "u": cmd = "up"; break;
    case "d": cmd = "down"; break;
    default:
	sscanf(cmd, "%s %s", cmd, arg);
	break;
    }

    exec_cost = 1000 + get_exec_cost();

    for (i = 0, sz = sizeof(list); i < sz; i++) {
	if (player == 0 || this_object() == 0) {
	    return 0;
	}

	chunk = list[i];
	if (arrayp(chunk)) {
	    verb = chunk[A_VERB];
	    if (strlen(verb) > strlen(cmd) ||
		(cmd[0 .. strlen(verb) - 1] != verb && verb != "")) {
		continue;
	    }
	    func = chunk[A_FUNC];
	} else {
	    func = chunk[cmd];
	    if (func == 0) {
		continue;
	    }
	}

	obj = chunk[A_OBJ];
	if (obj == 0) {
	    continue;
	}
	set_verb(cmd);
	lock(privileged = 1,
	     prog = obj->_F_checkfunc(func),
	     privileged = 0);

	if (prog != 0) {
	    if (this_object() == this_user()) {
		if (obj->_F_call(func, arg)) {
		    set_verb(0);
		    set_this_player(save_player);
		    return exec_cost - get_exec_cost();
		}
		continue;
	    } else if (function_object(func, obj) != 0) {
		if (call_other(obj, func, arg)) {
		    set_verb(0);
		    set_this_player(save_player);
		    return exec_cost - get_exec_cost();
		}
		continue;
	    }
	}
	write("Error: function " + func + " not found.\n");
	set_verb(0);
	set_this_player(save_player);
	return exec_cost - get_exec_cost();
    }

    if (player != 0 && (player=interactive(player)) != 0) {
	player->notify_fail();
    }
    set_verb(0);
    set_this_player(save_player);
    return 0;
}

/*
 * NAME:	localcmd()
 * DESCRIPTION:	show the current verbs defined
 */
static void localcmd()
{
    string list;
    int i, sz;
    mixed chunk;

    list = "Local commands:\n";
    for (i = 0, sz = sizeof(chunks); i < sz; i++) {
	chunk = chunks[i];
	if (mappingp(chunk)) {
	    mapping m;

	    m = chunk + ([ ]);
	    m[A_OBJ] = 0;
	    m[A_NEXT] = 0;
	    list += implode(map_indices(m), " ") + " ";
	} else {
	    list += chunk[A_VERB] + " ";
	}
    }

    write(list[0 .. strlen(list) - 2] + "\n");
}
