/*
 * NAME:	init_global()
 * DESCRIPTION:	initialize the global object interface
 */
private void init_global()
{
    lock(privileged = 1,
	 GLOBAL->add_object(),
	 privileged = 0);
}

/*
 * NAME:	set_this_player()
 * DESCRIPTION:	set the current player
 */
static void set_this_player(object player)
{
    GLOBAL->set_this_player(player);
}

/*
 * NAME:	this_player()
 * DESCRIPTION:	return the current player, or interactive user
 */
static varargs object this_player(int flag)
{
    if (flag) {
	if (this_user()) {
	    return this_user()->query_player();
	}
	return 0;
    }
    return GLOBAL->query_this_player();
}

/*
 * NAME:	set_living_name()
 * DESCRIPTION:	set the living name of the current object
 */
static void set_living_name(string name)
{
    ARGCHECK(name, set_living_name, 1);

    lock(privileged = 1,
	 GLOBAL->set_living_name(name),
	 privileged = 0);
}

/*
 * NAME:	find_player()
 * DESCRIPTION:	find a player by name
 */
static object find_player(string name)
{
    ARGCHECK(name, find_player, 1);

    return GLOBAL->query_find_player(name);
}

/*
 * NAME:	find_living()
 * DESCRIPTION:	find a living object by name
 */
static object find_living(string name)
{
    ARGCHECK(name, find_living, 1);

    return GLOBAL->query_find_living(name);
}

/*
 * NAME:	set_verb()
 * DESCRIPTION:	set the current verb
 */
private void set_verb(string verb)
{
    lock(privileged = 1,
	 GLOBAL->set_verb(verb),
	 privileged = 0);
}

/*
 * NAME:	query_verb()
 * DESCRIPTION:	get the current verb
 */
static string query_verb()
{
    return GLOBAL->query_verb();
}

/*
 * NAME:	show_wiz_list()
 * DESCRIPTION:	show the wizlist
 */
private void show_wiz_list()
{
    GLOBAL->show_wiz_list();
}

/*
 * NAME:	del_object()
 * DESCRIPTION:	delete this object
 */
private void del_object()
{
    lock(privileged = 1,
	 GLOBAL->del_object(),
	 privileged = 0);
}

/*
 * NAME:	add_heart_beat()
 * DESCRIPTION:	add a heart_beat
 */
private void add_heart_beat()
{
    lock(privileged = 1,
	 GLOBAL->add_heart_beat(),
	 privileged = 0);
}

/*
 * NAME:	del_heart_beat()
 * DESCRIPTION:	delete a heart_beat
 */
private void del_heart_beat()
{
    lock(privileged = 1,
	 GLOBAL->del_heart_beat(),
	 privileged = 0);
}

/*
 * NAME:	add_call_out()
 * DESCRIPTION:	add a call_out
 */
private void add_call_out()
{
    if (object_name(this_object()) != GLOBAL) {
	lock(privileged = 1,
	     GLOBAL->add_call_out(),
	     privileged = 0);
    }
}

/*
 * NAME:	del_call_out()
 * DESCRIPTION:	delete a call_out
 */
private void del_call_out()
{
    if (object_name(this_object()) != GLOBAL) {
	lock(privileged = 1,
	     GLOBAL->del_call_out(),
	     privileged = 0);
    }
}

/*
 * NAME:	lambda()
 * DESCRIPTION:	compile a function definition to a closure
 */
private mixed relay(string func, mixed arg, closure expr)
{
    return call_other(GLOBAL, func, arg, expr);
}

/*
 * NAME:	lambda()
 * DESCRIPTION:	compile a function definition to a closure
 */
static varargs closure lambda(mixed func, mixed *def)
{
    if (def == 0) {
	ARGCHECK(stringp(func), lambda, 1);
    } else {
	ARGCHECK(arrayp(func), lambda, 1);
	ARGCHECK(arrayp(def) && sizeof(def) != 0, lambda, 2);
    }

    return GLOBAL->compile(func, def);
}

/*
 * NAME:	apply()
 * DESCRIPTION:	apply a closure
 */
static varargs mixed apply(mixed args...)
{
    if (sizeof(args) == 0) {
	error("Too few arguments to function apply");
    }
    return (closurep(args[0])) ? GLOBAL->eval(args, ([ ]))[1] : args[0];
}
