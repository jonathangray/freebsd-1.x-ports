private object environment;	/* environment of this object */
private object *inventory;	/* inventory of this object */
private object next_inv;	/* next object in inventory */

/*
 * NAME:	_Q_env()
 * DESCRIPTION:	return the environment of this object
 */
nomask object _Q_env()
{
    return environment;
}

/*
 * NAME:	_Q_inv()
 * DESCRIPTION:	return the inventory of this object
 */
nomask object *_Q_inv()
{
    if (PRIVILEGED()) {
	return inventory;
    }
}

/*
 * NAME:	_F_next_inv()
 * DESCRIPTION:	set the next object in the inventory
 */
nomask void _F_next_inv(object next)
{
    if (PRIVILEGED()) {
	next_inv = next;
    }
}

/*
 * NAME:	_Q_next_inv()
 * DESCRIPTION:	return the next object in the inventory
 */
nomask object _Q_next_inv()
{
    return next_inv;
}

/*
 * NAME:	_F_rm_inv()
 * DESCRIPTION:	remove an object from the inventory of this object
 */
nomask void _F_rm_inv(object obj, int light)
{
    if (PRIVILEGED()) {
	int alive, i, sz;
	object invobj;

	privileged++;
	alive = living(obj);
	if (alive) {
	    remove_actions(obj, this_object());
	}
	if (living(this_object())) {
	    remove_actions(this_object(), obj);
	}
	i = member_array(obj, inventory);
	if (i != 0) {
	    inventory[i - 1]->_F_next_inv((i == sizeof(inventory) - 1) ?
					   0 : inventory[i + 1]);
	}
	inventory -= ({ obj });
	add_light(-light);

	for (i = 0, sz = sizeof(inventory); i < sz; i++) {
	    invobj = inventory[i];
	    if (alive) {
		remove_actions(obj, invobj);
	    }
	    if (living(invobj)) {
		remove_actions(invobj, obj);
	    }
	}
	--privileged;
    }
}

/*
 * NAME:	_F_add_inv()
 * DESCRIPTION:	add an object to the inventory of this object
 */
nomask void _F_add_inv(object obj, int light)
{
    if (PRIVILEGED()) {
	privileged++;
	obj->_F_next_inv((sizeof(inventory) == 0) ? 0 : inventory[0]);
	inventory = ({ obj }) + inventory;
	add_light(light);
	--privileged;
    }
}

/*
 * NAME:	_F_move()
 * DESCRIPTION:	move this object somewhere
 */
nomask void _F_move(object env)
{
    if (PRIVILEGED()) {
	environment = env;
    }
}

/*
 * NAME:	query_inventory()
 * DESCRIPTION:	return the inventory of an object
 */
private object *query_inventory(object obj)
{
    object *inv;

    lock(privileged = 1,
	 inv = obj->_Q_inv(),
	 privileged = 0);

    return inv;
}

/*
 * NAME:	remove_inv()
 * DESCRIPTION:	remove object from inventory
 */
private void remove_inv(object obj, object from)
{
    privileged++;
    from->_F_rm_inv(obj, query_light(obj));
    --privileged;
}

/*
 * NAME:	move()
 * DESCRIPTION:	do the move thing
 */
private void move(object obj, object from, object dest)
{
    int light;

    privileged = 1;
    light = query_light(obj);
    if (from != 0) {
	from->_F_rm_inv(obj, light);
    }
    obj->_F_move(dest);
    dest->_F_add_inv(obj, light);
    privileged = 0;
}

/*
 * NAME:	environment()
 * DESCRIPTION:	return the environment of an object
 */
static varargs object environment(object obj)
{
    return (obj == 0) ? environment : obj->_Q_env();
}

/*
 * NAME:	move_object()
 * DESCRIPTION:	move an object
 */
static void move_object(mixed obj, mixed dest)
{
    object save_player, env, *inv, invobj;
    int i, sz;

    if (stringp(obj)) {
	call_other(obj = DRIVER->path_object(obj), "???");
	obj = find_object(obj);
    }
    ARGCHECK(objectp(obj), move_object, 1);
    if (stringp(dest)) {
	call_other(dest = DRIVER->path_object(dest), "???");
	dest = find_object(dest);
    }
    ARGCHECK(objectp(dest), move_object, 2);

    env = dest;
    do {
	if (obj == env) {
	    error("Can't move object inside itself.");
	}
    } while ((env=env->_Q_env()) != 0);

    save_player = this_player();

    env = obj->_Q_env();
    if (env != 0 && living(obj)) {
	set_this_player(obj);
	env->exit(obj);
	if (obj == 0) {
	    return;
	}
    }

    obj->_F_reset();
    dest->_F_reset();
    lock(move(obj, env, dest));

    if (living(obj)) {
	set_this_player(obj);
	dest->init();
	if (obj == 0 || obj->_Q_env() != dest) {
	    set_this_player(save_player);
	    return;
	}
    }
    inv = query_inventory(dest) - ({ obj });
    for (i = 0, sz = sizeof(inv); i < sz; i++) {
	invobj = inv[i];
	if (invobj != 0 && living(invobj)) {
	    set_this_player(invobj);
	    obj->init();
	    if (obj == 0 || obj->_Q_env() != dest) {
		set_this_player(save_player);
		return;
	    }
	}
	if (living(obj) && invobj != 0) {
	    set_this_player(obj);
	    invobj->_F_reset();
	    invobj->init();
	    if (obj == 0 || obj->_Q_env() != dest) {
		set_this_player(save_player);
		return;
	    }
	}
    }
    if (living(dest)) {
	set_this_player(dest);
	obj->init();
    }
    set_this_player(save_player);
}

/*
 * NAME:	all_inventory()
 * DESCRIPTION:	return an array with the inventory of an object
 */
static varargs object *all_inventory(object obj)
{
    object *inv;

    if (obj == 0) {
	obj = this_object();
    }
    ARGCHECK(obj, all_inventory, 1);

    inv = query_inventory(obj);
    return inv[..];
}

/*
 * NAME:	deep_inventory()
 * DESCRIPTION:	return the deep inventory of an object
 */
static object *deep_inventory(object obj)
{
    object *inv, *sub;
    int i, sz;

    ARGCHECK(obj, deep_inventory, 1);

    inv = query_inventory(obj);
    for (i = 0, sz = sizeof(inv); i < sz; i++) {
	sub = query_inventory(inv[i]);
	if (sizeof(sub) != 0) {
	    inv += sub;
	    sz += sizeof(sub);
	}
    }

    return inv;
}

/*
 * NAME:	first_inventory()
 * DESCRIPTION:	return the first item in the inventory of obj
 */
static varargs object first_inventory(mixed obj)
{
    object *inv;

    if (obj == 0) {
	obj == this_object();
    } else if (stringp(obj)) {
	call_other(obj = DRIVER->path_object(obj), "???");
	obj = find_object(obj);
    }
    ARGCHECK(objectp(obj), first_inventory, 1);

    inv = query_inventory(obj);
    obj = (sizeof(inv) != 0) ? inv[0] : 0;

    return obj;
}

/*
 * NAME:	next_inventory()
 * DESCRIPTION:	return the next object in the environment's inventory
 */
static varargs object next_inventory(object obj)
{
    if (obj == 0) {
	obj = this_object();
    }
    ARGCHECK(obj, next_inventory, 1);

    return obj->_Q_next_inv();
}
