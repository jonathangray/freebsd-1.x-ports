private mixed *callout;		/* call_out key */

/*
 * NAME:	call_out()
 * DESCRIPTION:	handle a call_out
 */
static varargs void call_out(string func, int delay, mixed args...)
{
    ARGCHECK(func, call_out, 1);

    if (func != "_F_reset") {
	lock(add_call_out(),
	     ::call_out("_F_call_out", delay, callout, func, this_player(),
			args...));
    }
}

/*
 * NAME:	_F_call_out()
 * DESCRIPTION:	handle a call_out
 */
nomask varargs void
_F_call_out(mixed *key, string func, object player, mixed args...)
{
    if (key == callout) {
	del_call_out();
	set_this_player(player);
	call_other(this_object(), func, args...);
    }
}

/*
 * NAME:	find_call_out()
 * DESCRIPTION:	find a call_out
 */
static int find_call_out(string func)
{
    mixed **clist, *c;
    int i, sz;

    clist = status(this_object())[O_CALLOUTS];
    for (i = 0, sz = sizeof(clist); i < sz; i++) {
	if (sizeof(c=clist[i]) > 3 && c[4] == func) {
	    return c[2];
	}
    }
    return -1;
}

/*
 * NAME:	remove_call_out()
 * DESCRIPTION:	remove a call_out
 */
static int remove_call_out(string func)
{
    mixed **clist, *c;
    int i, sz;

    clist = status(this_object())[O_CALLOUTS];
    for (i = 0, sz = sizeof(clist); i < sz; i++) {
	if (sizeof(c=clist[i]) > 3 && c[4] == func) {
	    return lock(del_call_out(),
			::remove_call_out(c[0]));
	}
    }
    return -1;
}

/*
 * NAME:	set_heart_beat()
 * DESCRIPTION:	start or stop the heart_beat
 */
static int set_heart_beat(int flag)
{
    if (flag) {
	if (callout[0] == 0) {
	    lock(add_heart_beat(),
		 callout[0] = ::call_out("_F_heart_beat", 1 + (time() & 1),
					 callout));
	    return 1;
	}
    } else {
	if (callout[0] > 0) {
	    lock(del_heart_beat(),
		 ::remove_call_out(callout[0]),
		 callout[0] = 0);
	    return 1;
	}
	callout[0] = 0;
    }
    return 0;
}

/*
 * NAME:	_F_heart_beat()
 * DESCRIPTION:	handle heart_beat calls
 */
nomask void _F_heart_beat(mixed *key)
{
    if (key == callout) {
	key[0] = -1;
	del_heart_beat();
	if (living(this_object())) {
	    set_this_player(this_object());
	} else {
	    set_this_player(0);
	}
	this_object()->heart_beat();
	if (key[0] < 0) {
	    lock(add_heart_beat(),
		 key[0] = ::call_out("_F_heart_beat", 1 + (time() & 1), key));
	}
    }
}
