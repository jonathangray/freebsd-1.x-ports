private int reset_time;

/*
 * NAME:	_F_reset0()
 * DESCRIPTION:	reset an object for the first time
 */
private void _F_reset0()
{
    object save_player;

    if (function_object("reset", this_object()) != 0) {
	reset_time = time() + RESET_TIME * 50 + random(RESET_TIME * 20);
	save_player = this_player();
	this_object()->reset(0);
	set_this_player(save_player);
    } else {
	reset_time = INT_MAX;
    }
}

/*
 * NAME:	_F_reset()
 * DESCRIPTION:	reset an object, if it is time
 */
nomask void _F_reset()
{
    object save_player;

    if (reset_time <= time()) {
	reset_time = time() + RESET_TIME * 50 + random(RESET_TIME * 20);
	save_player = this_player();
	this_object()->reset(1);
	set_this_player(save_player);
    }
}
