# include <config.h>
# include "/dgd/lib/privilege.h"

object *usr;		/* user array just before a swapout */

/*
 * NAME:	initialize
 * DESCRIPTION:	called once at game startup
 */
static void initialize()
{
    string *castles, castle, err;
    int i, sz, t;

    send_message("\nLoading init file room/init_file\n");
    castles = explode(read_file("/room/init_file"), "\n");
    for (i = 0, sz = sizeof(castles); i < sz; i++) {
	castle = castles[i];
	if (strlen(castle) != 0 && castle[0] != '#') {
	    send_message("Preloading: " + castle);
	    t = time();
	    if (castle[strlen(castle) - 2 ..] == ".c") {
		castle = castle[0 .. strlen(castle) - 3];
	    }
	    err = catch(call_other(castle, "???"));
	    if (err == 0) {
		send_message(" " + (time() - t) + ".0\n");
	    } else {
		send_message(err + "\n");
	    }
	}
    }
    send_message("Setting up ipc.\n");
    call_out("swapswap", 1800);
}

/*
 * NAME:	swapswap()
 * DESCRIPTION:	swap out all objects
 */
static void swapswap()
{
    call_out("swapswap", 1800);
    usr = 0;
    swapout();
}

/*
 * NAME:	set_users()
 * DESCRIPTION:	keep a copy of the users array
 */
void set_users()
{
    if (PRIVILEGED()) {
	usr = users();
    }
}

/*
 * NAME:	restored()
 * DESCRIPTION:	re-initialize the system after a restore
 */
static void restored()
{
    int i, sz;

    if (usr != 0) {
	for (i = 0, sz = sizeof(usr); i < sz; i++) {
	    catch(usr[i]->force_close());
	}
    }
    usr = 0;
    send_message("\nState restored.\nSetting up ipc.\n");
}

/*
 * NAME:	path_ed_read()
 * DESCRIPTION:	handle an editor read path
 */
string path_ed_read(string path)
{
    if (path != "" && path[0] != '/') {
	path = "/" + this_user()->query_player()->query_path() + "/" + path;
    }
    return this_user()->query_player()->valid_read(path);
}

/*
 * NAME:	path_ed_write()
 * DESCRIPTION:	handle an editor write path
 */
string path_ed_write(string path)
{
    if (path != "" && path[0] != '/') {
	path = "/" + this_user()->query_player()->query_path() + "/" + path;
    }
    return this_user()->query_player()->valid_write(path);
}

/*
 * NAME:	path_object()
 * DESCRIPTION:	translate an object path
 */
string path_object(string path)
{
    int i;

    if (sscanf(path, "../%*s") != 0 || sscanf(path, "%*s/../%*s") != 0) {
	error("Illegal path");
    }
    if ((i=strlen(path)) >= 2 && path[i - 2 ..] == ".c") {
	return path[0 .. i - 3];
    }
    return path;
}

/*
 * NAME:	path_inherit()
 * DESCRIPTION:	translate an inherit path
 */
string path_inherit(string file, string path)
{
    return path_object(path);
}

/*
 * NAME:	path_include()
 * DESCRIPTION:	translate an include path
 */
string path_include(string file, string path)
{
    if (path[0] != '/') {
	return file + "/../" + path;
    }
    return path;
}

/*
 * NAME:	compile_object()
 * DESCRIPTION:	(not) used for virtual objects
 */
static object compile_object(string file)
{
    return 0;
}

/*
 * NAME:	recompile()
 * DESCRIPTION:	(not) used to recompile objects
 */
static void recompile(object obj)
{
}

/*
 * NAME:	telnet_connect()
 * DESCRIPTION:	return a player object
 */
static object telnet_connect()
{
    object user, player;

    GLOBAL->set_this_player(0);
    user = clone_object(USER);
    player = MASTER->connect();
    user->set_player(player);
    player->_F_user(user);

    return user;
}

/*
 * NAME:	binary_connect()
 * DESCRIPTION:	return another player object (just to test)
 */
static object binary_connect()
{
    object user, player;

    GLOBAL->set_this_player(0);
    user = clone_object(USER);
    player = MASTER->connect();
    user->set_player(player);
    player->_F_user(user);

    return user;
}

/*
 * NAME:	log_error()
 * DESCRIPTION:	log a runtime error
 */
static void log_error(string error, int caught)
{
    mixed **trace;
    string progname, objname, function, str;
    int i, sz, line, len;
    object player;

    if (caught) {
	return;
    }
    send_message(error + "\n");
    trace = call_trace();
    if ((sz=sizeof(trace) - 1) != 0) {
	for (i = 0; i < sz; i++) {
	    progname = trace[i][1];
	    function = trace[i][2];

	    if (progname == AUTO && strlen(function) > 3) {
		switch (function[0 .. 2]) {
		case "bad":
		    progname = trace[i - 1][1];
		    function = trace[i - 1][2];
		case "_F_":
		case "_Q_":
		    continue;

		default:
		    break;
		}
	    }

	    objname  = trace[i][0];
	    line     = trace[i][3];

	    if (line == 0) {
		str = "    ";
	    } else {
		str = "    " + line;
		str = str[strlen(str) - 4 ..];
	    }
	    str += " " + function + " ";
	    len = strlen(function);
	    if (len < 17) {
		str += "                 "[len ..];
	    }
	    str += progname;
	    if (progname != objname) {
		len = strlen(progname);
		if (len < strlen(objname) && progname == objname[.. len - 1]) {
		    str += " (" + objname[len ..] + ")";
		} else {
		    str += " (" + objname + ")";
		}
	    }
	    send_message(str + "\n");
	}

	player = GLOBAL->query_this_player();
	if (player != 0 && function_object("valid_player", player) == PLAYER &&
	    player->_Q_user() != 0) {
	    if (player->query_level() < 21) {
		player->catch_tell("Your significant mind notices the fabric " +
				   "of space.\n");
	    } else {
		player->catch_tell(error + "\nObject: " + objname +
				   ", program: " + progname + ", line " +
				   line + "\n");
	    }
	}
    }
}

/*
 * NAME:	compile_log()
 * DESCRIPTION:	return the name of a compile time error log
 */
string compile_log(string file)
{
    string *path;

    path = explode(file, "/");
    if (path[0] == "players") {
	return "/log/" + path[1];
    }
    return "/log/log";
}

int shutting_down;

/*
 * NAME:	start_shut_down()
 * DESCRIPTION:	start shutting down the game
 */
void start_shut_down()
{
    if (PRIVILEGED() && shutting_down == 0) {
	shutting_down = call_out("do_shutdown", 1);
    }
}

/*
 * NAME:	do_shutdown()
 * DESCRIPTION:	actually shut down the game
 */
static void do_shutdown()
{
    object *u, player;
    int i;

    u = users();
    for (i = 0; i < sizeof(u); i++) {
	u[i]->catch_tell("Deegeedee shouts: Mud shutting down immediately.\n");
    }

    for (i = 0; i < sizeof(u); i++) {
	player = u[i]->query_player();
	GLOBAL->set_this_player(player);
	catch(player->quit());
    }
    send_message("Shutdown.\n");
    shutdown();
}
