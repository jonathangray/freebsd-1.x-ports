# include <config.h>
# undef status
# include <status.h>
# include "/dgd/lib/privilege.h"

object player;		/* associated player object */
int echo;		/* is input echoing turned on */
object snooper;		/* snooper of this object */
object input_obj;	/* object for input_to */
string input_func;	/* function for input_to */
object editor;		/* editor object */
string exit_func;	/* editor exit function */
int timestamp;		/* last time something was typed */
string fail_mesg;	/* notify_fail message */
string file;		/* file for which offset is remembered */
int line, offset;	/* line and offset in file */

/*
 * NAME:	set_player()
 * DESCRIPTION:	set the player object
 */
void set_player(object obj)
{
    if (object_name(previous_object()) == DRIVER || PRIVILEGED()) {
	player = obj;
    }
}

/*
 * NAME:	query_player()
 * DESCRIPTION:	return the player object
 */
object query_player()
{
    return player;
}

/*
 * NAME:	open()
 * DESCRIPTION:	start a user connection for this object
 */
static void open()
{
    timestamp = time();
    echo = 1;
    set_this_player(player);
    HNAME->request_ip_name(query_ip_number(player));
    player->_F_call("logon");
}

/*
 * NAME:	close()
 * DESCRIPTION:	close the user connection of this object
 */
static void close()
{
    if (editor != 0) {
	editor->rescue_file();
	destruct(editor);
    }
    if (player != 0) {
	player = 0;
	destruct(this_object());
    }
}

/*
 * NAME:	force_close()
 * DESCRIPTION:	forcibly close the user connection
 */
void force_close()
{
    if (object_name(previous_object()) == DRIVER) {
	player = 0;
	if (editor != 0) {
	    destruct(editor);
	}
	destruct(this_object());
    }
}

/*
 * NAME:	catch_tell()
 * DESCRIPTION:	catch a message
 */
void catch_tell(string str)
{
    if (str == 0) {
	return;
    }

    send_message(str);
    if (snooper != 0) {
	snooper->catch_tell("%" + str);
    }
}

/*
 * NAME:        ralign()
 * DESCRIPTION: return a number as a string, aligned to the right
 */
 string ralign(int num)
{
    string str;

    str = "" + num;
    return "           "[0 .. 10 - strlen(str)] + str;
}

/*
 * NAME:        show_status()
 * DESCRIPTION: show resource usage information
 */
private void show_status()
{
    mixed *stat;

    stat = status();
    catch_tell(
"Swap device:        " + ralign(stat[ST_SWAPSIZE]) + " sectors,\n" +
"                    " + ralign(stat[ST_SWAPUSED]) + " used.\n" +
"Objects swapped out:" + ralign(stat[ST_SWAPRATE1]) + " per minute.\n" +
"Memory allocated:   " + ralign(stat[ST_SMEMSIZE] + stat[ST_DMEMSIZE]) +
		     " bytes,\n" +
"                    " + ralign(stat[ST_SMEMUSED] + stat[ST_DMEMUSED]) +
		     " used.\n" +
"Objects:            " + ralign(stat[ST_NOBJECTS]) + "\n" +
"Call_outs:          " + ralign(stat[ST_NCOSHORT] + stat[ST_NCOLONG]) + "\n");
}

/*
 * NAME:	receive_message()
 * DESCRIPTION:	receive user input
 */
static void receive_message(string str)
{
    set_this_player(player);
    timestamp = time();
    if (echo && snooper != 0) {
	snooper->catch_tell("%" + str + "\n");
    }
    if (echo == 0) {
	send_message(echo = 1);
    }

    if (strlen(str) != 0 && str[0] == '!' && environment(player)) {
	if (str == "!status") {
	    show_status();
	} else {
	    command(str[1 ..], player);
	}
    } else if (editor != 0) {
	editor->edit(str);
	if (query_editor(editor) == 0) {
	    destruct(editor);
	    if (exit_func == 0) {
		catch_tell("Exit from ed.\n");
	    } else {
		call_other(player, exit_func);
	    }
	}
    } else if (input_obj != 0) {
	object obj;
	string func;

	obj = input_obj;
	func = input_func;
	input_obj = 0;
	input_func = 0;
	send_message(1);
	obj->_F_call(func, str);
    } else {
	if (input_func != 0) {
	    input_func = 0;
	    send_message(1);
	}
	if (strlen(str) != 0 && str[0] == '!') {
	    str = str[1 ..];
	}
	if (strlen(str) != 0) {
	    if (str == "status") {
		show_status();
	    } else {
		command(str, player);
	    }
	}
    }

    if (editor != 0) {
	catch_tell((query_editor(editor) == "insert") ? "*\b" : ":");
    } else if (input_obj == 0) {
	catch_tell("> ");
    }
}

/*
 * NAME:	set_input_to()
 * DESCRIPTION:	redirect user input to a function
 */
int set_input_to(object obj, string func, int flag)
{
    if (PRIVILEGED() && input_obj == 0) {
	input_obj = obj;
	input_func = func;
	if (flag) {
	    send_message(echo = 0);
	}
	return 1;
    }
    return 0;
}

/*
 * NAME:	set_snoop()
 * DESCRIPTION:	set the snooper of this object
 */
void set_snoop(object snoopy)
{
    if (PRIVILEGED()) {
	snooper = snoopy;
    }
}

/*
 * NAME:	query_snoop()
 * DESCRIPTION:	query the snooper of this object
 */
object query_snoop()
{
    if (PRIVILEGED()) {
	return snooper;
    }
}

/*
 * NAME:	set_editor()
 * DESCRIPTION:	set the editor object
 */
void set_editor(object ed, string func)
{
    if (PRIVILEGED()) {
	editor = ed;
	exit_func = func;
    }
}

/*
 * NAME:	query_editor()
 * DESCRIPTION:	return the editor object
 */
object query_editor()
{
    if (PRIVILEGED()) {
	return editor;
    }
}

/*
 * NAME:	set_notify_fail()
 * DESCRIPTION:	set the notify_fail message
 */
void set_notify_fail(string mesg)
{
    fail_mesg = mesg;
}

/*
 * NAME:	notify_fail()
 * DESCRIPTION:	display the notify_fail message
 */
void notify_fail()
{
    if (fail_mesg != 0) {
	catch_tell(fail_mesg);
    }
}

/*
 * NAME:	set_file_offset()
 * DESCRIPTION:	set the offset in a file
 */
void set_file_offset(string f, int l, int o)
{
    if (PRIVILEGED()) {
	file = f;
	line = l;
	offset = o;
    }
}

/*
 * NAME:	query_file_offset()
 * DESCRIPTION:	return a file offset
 */
int *query_file_offset(string f)
{
    if (PRIVILEGED() && file == f) {
	return ({ line, offset });
    }
    return 0;
}

/*
 * NAME:	query_idle()
 * DESCRIPTION:	return idle time of this user
 */
int query_idle()
{
    return time() - timestamp;
}
