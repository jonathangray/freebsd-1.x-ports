object user;		/* user for this editor */

/*
 * NAME:        edit()
 * DESCRIPTION: handle an editor command
 */
varargs void edit(string cmd)
{
    if (user == 0 && previous_object() == this_user()->query_player()) {
        user = this_user();
        cmd = editor(cmd);
    } else if (previous_object() == user) {
        cmd = editor(cmd);
    }
    if (cmd != 0) {
	user->catch_tell(cmd);
    }
}

/*
 * NAME:        rescue_file()
 * DESCRIPTION: attempt to rescue the currently edited file
 */
void rescue_file()
{
    string file, *path;

    if (previous_object() != user) {
        return;
    }

    if (query_editor(this_object()) == "input") {
	editor(".");
    }
    if (sscanf(editor("f"), "\"%s\" [Modified] %*s", file) == 2) {
        set_this_player(user->query_player());
        path = explode(file, "/");
        editor("w! " + "/players/" + user->query_player()->query_real_name() +
               "/.dead_ed_files/" + path[sizeof(path) - 1]);
    }
}
