/*
 * NAME:	valid()
 * DESCRIPTION:	check if a path is valid
 */
private string valid(string file, int type)
{
    object player;

    if (object_name(this_object()) == MASTER) {
	return file;
    }

    player = this_player();
    if (player == 0 || interactive(player) == 0) {
	return 0;
    }
    file = (type == 0) ? player->valid_read(file) : player->valid_write(file);
    if (player == 0) {
	return 0;
    }
    if (!stringp(file)) {
	write("Bad file name.\n");
	return 0;
    } else if (file != "" &&
	       (file[0] == '/' || sscanf(file, "%*s ") != 0 ||
	        sscanf(file, "%*s..") != 0)) {
	error("Illegal path: " + file);
    }
    return file;
}

/*
 * NAME:	file_size()
 * DESCRIPTION:	get the size of a file
 */
static int file_size(string file)
{
    int *sizes;

    ARGCHECK(file, file_size, 1);
    file = valid(file, 0);
    if (file == 0) {
	return -1;
    }

    sizes = ::get_dir(file)[1];
    if (sizeof(sizes) != 1) {
	return -1;
    }

    return sizes[0];
}

/*
 * NAME:	log_file()
 * DESCRIPTION:	log something
 */
static void log_file(string file, string str)
{
    int *sizes;

    ARGCHECK(file, log_file, 1);
    ARGCHECK(str, log_file, 2);

    if (sscanf(file, "%*s/") != 0 || strlen(file) > 30) {
	error("Illegal file name to log_file(" + file + ")");
    }
    file = "/log/" + file;
    sizes = ::get_dir(file)[1];
    if (sizeof(sizes) == 1 && sizes[0] >= LOG_FILE_SIZE) {
	::remove_file(file + ".old");
	::rename_file(file, file + ".old");
    }
    ::write_file(file, str);
}

/*
 * NAME:	get_dir()
 * DESCRIPTION:	get directory info
 */
static string *get_dir(string file)
{
    int len;

    ARGCHECK(file, get_dir, 1);
    file = valid(file, 0);
    if (file == 0) {
	return ({ });
    }

    len = strlen(file);
    if (file == "" || file[len - 1] == '/') {
	file += "*";
    } else if (len > 1 && file[len - 2 ..] == "/.") {
	file[len - 1] = '*';
    }
    return ::get_dir(file)[0];
}

/*
 * NAME:	mkdir()
 * DESCRIPTION:	make a directory
 */
static int mkdir(string file)
{
    ARGCHECK(file, mkdir, 1);
    file = valid(file, 0);
    return (file != 0 && ::make_dir(file) != 0);
}

/*
 * NAME:	make_dir()
 * DESCRIPTION:	make a directory
 */
static int make_dir(string file)
{
    return mkdir(file);
}

/*
 * NAME:	read_bytes()
 * DESCRIPTION:	read bytes from a file
 */
static varargs string read_bytes(string file, int start, int size)
{
    ARGCHECK(file, read_bytes, 1);
    file = valid(file, 0);
    if (file == 0) {
	return 0;
    }

    return ::read_file(file, start, size);
}

/*
 * NAME:	read_lines()
 * DESCRIPTION:	return a line range of a file
 */
private string *read_lines(string file, int first, int num)
{
    int line, offset, size, *saved;
    string str, *lines;

    if (first < 0 || num < 0) {
	return 0;
    }

    if (first == 0) {
	first = 1;
    }
    line = 1;
    if (this_user()) {
	lock(privileged = 1,
	     saved = this_user()->query_file_offset(file),
	     privileged = 0);
	if (saved != 0) {
	    line = saved[0];
	    if (line > 2 * first) {
		line = 1;
	    } else {
		offset = saved[1];
	    }
	}
    }

    for (;;) {
	if (line <= first) {
	    str = ::read_file(file, offset, FILE_CHUNK);
	    if (str == 0) {
		return 0;
	    }
	    lines = explode("\n" + str + "\n", "\n");
	    size = sizeof(lines) - 1;
	    if (line == first) {
		if (num == 0 || size < num) {
		    if (strlen(str) < FILE_CHUNK) {
			return lines;
		    }
		    error("Line range too large");
		}
	    }
	    if (size == 0) {
		if (strlen(str) < FILE_CHUNK) {
		    return 0;
		}
		error("Line too long");
	    }
	} else {
	    offset -= FILE_CHUNK;
	    if (offset < 0) {
		offset = 0;
	    }
	    str = ::read_file(file, offset, FILE_CHUNK);
	    if (str == 0) {
		return 0;
	    }
	    lines = explode("\n" + str + "\n", "\n");
	    size = sizeof(lines) - 1;
	    if (offset == 0) {
		line = 1;
	    } else {
		if (size == 0) {
		    error("Line too long");
		} else {
		    --size;
		    line -= size;
		    if (line <= 0) {
			/* ??? */
			line = 1;
			offset = 0;
			continue;
		    }
		    offset += strlen(lines[0]) + 1;
		    lines = lines[1 ..];
		}
	    }
	}

	if (line <= first && line + size > first) {
	    if (num != 0 && line + size >= first + num) {
		first -= line;
		if (this_user() != 0) {
		    line += first + num;
		    offset += strlen(implode(lines[0 .. first + num - 1],
				     "\n")) + 1;
		    lock(privileged = 1,
			 this_user()->set_file_offset(file, line, offset),
			 privileged = 0);
		}
		return lines[first .. first + num - 1] + ({ "" });
	    }
	    size = first - line;
	}
	if (line < first) {
	    line += size;
	    offset += strlen(implode(lines[0 .. size - 1], "\n")) + 1;
	}
    }
}

/*
 * NAME:	read_file()
 * DESCRIPTION:	read a file
 */
static varargs string read_file(string file, int first, int num)
{
    string *lines;

    ARGCHECK(file, read_file, 1);
    file = valid(file, 0);
    if (file == 0) {
	return 0;
    }

    if (first == 0 && num == 0) {
	return ::read_file(file);
    }

    lines = read_lines(file, first, num);
    if (lines == 0) {
	return 0;
    }
    return implode(lines, "\n");
}

/*
 * NAME:	rm()
 * DESCRIPTION:	remove a file
 */
static int rm(string file)
{
    ARGCHECK(file, rm, 1);
    file = valid(file, 1);
    return (file != 0 && ::remove_file(file));
}

/*
 * NAME:	remove_file()
 * DESCRIPTION:	remove a file
 */
static int remove_file(string file)
{
    return rm(file);
}

/*
 * NAME:	rmdir()
 * DESCRIPTION:	remove a directory
 */
static int rmdir(string file)
{
    ARGCHECK(file, rmdir, 1);
    file = valid(file, 1);
    return (file != 0 && ::remove_dir(file));
}

/*
 * NAME:	remove_dir()
 * DESCRIPTION:	remove a directory
 */
static int remove_dir(string file)
{
    return rmdir(file);
}

/*
 * NAME:	rename()
 * DESCRIPTION:	rename a file
 */
static int rename(string from, string to)
{
    ARGCHECK(from, rename, 1);
    ARGCHECK(to, rename, 2);

    from = valid(from, 1);
    if (from == 0) {
	return 0;
    }
    to = valid(to, 1);
    if (to == 0) {
	return 0;
    }
    return ::rename_file(from, to);
}

/*
 * NAME:	rename_file()
 * DESCRIPTION:	rename a file
 */
static int rename_file(string from, string to)
{
    return rename(from, to);
}

/*
 * NAME:	restore_object()
 * DESCRIPTION:	restore an object
 */
static int restore_object(string file)
{
    ARGCHECK(file, restore_object, 1);
    return ::restore_object(file + ".o");
}

/*
 * NAME:	save_object()
 * DESCRIPTION:	save an object
 */
static void save_object(string file)
{
    string str, *path;

    ARGCHECK(file, save_object, 1);

    if (this_object() == 0) {
	return;
    }
    path = explode(object_name(this_object()), "/");
    switch (path[0]) {
    case "players":
	if (sscanf(file, "players/%s/", str) == 0 || str != path[1] ||
	    sscanf(file, "%*s.") != 0) {
	    error("Illegal save file name " + file);
	}
	break;

    case "obj":
    case "room":
    case "std":
	break;

    default:
	error("Illegal use of save_object()");
    }
    ::save_object(file + ".o");
}

/*
 * NAME:	write_bytes()
 * DESCRIPTION:	write bytes to file
 */
static int write_bytes(string file, int start, string str)
{
    int *sizes;

    ARGCHECK(file, write_bytes, 1);
    ARGCHECK(str, write_bytes, 2);

    file = valid(file, 1);
    if (file != 0) {
	if (start == 0) {
	    sizes = ::get_dir(file)[1];
	    if (sizeof(sizes) == 1) {
		start = -sizes[0];
	    }
	}
	return ::write_file(file, str, start);
    }
    return 0;
}

/*
 * NAME:	write_file()
 * DESCRIPTION:	write string to file
 */
static int write_file(string file, string str)
{
    ARGCHECK(file, write_file, 1);
    file = valid(file, 1);
    return (file != 0 && ::write_file(file, str));
}

/*
 * NAME:	cat()
 * DESCRIPTION:	show a file
 */
static varargs int cat(string file, int first, int num)
{
    string *lines;

    ARGCHECK(file, cat, 1);
    file = valid(file, 0);
    if (file == 0) {
	return 0;
    }

    if (num == 0) {
	num = CAT_LINES + 1;
    }
    lines = read_lines(file, first, num);
    if (lines == 0) {
	return 0;
    }

    if (sizeof(lines) <= CAT_LINES + 1) {
	write(implode(lines, "\n"));
    } else {
	write(implode(lines[0 .. CAT_LINES - 1], "\n") +
	      "\n***TRUNCATED***\n");
    }
    return 1;
}

/*
 * NAME:	tail()
 * DESCRIPTION:	show the tail of a file
 */
static int tail(string file)
{
    int size, *sizes;
    string str, *lines;

    ARGCHECK(file, tail, 1);
    file = valid(file, 0);
    if (file == 0) {
	return 0;
    }

    sizes = ::get_dir(file)[1];
    if (sizeof(sizes) == 1) {
	size = TAIL_CHUNK;
	for (;;) {
	    if (size > sizes[0]) {
		size = sizes[0];
	    }

	    str = ::read_file(file, -size, size);
	    if (str == 0 || strlen(str) != size) {
		return 0;
	    }
	    lines = explode("\n" + str + "\n", "\n");

	    if (sizeof(lines) >= TAIL_LINES + 1 || size == sizes[0]) {
		if ((size=sizeof(lines)) > TAIL_LINES + 1) {
		    str = implode(lines[size - TAIL_LINES - 1 ..], "\n");
		} else if (size == 1) {
		    str = lines[0];
		}
		write(str);
		return 1;
	    }
	    size += TAIL_CHUNK;
	}
    }
    return 0;
}

/*
 * NAME:	editor()
 * DESCRIPTION:	handle an editor command
 */
static string editor(string cmd)
{
    string fname;

    fname = explode(object_name(this_object()), "#")[0];
    if (fname != EDITOR && fname != CINDENT) {
	error("Illegal call to editor()");
    }
    if (cmd == 0) {
	return ::editor();
    } else {
	return ::editor(cmd);
    }
}

/*
 * NAME:	ed()
 * DESCRIPTION:	start an editor session
 */
static varargs void ed(string file, string exit_func)
{
    object user, editor;

    if (this_player() == 0) {
	return;
    }
    if ((user=interactive(this_object())) == 0) {
	error("Tried to start an ed session on a non-interactive player");
    }
    if (this_player() != this_object()) {
	error("Illegal start of ed");
    }

    lock(privileged = 1,
	 editor = user->query_editor(),
	 privileged = 0);
    if (editor != 0) {
	error("Tried to start an ed session, when already active");
    }
    lock(privileged = 1,
	 editor = clone_object(EDITOR),
	 user->set_editor(editor, exit_func),
	 privileged = 0);

    if (file == 0) {
	editor->edit();
    } else {
	editor->edit("e /" + file);
    }
}

/*
 * NAME:	cindent()
 * DESCRIPTION:	indent an LPC file
 */
static int cindent(string file)
{
    return clone_object(CINDENT)->indent(file);
}

/*
 * NAME:	ls()
 * DESCRIPTION:	write a directory listing
 */
static int ls(string file)
{
    mixed *dir;
    string *list, str, dirlist;
    int *sizes, i, j, sz, max, rows;

    ARGCHECK(file, ls, 1);
    file = valid(file, 0);
    if (file == 0) {
	return 0;
    }

    i = strlen(file);
    if (file == "" || file[i - 1] == '/') {
	file += "*";
    } else if (i > 1 && file[i - 2 ..] == "/.") {
	file[i - 1] = '*';
    }
    dir = ::get_dir(file);
    list = dir[0];
    if (sizeof(list) == 0) {
	return 0;
    }

    for (i = 0, sz = sizeof(list); i < sz; i++) {
	j = strlen(list[i]);
	if (j > max) {
	    max = j;
	}
    }
    max++;
    j = 80 / (max + 1);
    rows = sz / j;
    if (sz % j > 0) {
	rows++;
    }

    dirlist = "";
    sizes = dir[1];
    for (i = 0; i < rows; i++) {
	j = i;
	for (;;) {
	    str = list[j];
	    if (sizes[j] < 0) {
		str += "/";
	    }
	    j += rows;
	    if (j >= sz) {
		dirlist += str + "\n";
		break;
	    }
	    dirlist += (str + "                                        ")
		       [0 .. max];
	}
    }
    write(dirlist);

    return 1;
}
