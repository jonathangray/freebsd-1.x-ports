private string creator;

/*
 * NAME:	_Q_creator()
 * DESCRIPTION:	return the creator of the current object
 */
nomask string _Q_creator()
{
    return creator;
}

/*
 * NAME:	creator()
 * DESCRIPTION:	return the creator of this object
 */
static string creator(object obj)
{
    ARGCHECK(obj, creator, 1);

    return obj->_Q_creator();
}

/*
 * NAME:	init_creator()
 * DESCRIPTION:	set the creator of this object
 */
private void init_creator()
{
    string *path;

    path = explode(object_name(this_object()), "/");
    if (path[0] == "players") {
	creator = path[1];
    } else if (sscanf(object_name(this_object()), "%*s#") == 1 &&
	       previous_object() != 0) {
	creator = previous_object()->_Q_creator();
    }
}
