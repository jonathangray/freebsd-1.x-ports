private int light;

/*
 * NAME:	_F_add_light()
 * DESCRIPTION:	add to the local light value of this object
 */
nomask int _F_add_light(int i)
{
    if (PRIVILEGED()) {
	light += i;
    }
    return light;
}

/*
 * NAME:	query_light()
 * DESCRIPTION:	return the local light value of an object
 */
private int query_light(object obj)
{
    return obj->_F_add_light(0);
}

/*
 * NAME:	add_light()
 * DESCRIPTION:	adjust the light value of this object, including environments
 */
private int add_light(int i)
{
    int total_light;
    object env;

    total_light = light += i;
    for (env = environment(); env != 0; env = environment(env)) {
	total_light = env->_F_add_light(i);
    }

    return total_light;
}

/*
 * NAME:	set_light()
 * DESCRIPTION:	set the light value of an object
 */
static int set_light(int i)
{
    lock(privileged = 1,
	 i = add_light(i),
	 privileged = 0);

    return i;
}
