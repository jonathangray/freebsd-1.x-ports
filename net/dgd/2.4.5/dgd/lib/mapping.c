/*
 * NAME:	mkmapping()
 * DESCRIPTION:	create a mapping of two arrays
 */
static mapping mkmapping(mixed *indices, mixed *values)
{
    mapping map;
    int i, sz;

    ARGCHECK(indices, mkmapping, 1);
    ARGCHECK(values, mkmapping, 2);

    if (sizeof(indices) != sizeof(values)) {
	error("Unequal argument sizes in mkmapping()");
    }
    map = ([ ]);
    for (i = 0, sz = sizeof(indices); i < sz; i++) {
	map[indices[i]] = values[i];
    }

    return map;
}

/*
 * NAME:	m_indices()
 * DESCRIPTION:	return the indices of a mapping
 */
static mixed *m_indices(mapping map)
{
    ARGCHECK(map, m_indices, 1);
    return map_indices(map);
}

/*
 * NAME:	m_values()
 * DESCRIPTION:	return the values of a mapping
 */
static mixed *m_values(mapping map)
{
    ARGCHECK(map, m_values, 1);
    return map_values(map);
}

/*
 * NAME:	m_delete()
 * DESCRIPTION:	return a mapping with an element deleted
 */
static mapping m_delete(mapping map, mixed elt)
{
    ARGCHECK(map, m_delete, 1);

    map += ([ ]);
    map[elt] = 0;
    return map;
}

/*
 * NAME:	m_sizeof()
 * DESCRIPTION:	return the size of a mapping
 */
static int m_sizeof(mapping map)
{
    ARGCHECK(map, m_sizeof, 1);
    return map_sizeof(map);
}

/*
 * NAME:	filter_mapping()
 * DESCRIPTION:	filter a mapping by its values
 */
static varargs mapping
filter_mapping(mapping map, mixed func, mixed obj, mixed arg)
{
    mixed *indices, *values, value;
    mapping copy;
    int i, sz;

    ARGCHECK(map, filter_mapping, 1);
    if (closurep(func) && obj == 0 && arg == 0) {
	return relay("lambda_filter_mapping", map, func);
    }
    ARGCHECK(stringp(func), filter_mapping, 2);
    if (stringp(obj)) {
	call_other(obj = DRIVER->path_object(obj), "???");
	obj = find_object(obj);
    }
    ARGCHECK(objectp(obj), filter_mapping, 3);

    indices = map_indices(map);
    values = map_values(map);
    copy = ([ ]);
    for (i = 0, sz = sizeof(indices); i < sz; i++) {
	if (call_other(obj, func, value = values[i], arg)) {
	    copy[indices[i]] = value;
	}
    }

    return copy;
}

/*
 * NAME:	map_mapping()
 * DESCRIPTION:	map the values of a mapping
 */
static varargs mapping
map_mapping(mapping map, mixed func, mixed obj, mixed arg)
{
    mixed *indices, *values;
    mapping copy;
    int i, sz;

    ARGCHECK(map, map_mapping, 1);
    if (closurep(func) && obj == 0 && arg == 0) {
	return relay("lambda_map_mapping", map, func);
    }
    ARGCHECK(stringp(func), map_mapping, 2);
    if (stringp(obj)) {
	call_other(obj = DRIVER->path_object(obj), "???");
	obj = find_object(obj);
    }
    ARGCHECK(objectp(obj), map_mapping, 3);

    indices = map_indices(map);
    values = map_values(map);
    copy = ([ ]);
    for (i = 0, sz = sizeof(indices); i < sz; i++) {
	copy[indices[i]] = call_other(obj, func, values[i], arg);
    }

    return copy;
}
