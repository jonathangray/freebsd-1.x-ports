/*
 * NAME:	badarg()
 * DESCRIPTION:	cause a bad argument error
 */
private void badarg(string func, int arg)
{
    error("Bad argument " + arg + " to function " + func);
}

/*
 * NAME:	capitalize()
 * DESCRIPTION:	capitalize a string
 */
static string capitalize(string str)
{
    ARGCHECK(str, capitalize, 1);

    if (str[0] >= 'a' && str[0] <= 'z') {
	str[0] -= 'a' - 'A';
    }
    return str;
}

/*
 * NAME:	lower_case()
 * DESCRIPTION:	convert a string to lower case
 */
static string lower_case(string str)
{
    int i, len, c;

    ARGCHECK(str, lower_case, 1);

    for (i = 0, len = strlen(str); i < len; i++) {
	c = str[i];
	if (c >= 'A' && c <= 'Z') {
	    str[i] += 'a' - 'A';
	}
    }
    return str;
}

/*
 * NAME:	set_bit()
 * DESCRIPTION:	set a bit in a bit string
 */
static string set_bit(string str, int bit)
{
    int ind, len, c;

    ARGCHECK(str, set_bit, 1);

    if (bit > MAX_BITS) {
	error("Too big bit number " + bit);
    }
    ind = bit / 6;
    len = strlen(str);
    if (ind >= len) {
	do {
	    str += "                    ";
	    len += 20;
	} while (ind >= len);
	str = str[0 .. ind];
    }
    c = str[ind];
    if (c < ' ') {
	error("Illegal bit pattern in character " + ind);
    }
    str[ind] = (c - ' ' | 1 << bit % 6) + ' ';

    return str;
}

/*
 * NAME:	clear_bit()
 * DESCRIPTION:	clear a bit in a bit string
 */
static string clear_bit(string str, int bit)
{
    int ind, c;

    ARGCHECK(str, clear_bit, 1);

    if (bit > MAX_BITS) {
	error("Too big bit number " + bit);
    }
    ind = bit / 6;
    if (ind >= strlen(str)) {
	return str;
    }
    c = str[ind];
    if (c < ' ') {
	error("Illegal bit pattern in character " + ind);
    }
    str[ind] = (c - ' ' & ~(1 << bit % 6)) + ' ';

    return str;
}

/*
 * NAME:	test_bit()
 * DESCRIPTION:	test a bit in a bit string
 */
static int test_bit(string str, int bit)
{
    int ind;

    ARGCHECK(str, test_bit, 1);

    ind = bit / 6;
    if (ind >= strlen(str)) {
	return 0;
    }

    return (str[ind] - ' ' & 1 << bit % 6) != 0;
}

/*
 * NAME:	member_array()
 * DESCRIPTION:	return the index of the element in the array, or -1
 */
static int member_array(mixed elt, mixed *arr)
{
    int i, sz;

    ARGCHECK(arr, member_array, 1);

    for (i = 0, sz = sizeof(arr); i < sz; i++) {
	if (arr[i] == elt) {
	    return i;
	}
    }
    return -1;
}

/*
 * NAME:	filter_array
 * DESCRIPTION:	filter the elements of an array
 */
static varargs mixed *
filter_array(mixed *arr, mixed func, mixed obj, mixed arg)
{
    mixed *copy, elt;
    int i, j, sz;

    ARGCHECK(arr, filter_array, 1);
    if (closurep(func) && obj == 0 && arg == 0) {
	return relay("lambda_filter_array", arr, func);
    }
    ARGCHECK(stringp(func), filter_array, 2);
    if (stringp(obj)) {
	call_other(obj = DRIVER->path_object(obj), "???");
	obj = find_object(obj);
    }
    ARGCHECK(objectp(obj), filter_array, 3);

    copy = allocate(sz = sizeof(arr));
    for (i = 0, j = -1; i < sz; i++) {
	if (call_other(obj, func, elt = arr[i], arg)) {
	    copy[++j] = elt;
	}
    }

    return copy[0 .. j];
}

/*
 * NAME:	map_array
 * DESCRIPTION:	map the elements of an array
 */
static varargs mixed *map_array(mixed *arr, mixed func, mixed obj, mixed arg)
{
    mixed *copy;
    int i, sz;

    ARGCHECK(arr, map_array, 1);
    if (closurep(func) && obj == 0 && arg == 0) {
	return relay("lambda_map_array", arr, func);
    }
    ARGCHECK(stringp(func), map_array, 2);
    if (stringp(obj)) {
	call_other(obj = DRIVER->path_object(obj), "???");
	obj = find_object(obj);
    }
    ARGCHECK(objectp(obj), map_array, 3);

    copy = allocate(sz = sizeof(arr));
    for (i = 0; i < sz; i++) {
	copy[i] = call_other(obj, func, arr[i], arg);
    }

    return copy;
}

/*
 * NAME:	extract()
 * DESCRIPTION:	extract a substring
 */
static varargs string extract(string str, int first, int last)
{
    ARGCHECK(str, extract, 1);

    /* will mistake extract(str, 0) for extract(str, 0, 0) */
    if (first != 0 && last == 0) {
	last = strlen(str) - 1;
    } else if (first >= strlen(str) || last < first) {
	return "";
    }

    return str[first .. last];
}

/*
 * NAME:	slice_array()
 * DESCRIPTION:	return part on an array
 */
static mixed *slice_array(mixed *arr, int first, int last)
{
    ARGCHECK(arr, slice_array, 1);

    if (first >= sizeof(arr) || last < first) {
	return ({ });
    }
    return arr[first .. last];
}

/*
 * NAME:	sort_array()
 * DESCRIPTION:	sort an array
 */
static varargs mixed *sort_array(mixed *source, mixed func, mixed obj)
{
    int step, halfstep, size;
    int i, j, i1, i2, end1, end2;
    mixed *dest, *temp;

    ARGCHECK(source, sort_array, 1);
    if (closurep(func) && obj == 0) {
	return relay("lambda_sort_array", source, func);
    }
    ARGCHECK(stringp(func), sort_array, 2);
    if (obj == 0) {
	obj = this_object();
    } else if (stringp(obj)) {
	call_other(obj = DRIVER->path_object(obj), "???");
	obj = find_object(obj);
    }
    ARGCHECK(objectp(obj), sort_array, 3);

    size = sizeof(source);
    if (size < 2) {
        return source[..];
    }
    dest = allocate(size);
    step = 2;
    halfstep = 1;

    while (halfstep < size) {
        for (i = j = 0; i < size; i += step) {
            i1 = i;
            i2 = i + halfstep;
            end1 = i2;
            if (end1 > size) {
                end1 = size;
            }
            end2 = i + step;
            if (end2 > size) {
                end2 = size;
            }
            while (i1 < end1 && i2 < end2) {
                if (call_other(obj, func, source[i1], source[i2]) > 0) {
                    dest[j++] = source[i2++];
                } else {
                    dest[j++] = source[i1++];
                }
            }
            if (i1 == end1) {
                while (i2 < end2) {
                    dest[j++] = source[i2++];
                }
            } else {
                while (i1 < end1) {
                    dest[j++] = source[i1++];
                }
            }
        }
        halfstep = step;
        step += step;
        temp = source;
        source = dest;
        dest = temp;
    }
    return source;
}

/*
 * NAME:	unique_array()
 * DESCRIPTION:	subdevide an array of objects into arrays with objects where
 *		obj->func() returned identical descriptions
 */
static varargs mixed *unique_array(mixed *arr, string func, mixed exclude)
{
    mapping map;
    int i, sz;
    mixed val, elt;
    object *list;

    ARGCHECK(arr, unique_array, 1);
    ARGCHECK(func, unique_array, 2);

    map = ([ ]);
    for (i = 0, sz = sizeof(arr); i < sz; i++) {
	elt = arr[i];
	if (objectp(elt) && (val=call_other(elt, func)) != exclude) {
	    list = map[val];
	    if (list == 0) {
		list = ({ elt });
	    } else {
		list = ({ elt }) + list;
	    }
	    map[val] = list;
	}
    }
    return map_values(map);
}
