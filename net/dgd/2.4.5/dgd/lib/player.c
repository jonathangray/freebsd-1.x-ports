# include <config.h>
# include "/dgd/lib/privilege.h"

private object user;		/* associated user object */

nomask void valid_player() {}

/*
 * NAME:	_F_user()
 * DESCRIPTION:	set the user object
 */
void _F_user(object obj)
{
    if (object_name(previous_object()) == DRIVER || PRIVILEGED()) {
	user = obj;
    }
}

/*
 * NAME:	_Q_user()
 * DESCRIPTION:	query the user object
 */
object _Q_user()
{
    return user;
}

/*
 * NAME:	catch_tell()
 * DESCRIPTION:	pass on a message to the user object
 */
nomask void catch_tell(string str)
{
    if (user != 0) {
	user->catch_tell(str);
    }
}
