private int privileged;		/* privilege level of this object */

/*
 * NAME:	_Q_priv()
 * DESCRIPTION:	return the privilege level of this object
 */
nomask int _Q_priv()
{
    return privileged;
}
