/*
 * auto object for 2.4.5 mudlib
 */

# include <config.h>
# undef status
# include <status.h>
# include <limits.h>

# include "reset.h"
# include "privilege.h"
# include "creator.h"
# include "global.h"
# include "tool.h"
# include "light.h"
# include "living.h"
# include "inventory.h"
# include "interactive.h"
# include "file.h"
# include "mapping.h"
# include "call_out.h"
# include "simfun.h"

# include "reset.c"
# include "privilege.c"
# include "creator.c"
# include "global.c"
# include "tool.c"
# include "light.c"
# include "living.c"
# include "inventory.c"
# include "interactive.c"
# include "file.c"
# include "mapping.c"
# include "call_out.c"
# include "simfun.c"


private void initialize()
{
    INIT_PRIVILEGE();
    INIT_CREATOR();
    INIT_GLOBAL();
    INIT_TOOL();
    INIT_LIGHT();
    INIT_LIVING();
    INIT_INVENTORY();
    INIT_INTERACTIVE();
    INIT_FILE();
    INIT_MAPPING();
    INIT_CALL_OUT();
    INIT_SIMFUN();
}

/*
 * NAME:	_F_create()
 * DESCRIPTION:	initialize the object
 */
nomask void _F_create()
{
    lock(initialize());

    INIT_RESET();
}
