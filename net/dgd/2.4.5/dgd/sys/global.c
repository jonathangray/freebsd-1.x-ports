# include <config.h>
# include "/dgd/lib/privilege.h"
# undef status

# define C_CONTROL		 21

# define C_IF			  1
# define C_IFNOT		  2
# define C_LAND			  3
# define C_LOR			  4
# define C_WHILE		  5
# define C_DO			  6
# define C_PROGN		  7
# define C_ASSIGN		  8
# define C_PLUSEQ		  9
# define C_MINEQ		 10
# define C_MULTEQ		 11
# define C_DIVEQ		 12
# define C_MODEQ		 13
# define C_LSHIFTEQ		 14
# define C_RSHIFTEQ		 15
# define C_ANDEQ		 16
# define C_XOREQ		 17
# define C_OREQ			 18
# define C_PLUSPLUS		 19
# define C_MINMIN		 20
# define C_SSCANF		 21

# define C_INDEX		 22
# define C_RANGE		 23
# define C_CALL_OTHER		 24
# define C_NOT			 25
# define C_LNOT			 26
# define C_NEGATE		 27
# define C_MULT			 28
# define C_DIV			 29
# define C_MOD			 30
# define C_PLUS			 31
# define C_MIN			 32
# define C_LSHIFT		 33
# define C_RSHIFT		 34
# define C_LESS			 35
# define C_LESSEQ		 36
# define C_GREATER		 37
# define C_GREATEREQ		 38
# define C_EQ			 39
# define C_NOTEQ		 40
# define C_AND			 41
# define C_XOR			 42
# define C_OR			 43
# define C_QUOTE		 44

# define C_ADD_ACTION		 45
# define C_ADD_VERB		 46
# define C_ADD_WORTH		 47
# define C_ALL_INVENTORY	 48
# define C_ALLOCATE		 49
# define C_APPLY		 50
# define C_CALL_OUT		 51
# define C_CALL_TRACE		 52
# define C_CAPITALIZE		 53
# define C_CAT			 54
# define C_CINDENT		 55
# define C_CLEAR_BIT		 56
# define C_CLONE_OBJECT		 57
# define C_CLOSUREP		 58
# define C_COMMAND		 59
# define C_CREATE_WIZARD	 60
# define C_CREATOR		 61
# define C_CRYPT		 62
# define C_CTIME		 63
# define C_DEEP_INVENTORY	 64
# define C_DESTRUCT		 65
# define C_DISABLE_COMMANDS	 66
# define C_ENABLE_COMMANDS	 67
# define C_ENVIRONMENT		 68
# define C_ERROR		 69
# define C_EXEC			 70
# define C_EXPLODE		 71
# define C_FILE_NAME		 72
# define C_FILE_SIZE		 73
# define C_FILTER_ARRAY		 74
# define C_FILTER_MAPPING	 75
# define C_FIND_CALL_OUT	 76
# define C_FIND_LIVING		 77
# define C_FIND_OBJECT		 78
# define C_FIND_PLAYER		 79
# define C_FIRST_INVENTORY	 80
# define C_FUNCTION_EXISTS	 81
# define C_GET_DIR		 82
# define C_GET_EXEC_COST	 83
# define C_IMPLODE		 84
# define C_INTERACTIVE		 85
# define C_INTP			 86
# define C_LAMBDA		 87
# define C_LIVING		 88
# define C_LOCALCMD		 89
# define C_LOG_FILE		 90
# define C_LOWER_CASE		 91
# define C_LS			 92
# define C_M_DELETE		 93
# define C_M_INDICES		 94
# define C_M_SIZEOF		 95
# define C_M_VALUES		 96
# define C_MAP_ARRAY		 97
# define C_MAP_MAPPING		 98
# define C_MAPPINGP		 99
# define C_MEMBER_ARRAY		100
# define C_MKDIR		101
# define C_MKMAPPING		102
# define C_MOVE_OBJECT		103
# define C_NEXT_INVENTORY	104
# define C_NOTIFY_FAIL		105
# define C_OBJECTP		106
# define C_PARSE_STRING		107
# define C_POINTERP		108
# define C_PRESENT		109
# define C_PREVIOUS_OBJECT	110
# define C_QUERY_HOST_NAME	111
# define C_QUERY_IDLE		112
# define C_QUERY_IP_NAME	113
# define C_QUERY_IP_NUMBER	114
# define C_QUERY_LOAD_AVERAGE	115
# define C_QUERY_SNOOP		116
# define C_QUERY_VERB		117
# define C_RANDOM		118
# define C_READ_BYTES		119
# define C_READ_FILE		120
# define C_REMOVE_CALL_OUT	121
# define C_RENAME		122
# define C_RESTORE_OBJECT	123
# define C_RM			124
# define C_RMDIR		125
# define C_SAVE_OBJECT		126
# define C_SAY			127
# define C_SET_BIT		128
# define C_SET_HEART_BEAT	129
# define C_SET_LIVING_NAME	130
# define C_SET_THIS_PLAYER	131
# define C_SHOUT		132
# define C_SHUTDOWN		133
# define C_SIZEOF		134
# define C_SNOOP		135
# define C_SORT_ARRAY		136
# define C_STATUS		137
# define C_STRINGP		138
# define C_STRLEN		139
# define C_SWAPOUT		140
# define C_TAIL			141
# define C_TELL_OBJECT		142
# define C_TELL_ROOM		143
# define C_TEST_BIT		144
# define C_THIS_OBJECT		145
# define C_THIS_PLAYER		146
# define C_TIME			147
# define C_TRANSFER		148
# define C_UNIQUE_ARRAY		149
# define C_USERS		150
# define C_VERSION		151
# define C_WIZLIST		152
# define C_WRITE		153
# define C_WRITE_BYTES		154
# define C_WRITE_FILE		155


# define MINMAX(min, max)	((min) | ((max) << 8))
# define MIN(c)			(minmax[c - 1] & 0xff)
# define MAX(c)			(minmax[c - 1] >> 8)


object command_giver;		/* the current player */
string notify_mesg;		/* current notify_fail mesg */
string verb;			/* the current verb */
mapping players;		/* player mapping */
mapping monsters;		/* monster mapping */
mapping objects;		/* objects mapping */
mapping heart_beats;		/* heart_beats mapping */
mapping call_outs;		/* call_outs mapping */
mapping codes;			/* function to code */
int *minmax;			/* min & max # of arguments for functions */

static void reset(int arg)
{
    int i;

    if (players == 0) {
	players = ([ ]);
	monsters = ([ ]);
	heart_beats = ([ ]);
	call_outs = ([ ]);

	codes = ([
	    "?" :		C_IF,
	    "?!" :		C_IFNOT,
	    "&&" :		C_LAND,
	    "||" :		C_LOR,
	    "while" :		C_WHILE,
	    "do" :		C_DO,
	    "," :		C_PROGN,
	    "=" :		C_ASSIGN,
	    "+=" :		C_PLUSEQ,
	    "-=" :		C_MINEQ,
	    "*=" :		C_MULTEQ,
	    "/=" :		C_DIVEQ,
	    "%=" :		C_MODEQ,
	    "<<=" :		C_LSHIFTEQ,
	    ">>=" :		C_RSHIFTEQ,
	    "&=" :		C_ANDEQ,
	    "^=" :		C_XOREQ,
	    "|=" :		C_OREQ,
	    "++" :		C_PLUSPLUS,
	    "--" :		C_MINMIN,
	    "sscanf" :		C_SSCANF,

	    "[" :		C_INDEX,
	    "[.." :		C_RANGE,
	    "->" :		C_CALL_OTHER,
	    "~" :		C_NOT,
	    "!" :		C_LNOT,
	    "negate" :		C_NEGATE,
	    "*" :		C_MULT,
	    "/" :		C_DIV,
	    "%" :		C_MOD,
	    "+" :		C_PLUS,
	    "-" :		C_MIN,
	    "<<" :		C_LSHIFT,
	    ">>" :		C_RSHIFT,
	    "<" :		C_LESS,
	    "<=" :		C_LESSEQ,
	    ">" :		C_GREATER,
	    ">=" :		C_GREATEREQ,
	    "==" :		C_EQ,
	    "!=" :		C_NOTEQ,
	    "&" :		C_AND,
	    "^" :		C_XOR,
	    "|" :		C_OR,
	    "'" :		C_QUOTE,

	    "add_action" :	C_ADD_ACTION,
	    "add_verb" :	C_ADD_VERB,
	    "add_worth" :	C_ADD_WORTH,
	    "all_inventory" :	C_ALL_INVENTORY,
	    "allocate" :	C_ALLOCATE,
	    "apply" :		C_APPLY,
	    "call_other" :	C_CALL_OTHER,
	    "call_out" :	C_CALL_OUT,
	    "call_trace" :	C_CALL_TRACE,
	    "capitalize" :	C_CAPITALIZE,
	    "cat" :		C_CAT,
	    "cindent" :		C_CINDENT,
	    "clear_bit" :	C_CLEAR_BIT,
	    "clone_object" :	C_CLONE_OBJECT,
	    "closurep" :	C_CLOSUREP,
	    "command" :		C_COMMAND,
	    "create_wizard" :	C_CREATE_WIZARD,
	    "creator" :		C_CREATOR,
	    "crypt" :		C_CRYPT,
	    "ctime" :		C_CTIME,
	    "deep_inventory" :	C_DEEP_INVENTORY,
	    "destruct" :	C_DESTRUCT,
	    "disable_commands":	C_DISABLE_COMMANDS,
	    "enable_commands" :	C_ENABLE_COMMANDS,
	    "environment" :	C_ENVIRONMENT,
	    "error" :		C_ERROR,
	    "exec" :		C_EXEC,
	    "explode" :		C_EXPLODE,
	    "file_name" :	C_FILE_NAME,
	    "file_size" :	C_FILE_SIZE,
	    "filter_array" :	C_FILTER_ARRAY,
	    "filter_mapping" :	C_FILTER_MAPPING,
	    "find_call_out" :	C_FIND_CALL_OUT,
	    "find_living" :	C_FIND_LIVING,
	    "find_object" :	C_FIND_OBJECT,
	    "find_player" :	C_FIND_PLAYER,
	    "first_inventory" :	C_FIRST_INVENTORY,
	    "function_exists" :	C_FUNCTION_EXISTS,
	    "get_dir" :		C_GET_DIR,
	    "get_exec_cost" :	C_GET_EXEC_COST,
	    "implode" :		C_IMPLODE,
	    "interactive" :	C_INTERACTIVE,
	    "intp" :		C_INTP,
	    "lambda" :		C_LAMBDA,
	    "living" :		C_LIVING,
	    "localcmd" :	C_LOCALCMD,
	    "log_file" :	C_LOG_FILE,
	    "lower_case" :	C_LOWER_CASE,
	    "ls" :		C_LS,
	    "m_delete" :	C_M_DELETE,
	    "m_indices" :	C_M_INDICES,
	    "m_sizeof" :	C_M_SIZEOF,
	    "m_values" :	C_M_VALUES,
	    "map_array" :	C_MAP_ARRAY,
	    "map_mapping" :	C_MAP_MAPPING,
	    "mappingp" :	C_MAPPINGP,
	    "member_array" :	C_MEMBER_ARRAY,
	    "mkdir" :		C_MKDIR,
	    "mkmapping" :	C_MKMAPPING,
	    "move_object" :	C_MOVE_OBJECT,
	    "next_inventory" :	C_NEXT_INVENTORY,
	    "notify_fail" :	C_NOTIFY_FAIL,
	    "objectp" :		C_OBJECTP,
	    "parse_string" :	C_PARSE_STRING,
	    "pointerp" :	C_POINTERP,
	    "present" :		C_PRESENT,
	    "previous_object" :	C_PREVIOUS_OBJECT,
	    "query_host_name" :	C_QUERY_HOST_NAME,
	    "query_idle" :	C_QUERY_IDLE,
	    "query_ip_name" :	C_QUERY_IP_NAME,
	    "query_ip_number" :	C_QUERY_IP_NUMBER,
	    "query_load_average":C_QUERY_LOAD_AVERAGE,
	    "query_snoop" :	C_QUERY_SNOOP,
	    "query_verb" :	C_QUERY_VERB,
	    "random" :		C_RANDOM,
	    "read_bytes" :	C_READ_BYTES,
	    "read_file" :	C_READ_FILE,
	    "remove_call_out" :	C_REMOVE_CALL_OUT,
	    "rename" :		C_RENAME,
	    "restore_object" :	C_RESTORE_OBJECT,
	    "rm" :		C_RM,
	    "rmdir" :		C_RMDIR,
	    "save_object" :	C_SAVE_OBJECT,
	    "say" :		C_SAY,
	    "set_bit" :		C_SET_BIT,
	    "set_heart_beat" :	C_SET_HEART_BEAT,
	    "set_living_name" :	C_SET_LIVING_NAME,
	    "set_this_player" :	C_SET_THIS_PLAYER,
	    "shout" :		C_SHOUT,
	    "shutdown" :	C_SHUTDOWN,
	    "sizeof" :		C_SIZEOF,
	    "snoop" :		C_SNOOP,
	    "sort_array" :	C_SORT_ARRAY,
	    "status" :		C_STATUS,
	    "stringp" :		C_STRINGP,
	    "strlen" :		C_STRLEN,
	    "swapout" :		C_SWAPOUT,
	    "tail" :		C_TAIL,
	    "tell_object" :	C_TELL_OBJECT,
	    "tell_room" :	C_TELL_ROOM,
	    "test_bit" :	C_TEST_BIT,
	    "this_object" :	C_THIS_OBJECT,
	    "this_player" :	C_THIS_PLAYER,
	    "time" :		C_TIME,
	    "transfer" :	C_TRANSFER,
	    "unique_array" :	C_UNIQUE_ARRAY,
	    "users" :		C_USERS,
	    "version" :		C_VERSION,
	    "wizlist" :		C_WIZLIST,
	    "write" :		C_WRITE,
	    "write_bytes" :	C_WRITE_BYTES,
	    "write_file" :	C_WRITE_FILE,
	]);

	minmax = ({
	    MINMAX(1, 255),	/* IF */
	    MINMAX(1, 255),	/* IFNOT */
	    MINMAX(2, 2),	/* LAND */
	    MINMAX(2, 2),	/* LOR */
	    MINMAX(3, 255),	/* WHILE */
	    MINMAX(3, 255),	/* DO */
	    MINMAX(2, 255),	/* PROGN */
	    MINMAX(2, 254),	/* ASSIGN */
	    MINMAX(2, 254),	/* PLUSEQ */
	    MINMAX(2, 254),	/* MINEQ */
	    MINMAX(2, 254),	/* MULTEQ */
	    MINMAX(2, 254),	/* DIVEQ */
	    MINMAX(2, 254),	/* MODEQ */
	    MINMAX(2, 254),	/* LSHIFTEQ */
	    MINMAX(2, 254),	/* RSHIFTEQ */
	    MINMAX(2, 254),	/* ANDEQ */
	    MINMAX(2, 254),	/* XOREQ */
	    MINMAX(2, 254),	/* OREQ */
	    MINMAX(1, 255),	/* PLUSPLUS */
	    MINMAX(1, 255),	/* MINMIN */
	    MINMAX(2, 255),	/* SSCANF */

	    MINMAX(2, 2),	/* INDEX */
	    MINMAX(3, 3),	/* RANGE */
	    MINMAX(2, 255),	/* CALL_OTHER */
	    MINMAX(1, 1),	/* NOT */
	    MINMAX(1, 1),	/* LNOT */
	    MINMAX(1, 1),	/* NEGATE */
	    MINMAX(2, 2),	/* MULT */
	    MINMAX(2, 2),	/* DIV */
	    MINMAX(2, 2),	/* MOD */
	    MINMAX(2, 2),	/* PLUS */
	    MINMAX(2, 2),	/* MIN */
	    MINMAX(2, 2),	/* LSHIFT */
	    MINMAX(2, 2),	/* RSHIFT */
	    MINMAX(2, 2),	/* LESS */
	    MINMAX(2, 2),	/* LESSEQ */
	    MINMAX(2, 2),	/* GREATER */
	    MINMAX(2, 2),	/* GREATEREQ */
	    MINMAX(2, 2),	/* EQ */
	    MINMAX(2, 2),	/* NOTEQ */
	    MINMAX(2, 2),	/* AND */
	    MINMAX(2, 2),	/* XOR */
	    MINMAX(2, 2),	/* OR */
	    MINMAX(1, 1),	/* QUOTE */

	    MINMAX(1, 3),	/* ADD_ACTION */
	    MINMAX(1, 1),	/* ADD_VERB */
	    MINMAX(1, 2),	/* ADD_WORTH */
	    MINMAX(0, 1),	/* ALL_INVENTORY */
	    MINMAX(1, 1),	/* ALLOCATE */
	    MINMAX(1, 255),	/* APPLY */
	    MINMAX(2, 255),	/* CALL_OUT */
	    MINMAX(0, 0),	/* CALL_TRACE */
	    MINMAX(1, 1),	/* CAPITALIZE */
	    MINMAX(1, 3),	/* CAT */
	    MINMAX(1, 1),	/* CINDENT */
	    MINMAX(2, 2),	/* CLEAR_BIT */
	    MINMAX(1, 1),	/* CLONE_OBJECT */
	    MINMAX(1, 1),	/* CLOSUREP */
	    MINMAX(1, 2),	/* COMMAND */
	    MINMAX(1, 2),	/* CREATE_WIZARD */
	    MINMAX(1, 1),	/* CREATOR */
	    MINMAX(1, 2),	/* CRYPT */
	    MINMAX(1, 1),	/* CTIME */
	    MINMAX(1, 1),	/* DEEP_INVENTORY */
	    MINMAX(1, 1),	/* DESTRUCT */
	    MINMAX(0, 0),	/* DISABLE_COMMANDS */
	    MINMAX(0, 0),	/* ENABLE_COMMANDS */
	    MINMAX(0, 1),	/* ENVIRONMENT */
	    MINMAX(1, 1),	/* ERROR */
	    MINMAX(2, 2),	/* EXEC */
	    MINMAX(2, 2),	/* EXPLODE */
	    MINMAX(1, 1),	/* FILE_NAME */
	    MINMAX(1, 1),	/* FILE_SIZE */
	    MINMAX(3, 4),	/* FILTER_ARRAY */
	    MINMAX(3, 4),	/* FILTER_MAPPING */
	    MINMAX(1, 1),	/* FIND_CALL_OUT */
	    MINMAX(1, 1),	/* FIND_LIVING */
	    MINMAX(1, 1),	/* FIND_OBJECT */
	    MINMAX(1, 1),	/* FIND_PLAYER */
	    MINMAX(0, 1),	/* FIRST_INVENTORY */
	    MINMAX(2, 2),	/* FUNCTION_EXISTS */
	    MINMAX(1, 1),	/* GET_DIR */
	    MINMAX(0, 0),	/* GET_EXEC_COST */
	    MINMAX(2, 2),	/* IMPLODE */
	    MINMAX(0, 1),	/* INTERACTIVE */
	    MINMAX(1, 1),	/* INTP */
	    MINMAX(1, 2),	/* LAMBDA */
	    MINMAX(1, 1),	/* LIVING */
	    MINMAX(0, 0),	/* LOCALCMD */
	    MINMAX(2, 2),	/* LOG_FILE */
	    MINMAX(1, 1),	/* LOWER_CASE */
	    MINMAX(1, 1),	/* LS */
	    MINMAX(2, 2),	/* M_DELETE */
	    MINMAX(1, 1),	/* M_INDICES */
	    MINMAX(1, 1),	/* M_SIZEOF */
	    MINMAX(1, 1),	/* M_VALUES */
	    MINMAX(3, 4),	/* MAP_ARRAY */
	    MINMAX(3, 4),	/* MAP_MAPPING */
	    MINMAX(1, 1),	/* MAPPINGP */
	    MINMAX(2, 2),	/* MEMBER_ARRAY */
	    MINMAX(1, 1),	/* MKDIR */
	    MINMAX(2, 2),	/* MKMAPPING */
	    MINMAX(2, 2),	/* MOVE_OBJECT */
	    MINMAX(0, 1),	/* NEXT_INVENTORY */
	    MINMAX(1, 1),	/* NOTIFY_FAIL */
	    MINMAX(1, 1),	/* OBJECTP */
	    MINMAX(2, 2),	/* PARSE_STRING */
	    MINMAX(1, 1),	/* POINTERP */
	    MINMAX(1, 2),	/* PRESENT */
	    MINMAX(0, 0),	/* PREVIOUS_OBJECT */
	    MINMAX(0, 0),	/* QUERY_HOST_NAME */
	    MINMAX(1, 1),	/* QUERY_IDLE */
	    MINMAX(0, 1),	/* QUERY_IP_NAME */
	    MINMAX(0, 1),	/* QUERY_IP_NUMBER */
	    MINMAX(0, 0),	/* QUERY_LOAD_AVERAGE */
	    MINMAX(1, 1),	/* QUERY_SNOOP */
	    MINMAX(0, 0),	/* QUERY_VERB */
	    MINMAX(1, 1),	/* RANDOM */
	    MINMAX(1, 3),	/* READ_BYTES */
	    MINMAX(1, 3),	/* READ_FILE */
	    MINMAX(1, 1),	/* REMOVE_CALL_OUT */
	    MINMAX(2, 2),	/* RENAME */
	    MINMAX(1, 1),	/* RESTORE_OBJECT */
	    MINMAX(1, 1),	/* RM */
	    MINMAX(1, 1),	/* RMDIR */
	    MINMAX(1, 1),	/* SAVE_OBJECT */
	    MINMAX(1, 2),	/* SAY */
	    MINMAX(2, 2),	/* SET_BIT */
	    MINMAX(1, 1),	/* SET_HEART_BEAT */
	    MINMAX(1, 1),	/* SET_LIVING_NAME */
	    MINMAX(1, 1),	/* SET_THIS_PLAYER */
	    MINMAX(1, 1),	/* SHOUT */
	    MINMAX(0, 0),	/* SHUTDOWN */
	    MINMAX(1, 1),	/* SIZEOF */
	    MINMAX(1, 1),	/* SNOOP */
	    MINMAX(2, 3),	/* SORT_ARRAY */
	    MINMAX(0, 1),	/* STATUS */
	    MINMAX(1, 1),	/* STRINGP */
	    MINMAX(1, 1),	/* STRLEN */
	    MINMAX(0, 0),	/* SWAPOUT */
	    MINMAX(1, 1),	/* TAIL */
	    MINMAX(2, 2),	/* TELL_OBJECT */
	    MINMAX(2, 3),	/* TELL_ROOM */
	    MINMAX(2, 2),	/* TEST_BIT */
	    MINMAX(0, 0),	/* THIS_OBJECT */
	    MINMAX(0, 1),	/* THIS_PLAYER */
	    MINMAX(0, 0),	/* TIME */
	    MINMAX(2, 2),	/* TRANSFER */
	    MINMAX(2, 3),	/* UNIQUE_ARRAY */
	    MINMAX(0, 0),	/* USERS */
	    MINMAX(0, 0),	/* VERSION */
	    MINMAX(0, 1),	/* WIZLIST */
	    MINMAX(1, 1),	/* WRITE */
	    MINMAX(3, 3),	/* WRITE_BYTES */
	    MINMAX(2, 2),	/* WRITE_FILE */
	});
    }
}

/*
 * NAME:	set_this_player()
 * DESCRIPTION:	set the current player
 */
void set_this_player(object player)
{
    command_giver = player;
}

/*
 * NAME:	query_this_player()
 * DESCRIPTION:	return the current player, or interactive user
 */
object query_this_player()
{
    return command_giver;
}

/*
 * NAME:	set_verb()
 * DESCRIPTION:	set the current verb
 */
void set_verb(string str)
{
    if (PRIVILEGED()) {
	verb = str;
    }
}

/*
 * NAME:	query_verb()
 * DESCRIPTION:	return the current verb
 */
string query_verb()
{
    return verb;
}

/*
 * NAME:	set_living_name()
 * DESCRIPTION:	set the living name of the current object
 */
void set_living_name(string name)
{
    object obj, *list;

    if (PRIVILEGED()) {
	obj = previous_object();
	if (living(obj)) {
	    if (interactive(obj) != 0) {
		players[name] = obj;
	    } else {
		list = monsters[name];
		if (list == 0) {
		    list = ({ obj });
		} else {
		    list = ({ obj }) + (list - ({ 0 }));
		}
		monsters[name] = list;
	    }
	}
    }
}

/*
 * NAME:	query_find_player()
 * DESCRIPTION:	find a player by name
 */
object query_find_player(string name)
{
    object obj;

    obj = players[name];
    if (obj != 0 && living(obj)) {
	return obj;
    }
    return 0;
}

/*
 * NAME:	query_find_living()
 * DESCRIPTION:	find a living object by name
 */
object query_find_living(string name)
{
    object obj, *list;
    int i, sz;

    obj = query_find_player(name);
    if (obj != 0) {
	return obj;
    }
    list = monsters[name];
    if (list != 0 && sizeof(list) != 0) {
	list -= ({ 0 });
	monsters[name] = list;
	for (i = 0, sz = sizeof(list); i < sz; i++) {
	    if (living(obj=list[i])) {
		return obj;
	    }
	}
    }
    return 0;
}

/*
 * NAME:	add_object()
 * DESCRIPTION:	add an object
 */
void add_object()
{
    if (PRIVILEGED()) {
	if (objects == 0) {
	    objects = ([ ]);
	}
	objects[creator(previous_object())]++;
    }
}

/*
 * NAME:	del_object()
 * DESCRIPTION:	delete an object
 */
void del_object()
{
    if (PRIVILEGED()) {
	objects[creator(previous_object())]--;
    }
}

/*
 * NAME:	add_call_out()
 * DESCRIPTION:	add a call_out
 */
void add_call_out()
{
    if (PRIVILEGED()) {
	call_outs[creator(previous_object())]++;
    }
}

/*
 * NAME:	del_call_out()
 * DESCRIPTION:	remove a call_out
 */
void del_call_out()
{
    if (PRIVILEGED()) {
	call_outs[creator(previous_object())]--;
    }
}

/*
 * NAME:	add_heart_beat()
 * DESCRIPTION:	add a heart_beat
 */
void add_heart_beat()
{
    if (PRIVILEGED()) {
	heart_beats[creator(previous_object())]++;
    }
}

/*
 * NAME:	del_heart_beat()
 * DESCRIPTION:	delete a heart_beat
 */
void del_heart_beat()
{
    if (PRIVILEGED()) {
	heart_beats[creator(previous_object())]--;
    }
}

/*
 * NAME:	show_wiz_list()
 * DESCRIPTION:	show the wizlist
 */
void show_wiz_list()
{
    string *wizards, name;
    int i, sz;

    write("name\t      objects hbeats callouts\n" +
	  "-------------------------------------\n");
    wizards = map_indices(objects);
    for (i = 0, sz = sizeof(wizards); i < sz; i++) {
	if (wizards[i] == 0) {
	    name = "Mudlib";
	} else {
	    name = wizards[i];
	}
	if (strlen(name) < 8) {
	    name += "\t";
	}
	write(name + "\t" + objects[wizards[i]] + "\t" +
	      heart_beats[wizards[i]] + "\t" + call_outs[wizards[i]] + "\n");
    }
}

/*
 * NAME:	ident()
 * DESCRIPTION:	return 1 if the argument is an identifier, 0 otherwise
 */
private int ident(mixed arg)
{
    int i, len, c;

    if (!stringp(arg)) {
	return 0;
    }
    len = strlen(arg);
    if (len == 0) {
	return 0;
    }
    c = arg[0];
    if ((c < 'a' || c > 'z') && (c < 'A' || c > 'Z') && c != '_') {
	return 0;
    }
    for (i = 1; i < len; i++) {
	c = arg[i];
	if ((c < 'a' || c > 'z') && (c < 'A' || c > 'Z') && c != '_' &&
	    (c < '0' || c > '9')) {
	    return 0;
	}
    }
    return 1;
}

/*
 * NAME:	eval()
 * DESCRIPTION:	evaluate an expression
 */
mapping eval(mixed expr, mapping vars)
{
    mixed code, *args, cond, body;
    int nargs, i;
    mapping old;

    if (stringp(expr)) {
	/* possibly a variable */
	vars[1] = (strlen(expr) > 1 && expr[0] == '\'') ? vars[expr] : expr;
	return vars;
    } else if (!arrayp(expr) || sizeof(expr) == 0 || !closurep(expr[0])) {
	/* ordinary expression */
	vars[1] = expr;
	return vars;
    }

    /* closure */
    args = expr[1 ..];
    expr = expr[0];
    code = expr[1];
    if (sizeof(expr) == 2) {
	/*
	 * short format
	 */
	if ((nargs=sizeof(args)) < MIN(code)) {
	    error("Too few arguments for closure");
	} else if (nargs > MAX(code)) {
	    error("Too many arguments for closure");
	}
    } else {
	/*
	 * long format
	 */
	nargs = expr[2];
	if (nargs != sizeof(args)) {
	    if (nargs < sizeof(args)) {
		error("Too many arguments for closure");
	    } else {
		error("Too few arguments for closure");
	    }
	}

	/* bind parameters */
	body = ([ ]);
	for (i = 0; i < nargs; i++) {
	    body[expr[i + 3]] = (vars=eval(args[i], vars))[1];
	}
	old = vars & expr[3 .. nargs + 2];	/* old parameter values */
	vars += body;				/* use new parameter values */
	args = expr[nargs + 3 ..];		/* closure arguments */
	expr = expr[3 .. nargs + 2];		/* parameter names */
	nargs = sizeof(args);

	/* deal with special codes */
	switch (code) {
	case C_IF:
	    for (i = 1; i < nargs; i += 2) {
		if ((vars=eval(args[i - 1], vars))[1]) {
		    return (eval(args[i], vars) - expr) + old;
		}
	    }
	    return (eval(args[i - 1], vars) - expr) + old;

	case C_IFNOT:
	    for (i = 1; i < nargs; i += 2) {
		if (!(vars=eval(args[i - 1], vars))[1]) {
		    return (eval(args[i], vars) - expr) + old;
		}
	    }
	    return (eval(args[i - 1], vars) - expr) + old;

	case C_LAND:
	    if (!(vars=eval(args[0], vars))[1]) {
		vars[1] = 0;
		return (vars - expr) + old;
	    }
	    vars = eval(args[1], vars);
	    vars[1] = !!vars[1];
	    return (vars - expr) + old;

	case C_LOR:
	    if ((vars=eval(args[0], vars))[1]) {
		vars[1] = 1;
		return (vars - expr) + old;
	    }
	    vars = eval(args[1], vars);
	    vars[1] = !!vars[1];
	    return (vars - expr) + old;

	case C_WHILE:
	    cond = args[0];
	    body = args[2];
	    while ((vars=eval(cond, vars))[1]) {
		vars = eval(body, vars);
	    }
	    return (eval(args[1], vars) - expr) + old;

	case C_DO:
	    body = args[0];
	    cond = args[1];
	    do {
		vars = eval(body, vars);
	    } while ((vars=eval(cond, vars))[1]);
	    return (eval(args[2], vars) - expr) + old;

	case C_ASSIGN:
	    for (i = 0; i < nargs; i += 2) {
		vars = eval(args[i + 1], vars);
		vars[args[i]] = vars[1];
	    }
	    return (vars - expr) + old;

	case C_PLUSEQ:
	    for (i = 0; i < nargs; i += 2) {
		vars = eval(args[i + 1], vars);
		vars[args[i]] += vars[1];
	    }
	    vars[1] = vars[args[i - 2]];
	    return (vars - expr) + old;

	case C_MINEQ:
	    for (i = 0; i < nargs; i += 2) {
		vars = eval(args[i + 1], vars);
		vars[args[i]] -= vars[1];
	    }
	    vars[1] = vars[args[i - 2]];
	    return (vars - expr) + old;

	case C_MULTEQ:
	    for (i = 0; i < nargs; i += 2) {
		vars = eval(args[i + 1], vars);
		vars[args[i]] *= vars[1];
	    }
	    vars[1] = vars[args[i - 2]];
	    return (vars - expr) + old;

	case C_DIVEQ:
	    for (i = 0; i < nargs; i += 2) {
		vars = eval(args[i + 1], vars);
		vars[args[i]] /= vars[1];
	    }
	    vars[1] = vars[args[i - 2]];
	    return (vars - expr) + old;

	case C_MODEQ:
	    for (i = 0; i < nargs; i += 2) {
		vars = eval(args[i + 1], vars);
		vars[args[i]] %= vars[1];
	    }
	    vars[1] = vars[args[i - 2]];
	    return (vars - expr) + old;

	case C_LSHIFTEQ:
	    for (i = 0; i < nargs; i += 2) {
		vars = eval(args[i + 1], vars);
		vars[args[i]] <<= vars[1];
	    }
	    vars[1] = vars[args[i - 2]];
	    return (vars - expr) + old;

	case C_RSHIFTEQ:
	    for (i = 0; i < nargs; i += 2) {
		vars = eval(args[i + 1], vars);
		vars[args[i]] >>= vars[1];
	    }
	    vars[1] = vars[args[i - 2]];
	    return (vars - expr) + old;

	case C_ANDEQ:
	    for (i = 0; i < nargs; i += 2) {
		vars = eval(args[i + 1], vars);
		vars[args[i]] &= vars[1];
	    }
	    vars[1] = vars[args[i - 2]];
	    return (vars - expr) + old;

	case C_XOREQ:
	    for (i = 0; i < nargs; i += 2) {
		vars = eval(args[i + 1], vars);
		vars[args[i]] ^= vars[1];
	    }
	    vars[1] = vars[args[i - 2]];
	    return (vars - expr) + old;

	case C_OREQ:
	    for (i = 0; i < nargs; i += 2) {
		vars = eval(args[i + 1], vars);
		vars[args[i]] |= vars[1];
	    }
	    vars[1] = vars[args[i - 2]];
	    return (vars - expr) + old;

	case C_PLUSPLUS:
	    for (i = nargs; i > 0; ) {
		vars[args[--i]]++;
	    }
	    vars[1] = vars[args[nargs - 1]];
	    return (vars - expr) + old;

	case C_MINMIN:
	    for (i = nargs; i > 0; ) {
		--vars[args[--i]];
	    }
	    vars[1] = vars[args[nargs - 1]];
	    return (vars - expr) + old;

	case C_SSCANF:
	    args[0] = (vars=eval(args[0], vars))[1];
	    args[1] = (vars=eval(args[1], vars))[1];
	    body = args[2 ..];
	    for (i = vars[1] = sscanf(args...); i > 0; ) {
		--i;
		vars[body[i]] = args[i + 2];
	    }
	    return (vars - expr) + old;
	}

	/*
	 * not a special code: evaluate all arguments
	 */
	for (i = 0; i < nargs; i++) {
	    args[i] = (vars=eval(args[i], vars))[1];
	}
	vars = (vars - expr) + old;	/* remove parameters, add old values */
    }

    if (stringp(code)) {
	/* local function */
	vars[1] = call_other(previous_object(), code, args...);
	return vars;
    }

    vars[1] = 0;
    switch (code) {
    case C_PROGN:		vars[1] = args[nargs - 1];
				break;

    case C_INDEX:		vars[1] = args[0][args[1]];
				break;
    case C_RANGE:		vars[1] = args[0][args[1] .. args[2]];
				break;
    case C_CALL_OTHER:		vars[1] = call_other(args...);
				break;
    case C_NOT:			vars[1] = ~args[0];
				break;
    case C_LNOT:		vars[1] = !args[0];
				break;
    case C_NEGATE:		vars[1] = -args[0];
				break;
    case C_MULT:		vars[1] = args[0] * args[1];
				break;
    case C_DIV:			vars[1] = args[0] / args[1];
				break;
    case C_MOD:			vars[1] = args[0] % args[1];
				break;
    case C_PLUS:		vars[1] = args[0] + args[1];
				break;
    case C_MIN:			vars[1] = args[0] - args[1];
				break;
    case C_LSHIFT:		vars[1] = args[0] << args[1];
				break;
    case C_RSHIFT:		vars[1] = args[0] >> args[1];
				break;
    case C_LESS:		vars[1] = args[0] < args[1];
				break;
    case C_LESSEQ:		vars[1] = args[0] <= args[1];
				break;
    case C_GREATER:		vars[1] = args[0] > args[1];
				break;
    case C_GREATEREQ:		vars[1] = args[0] >= args[1];
				break;
    case C_EQ:			vars[1] = args[0] == args[1];
				break;
    case C_NOTEQ:		vars[1] = args[0] != args[1];
				break;
    case C_AND:			vars[1] = args[0] & args[1];
				break;
    case C_XOR:			vars[1] = args[0] ^ args[1];
				break;
    case C_OR:			vars[1] = args[0] | args[1];
				break;
    case C_QUOTE:		vars[1] = args[0];
				break;

    case C_ADD_ACTION:		add_action(args...);
				break;
    case C_ADD_VERB:		add_verb(args[0]);
				break;
    case C_ADD_WORTH:		if (nargs == 1) {
				    add_worth(args[0], previous_object(1));
				} else {
				    add_worth(args[0], args[1]);
				}
				break;
    case C_ALL_INVENTORY:	if (nargs == 0) {
				    vars[1] = all_inventory(previous_object());
				} else {
				    vars[1] = all_inventory(args[0]);
				}
				break;
    case C_ALLOCATE:		vars[1] = allocate(args[0]);
				break;
    case C_APPLY:		vars[1] = eval(args, vars);
				break;
    case C_CALL_OUT:		call_out(args...);
				break;
    case C_CALL_TRACE:		vars[1] = call_trace();
				break;
    case C_CAPITALIZE:		vars[1] = capitalize(args[0]);
				break;
    case C_CAT:			vars[1] = cat(args...);
				break;
    case C_CINDENT:		vars[1] = cindent(args[0]);
				break;
    case C_CLEAR_BIT:		vars[1] = clear_bit(args[0], args[1]);
				break;
    case C_CLONE_OBJECT:	vars[1] = clone_object(args[0]);
				break;
    case C_CLOSUREP:		vars[1] = closurep(args[0]);
				break;
    case C_COMMAND:		if (nargs == 1) {
				    vars[1] = command(args[0],
						      previous_object());
				} else {
				    vars[1] = command(args[0], args[1]);
				}
				break;
    case C_CREATE_WIZARD:	vars[1] = create_wizard(args...);
				break;
    case C_CREATOR:		vars[1] = creator(args[0]);
				break;
    case C_CRYPT:		vars[1] = crypt(args...);
				break;
    case C_CTIME:		vars[1] = ctime(args[0]);
				break;
    case C_DEEP_INVENTORY:	vars[1] = deep_inventory(args[0]);
				break;
    case C_DESTRUCT:		destruct(args[0]);
				break;
    case C_DISABLE_COMMANDS:	disable_commands();
				break;
    case C_ENABLE_COMMANDS:	enable_commands();
				break;
    case C_ENVIRONMENT:		if (nargs == 0) {
				    vars[1] = environment(previous_object());
				} else {
				    vars[1] = environment(args[0]);
				}
				break;
    case C_ERROR:		error(args[0]);
    case C_EXEC:		vars[1] = exec(args[0], args[1]);
				break;
    case C_EXPLODE:		vars[1] = explode(args[0], args[1]);
				break;
    case C_FILE_NAME:		vars[1] = file_name(args[0]);
				break;
    case C_FILE_SIZE:		vars[1] = file_size(args[0]);
				break;
    case C_FILTER_ARRAY:	vars[1] = filter_array(args...);
				break;
    case C_FILTER_MAPPING:	vars[1] = filter_mapping(args...);
				break;
    case C_FIND_CALL_OUT:	vars[1] = find_call_out(args[0]);
				break;
    case C_FIND_LIVING:		vars[1] = find_living(args[0]);
				break;
    case C_FIND_OBJECT:		vars[1] = find_object(args[0]);
				break;
    case C_FIND_PLAYER:		vars[1] = find_player(args[0]);
				break;
    case C_FIRST_INVENTORY:	if (nargs == 0) {
				   vars[1] = first_inventory(previous_object());
				} else {
				    vars[1] = first_inventory(args[0]);
				}
				break;
    case C_FUNCTION_EXISTS:	vars[1] = function_exists(args[0], args[1]);
				break;
    case C_GET_DIR:		vars[1] = get_dir(args[0]);
				break;
    case C_GET_EXEC_COST:	vars[1] = get_exec_cost();
				break;
    case C_IMPLODE:		vars[1] = implode(args[0], args[1]);
				break;
    case C_INTERACTIVE:		if (nargs == 0) {
				    vars[1] = interactive(previous_object());
				} else {
				    vars[1] = interactive(args[0]);
				}
				break;
    case C_INTP:		vars[1] = intp(args[0]);
				break;
    case C_LAMBDA:		vars[1] = lambda(args...);
				break;
    case C_LIVING:		vars[1] = living(args[0]);
				break;
    case C_LOCALCMD:		localcmd();
				break;
    case C_LOG_FILE:		log_file(args[0], args[1]);
				break;
    case C_LOWER_CASE:		vars[1] = lower_case(args[0]);
				break;
    case C_LS:			vars[1] = ls(args[0]);
				break;
    case C_M_DELETE:		vars[1] = m_delete(args[0], args[1]);
				break;
    case C_M_INDICES:		vars[1] = m_indices(args[0]);
				break;
    case C_M_SIZEOF:		vars[1] = m_sizeof(args[0]);
				break;
    case C_M_VALUES:		vars[1] = m_values(args[0]);
				break;
    case C_MAP_ARRAY:		vars[1] = map_array(args...);
				break;
    case C_MAP_MAPPING:		vars[1] = map_mapping(args...);
				break;
    case C_MAPPINGP:		vars[1] = mappingp(args[0]);
				break;
    case C_MEMBER_ARRAY:	vars[1] = member_array(args[0], args[1]);
				break;
    case C_MKDIR:		vars[1] = mkdir(args[0]);
				break;
    case C_MKMAPPING:		vars[1] = mkmapping(args[0], args[1]);
				break;
    case C_MOVE_OBJECT:		move_object(args[0], args[1]);
				break;
    case C_NEXT_INVENTORY:	if (nargs == 0) {
				    vars[1] = next_inventory(previous_object());
				} else {
				    vars[1] = next_inventory(args[0]);
				}
				break;
    case C_NOTIFY_FAIL:		notify_fail(args[0]);
				break;
    case C_OBJECTP:		vars[1] = objectp(args[0]);
				break;
    case C_PARSE_STRING:	vars[1] = parse_string(args[0], args[1]);
				break;
    case C_POINTERP:		vars[1] = pointerp(args[0]);
				break;
    case C_PRESENT:		if (nargs == 1) {
				    vars[1] = present(args[0],
						      previous_object());
				} else {
				    vars[1] = present(args[0], args[1]);
				}
				break;
    case C_PREVIOUS_OBJECT:	vars[1] = previous_object(1);
				break;
    case C_QUERY_HOST_NAME:	vars[1] = query_host_name();
				break;
    case C_QUERY_IDLE:		vars[1] = query_idle(args[0]);
				break;
    case C_QUERY_IP_NAME:	vars[1] = query_ip_name(args...);
				break;
    case C_QUERY_IP_NUMBER:	vars[1] = query_ip_number(args...);
				break;
    case C_QUERY_LOAD_AVERAGE:	vars[1] = query_load_average();
				break;
    case C_QUERY_SNOOP:		vars[1] = query_snoop(args[0]);
				break;
    case C_QUERY_VERB:		vars[1] = query_verb();
				break;
    case C_RANDOM:		vars[1] = random(args[0]);
				break;
    case C_READ_BYTES:		vars[1] = read_bytes(args...);
				break;
    case C_READ_FILE:		vars[1] = read_file(args...);
				break;
    case C_REMOVE_CALL_OUT:	vars[1] = remove_call_out(args[0]);
				break;
    case C_RENAME:		vars[1] = rename(args[0], args[1]);
				break;
    case C_RESTORE_OBJECT:	vars[1] = restore_object(args[0]);
				break;
    case C_RM:			vars[1] = rm(args[0]);
				break;
    case C_RMDIR:		vars[1] = rmdir(args[0]);
				break;
    case C_SAVE_OBJECT:		save_object(args[0]);
				break;
    case C_SAY:			say(args...);
				break;
    case C_SET_BIT:		vars[1] = set_bit(args[0], args[1]);
				break;
    case C_SET_HEART_BEAT:	vars[1] = set_heart_beat(args[0]);
				break;
    case C_SET_LIVING_NAME:	set_living_name(args[0]);
				break;
    case C_SET_THIS_PLAYER:	set_this_player(args[0]);
				break;
    case C_SHOUT:		shout(args[0]);
				break;
    case C_SHUTDOWN:		shutdown();
				break;
    case C_SIZEOF:		vars[1] = sizeof(args[0]);
				break;
    case C_SNOOP:		vars[1] = snoop(args[0]);
				break;
    case C_SORT_ARRAY:		if (nargs == 2 && stringp(args[1])) {
				    vars[1] = sort_array(args[0], args[1],
							 previous_object());
				} else {
				    vars[1] = sort_array(args...);
				}
				break;
    case C_STATUS:		vars[1] = status(args...);
				break;
    case C_STRINGP:		vars[1] = stringp(args[0]);
				break;
    case C_STRLEN:		vars[1] = strlen(args[0]);
				break;
    case C_SWAPOUT:		swapout();
				break;
    case C_TAIL:		vars[1] = tail(args[0]);
				break;
    case C_TELL_OBJECT:		tell_object(args[0], args[1]);
				break;
    case C_TELL_ROOM:		tell_room(args...);
				break;
    case C_TEST_BIT:		vars[1] = test_bit(args[0], args[1]);
				break;
    case C_THIS_OBJECT:		vars[1] = previous_object();
				break;
    case C_THIS_PLAYER:		vars[1] = this_player(args...);
				break;
    case C_TIME:		vars[1] = time();
				break;
    case C_TRANSFER:		vars[1] = transfer(args[0], args[1]);
				break;
    case C_UNIQUE_ARRAY:	vars[1] = unique_array(args...);
				break;
    case C_USERS:		vars[1] = users();
				break;
    case C_VERSION:		vars[1] = version();
				break;
    case C_WIZLIST:		wizlist(args...);
				break;
    case C_WRITE:		write(args[0]);
				break;
    case C_WRITE_BYTES:		vars[1] = write_bytes(args[0], args[1],
						      args[2]);
				break;
    case C_WRITE_FILE:		vars[1] = write_file(args[0], args[1]);
				break;

    default:			error("Unknown closure");
    }

    return vars;
}

/*
 * NAME:	compile()
 * DESCRIPTION:	compile a lambda expression to a closure
 */
closure compile(mixed func, mixed *def)
{
    int code, i, j, nargs;
    mixed arg;

    if (def == 0) {
	/*
	 * short format
	 */
	code = codes[func];
	if (code != 0) {
	    if (code <= C_CONTROL) {
		error("Lambda: short format only for efuns, operators and " +
		      "local functions");
	    }
	    func = code;
	} else if (!ident(func)) {
	    error("Lambda: bad function");
	}
	return ({ LAMBDA, code });
    } else {
	/*
	 * long format
	 */
	for (i = 0, nargs = sizeof(func); i < nargs; i++) {
	    arg = func[i];
	    if (!stringp(arg) || strlen(arg) <= 1 || !ident(arg[1 ..])) {
		error("Lambda: invalid parameter " + (i + 1));
	    }
	    for (j = 0; j < i; j++) {
		if (func[j] == func[i]) {
		    error("Lambda: duplicate parameter");
		}
	    }
	}

	code = codes[def[0]];
	if (code != 0) {
	    i = sizeof(def) - 1;
	    if (i < MIN(code)) {
		error("Lambda: too few arguments");
	    } else if (i > MAX(code)) {
		error("Lambda: too many arguments");
	    } else {
		switch (code) {
		case C_IF:
		case C_IFNOT:
		    if ((i & 1) == 0) {
			error("Lambda: odd number of arguments required");
		    }
		    break;

		case C_ASSIGN:
		case C_PLUSEQ:
		case C_MINEQ:
		case C_MULTEQ:
		case C_DIVEQ:
		case C_MODEQ:
		case C_LSHIFTEQ:
		case C_RSHIFTEQ:
		case C_ANDEQ:
		case C_XOREQ:
		case C_OREQ:
		    if ((i & 1) != 0) {
			error("Lambda: even number of arguments required");
		    }
		    for (j = 1; j <= i; j += 2) {
			arg = def[j];
			if (!stringp(arg) || strlen(arg) <= 1 ||
			    !ident(arg[1 ..])) {
			    error("Lambda: invalid argument " + j);
			}
		    }
		    break;

		case C_PLUSPLUS:
		case C_MINMIN:
		    for (j = 1; j <= i; j++) {
			arg = def[j];
			if (!stringp(arg) || strlen(arg) <= 1 ||
			    !ident(arg[1 ..])) {
			    error("Lambda: invalid argument " + j);
			}
		    }
		    break;

		case C_SSCANF:
		    for (j = 3; j <= i; j++) {
			arg = def[j];
			if (!stringp(arg) || strlen(arg) <= 1 ||
			    !ident(arg[1 ..])) {
			    error("Lambda: invalid argument " + j);
			}
		    }
		    break;
		}
	    }
	    arg = code;
	} else if (!ident(arg=def[0])) {
	    error("Lambda: bad function");
	}

	return ({ LAMBDA, code, nargs }) + func + def[1 ..];
    }
}

/*
 * NAME:	filter_array
 * DESCRIPTION:	filter the elements of an array
 */
varargs mixed *lambda_filter_array(mixed *arr, closure func)
{
    mixed *copy, elt;
    int i, j, sz;

    copy = allocate(sz = sizeof(arr));
    for (i = 0, j = -1; i < sz; i++) {
	if (eval(({ func, elt = arr[i] }), ([ ]))[1]) {
	    copy[++j] = elt;
	}
    }

    return copy[0 .. j];
}

/*
 * NAME:	map_array
 * DESCRIPTION:	map the elements of an array
 */
varargs mixed *lambda_map_array(mixed *arr, closure func)
{
    mixed *copy;
    int i, sz;

    copy = allocate(sz = sizeof(arr));
    for (i = 0; i < sz; i++) {
	copy[i] = eval(({ func, arr[i] }), ([ ])[1]);
    }

    return copy;
}

/*
 * NAME:	sort_array()
 * DESCRIPTION:	sort an array
 */
varargs mixed *lambda_sort_array(mixed *source, closure func)
{
    int step, halfstep, size;
    int i, j, i1, i2, end1, end2;
    mixed *dest, *temp;

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
		if (eval(({ func, source[i1], source[i2] }), ([ ]))[1] > 0) {
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
 * NAME:	filter_mapping()
 * DESCRIPTION:	filter a mapping by its values
 */
varargs mapping lambda_filter_mapping(mapping map, closure func)
{
    mixed *indices, *values, value;
    mapping copy;
    int i, sz;

    indices = map_indices(map);
    values = map_values(map);
    copy = ([ ]);
    for (i = 0, sz = sizeof(indices); i < sz; i++) {
	if (eval(({ func, value = values[i] }), ([ ]))[1]) {
	    copy[indices[i]] = value;
	}
    }

    return copy;
}

/*
 * NAME:	map_mapping()
 * DESCRIPTION:	map the values of a mapping
 */
varargs mapping lambda_map_mapping(mapping map, closure func)
{
    mixed *indices, *values;
    mapping copy;
    int i, sz;

    indices = map_indices(map);
    values = map_values(map);
    copy = ([ ]);
    for (i = 0, sz = sizeof(indices); i < sz; i++) {
	copy[indices[i]] = eval(({ func, values[i] }), ([ ])[1]);
    }

    return copy;
}
