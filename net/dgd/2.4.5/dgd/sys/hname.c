inherit "/dgd/lib/player";

mapping hosts1, hosts2;
int num;

static void reset(int arg)
{
    if (hosts1 == 0) {
	hosts1 = ([ ]);
	hosts2 = ([ ]);
    }
}

void logon()
{
    if (interactive(previous_object())) {
	enable_commands();
	exec(this_object(), previous_object());
	catch_tell("\n");
	input_to("get_pass");
    }
}

static void get_pass(string password)
{
    string crypted;

    crypted = "HNf0aVEF//1pc";
    if (crypt(password, crypted) == crypted) {
	input_to("add_ip_name");
    } else {
	object player;

	player = clone_object("/obj/player");
	exec(player, this_object());
	destruct(player);
    }
}

static void add_ip_name(string str)
{
    string ipnum, ipname;

    input_to("add_ip_name");
    sscanf(str, "say %s=%s", ipnum, ipname);
    if (num == 50) {
	hosts2 = hosts1;
	hosts1 = ([ ]);
	num = 0;
    }
    hosts1[ipnum] = ipname;
    num++;
}

string request_ip_name(string ipnum)
{
    string ipname;

    ipname = hosts1[ipnum];
    if (ipname == 0) {
	ipname = hosts2[ipnum];
	if (ipname == 0) {
	    catch_tell("QUERY " + ipnum + "\n");
	    return ipnum;
	}
    }
    return ipname;
}
