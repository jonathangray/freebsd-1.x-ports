reset(started)
{
    if (!started)
	set_light(1);
}

init()
{
    add_action("south"); add_verb("south");
    add_action("east");  add_verb("east");
}

short()
{
    return "A dimly lit forest";
}

long()
{
    write("You are in part of a dimly lit forest.\n" +
	  "Trails lead south and east\n");
}

south()
{
    call_other(this_player(), "move_player", "south#room/south/sforst6");
    return 1;
}

east()
{
    call_other(this_player(), "move_player", "east#room/south/sforst1");
    return 1;
}
