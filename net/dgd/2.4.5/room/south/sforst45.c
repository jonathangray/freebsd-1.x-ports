reset(started)
{
    if (!started)
	set_light(1);
}

init()
{
    add_action("north"); add_verb("north");
    add_action("east");  add_verb("east");
}

short()
{
    return "A dimly lit forest";
}

long()
{
    write("You are in part of a dimly lit forest.\n" +
	  "Trails lead north and east\n");
}

north()
{
    call_other(this_player(), "move_player", "north#room/south/sforst37");
    return 1;
}

east()
{
    call_other(this_player(), "move_player", "east#room/south/sforst44");
    return 1;
}
