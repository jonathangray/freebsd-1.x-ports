reset(started)
{
    if (!started)
	set_light(1);
}

init()
{
    add_action("north");     add_verb("north");
    add_action("east");      add_verb("east");
    add_action("northwest"); add_verb("northwest");
    add_action("southeast"); add_verb("southeast");
}

short()
{
    return "The shore of Crescent Lake";
}

long()
{
    write("You are standing on the shore of Crescent Lake, a beautiful and\n" +
	  "clear lake. Out in the centre of the lake stands the Isle\n" +
	  "of the Magi.\n" +
	  "Trails lead into the forest to the south and west.\n" +
	  "The shore of Crescent Lake continues northwest and southeast\n");
}

north()
{
    call_other(this_player(), "move_player", "north#room/south/sforst49");
    return 1;
}

east()
{
    call_other(this_player(),"move_player", "east#room/south/sforst46");
    return 1;
}

northwest()
{
    call_other(this_player(),"move_player", "northwest#room/south/sshore24");
    return 1;
}

southeast()
{
    call_other(this_player(),"move_player", "southeast#room/south/sshore26");
    return 1;
}
