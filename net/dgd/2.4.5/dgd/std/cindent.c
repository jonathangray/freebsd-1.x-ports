/*
 * NAME:	indent()
 * DESCRIPTION:	indent an LPC file
 */
int indent(string file)
{
    editor("e " + file);
    editor("I");
    editor("x");
    destruct(this_object());
    return 1;
}
