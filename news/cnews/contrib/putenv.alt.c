/*
 * From: chip@ateng.ateng.com (Chip Salzenberg)
 * Newsgroups: comp.unix.wizards
 * Subject: Re: replacement for putenv()
 * Date: 13 Feb 89 16:51:05 GMT
 * 
 * Here is a rather nice replacement for putenv().  I wrote it for the BSD port
 * of my deliver program.  (I know it's source, but it's short.)  Its nicest
 * feature is the avoidance of memory waste when it is called several times.
 */

int
putenv(s)
char *s;
{
	static char **env_array;
	static int env_size;
	char *e;
	int i, j;

	if (env_array == NULL) {
		for (i = 0; environ[i]; ++i)
			;
		env_size = i + 10;   	   /* arbitrary */
		env_array = (char **) malloc(env_size * sizeof(char *));
		if (env_array == NULL)
			return 1;
		memcpy((char *)env_array, (char *)environ,
		       (int) ((i + 1) * sizeof(char *)));
		environ = env_array;
	} else if (environ != env_array)
		fprintf(stderr, "putenv: warning: someone moved environ!\n");

	if ((e = strchr(s, '=')) != NULL)
		++e;
	else
		e = s + strlen(s);

	j = 0;
	for (i = 0; env_array[i]; ++i)
		if (strncmp(env_array[i], s, e - s) != 0)
			env_array[j++] = env_array[i];

	if (j + 1 >= env_size) {
		env_size += 10;                 /* arbitrary */
		env_array = (char **) realloc((char *)env_array,
					env_size * sizeof(char **));
		if (env_array == NULL)
			return 1;
	}

	env_array[j++] = s;
	env_array[j] = NULL;

	environ = env_array;
	return 0;
}
