#define BUFSIZ	1024
#include <sys/file.h>

main(argc,argv)
int	argc;
char	**argv;
{
	char	*ofile, buf[BUFSIZ];
	int	fd, n, p, neednl;

	argc--, argv++;
	neednl = 0;
	p = 1;
	if (**argv == '-') {
		if ((fd = open(argv[0]+1,O_WRONLY|O_APPEND|O_CREAT,0644)) < 0
		    || dup2(fd, 1) < 0) {
			perror(argv[0]+1);
			exit(1);
		}
		argc--, argv++;
	}
	while (argc--) {
		if ((fd = open(*argv, 0)) < 0) {
			perror(*argv++);
			continue;
		}
		argv++;
		if (neednl)
			write(1, "\n", 1);
		while ((n = read(fd, buf, BUFSIZ)) > 0) {
			write(1, buf, n);
			if (n == 1)
				neednl = !(buf[0] == '\n' && buf[p-1] == '\n');
			else
				neednl = !(buf[n-1] == '\n' && buf[n-2]=='\n');
			p = n;
		}
		close(fd);
	}
	if (neednl)
		write(1, "\n", 1);
	exit(0);
}
