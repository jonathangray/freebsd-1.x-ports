# include <sys/types.h>
# include <sys/time.h>
# include <sys/socket.h>
# include <netinet/in.h>
# include <arpa/inet.h>
# include <netdb.h>
# include <stdio.h>

int main(argc, argv)
int argc;
char *argv[];
{
    char buffer[256], password[32], *p;
    struct sockaddr_in sin;
    struct hostent *host;
    int tcpip, len, on;
    FILE *input, *output;
    long ipnum;

    if (argc != 3) {
	fprintf(stderr, "Usage: %s port password\n", argv[0]);
	return 2;
    }
    strcpy(password, argv[2]);
    for (p = argv[2]; *p != '\0'; p++) {
	*p = ' ';
    }

    gethostname(buffer, sizeof(buffer));
    host = gethostbyname(buffer);
    if (host == (struct hostent *) NULL) {
	perror("gethostbyname");
	return 1;
    }

    for (;;) {
	tcpip = socket(host->h_addrtype, SOCK_STREAM, 0);
	if (tcpip < 0) {
	    perror("socket");
	    return 1;
	}
	on = 1;
	if (setsockopt(tcpip, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on)) < 0) {
	    perror("setsockopt");
	    exit(2);
	}

	memset(&sin, '\0', sizeof(sin));
	memcpy(&sin.sin_addr, host->h_addr, host->h_length);
	sin.sin_port = htons(atoi(argv[1]));
	sin.sin_family = host->h_addrtype;
	if (connect(tcpip, (struct sockaddr *) &sin, sizeof(sin)) >= 0) {
	    break;
	}
	close(tcpip);
	sleep(5);
    }

    input = fdopen(tcpip, "r");
    output = fdopen(tcpip, "w");
    fprintf(output, "HNAME\n%s\n", password);
    fflush(output);

    while (fgets(buffer, sizeof(buffer), input) != (char *) NULL) {
	len = strlen(buffer) - 1;
	if (len > 0 && buffer[len - 1] == '\r') {
	    --len;
	}
	buffer[len] = '\0';
	if (strncmp(buffer, "> ", 2) == 0) {
	    strcpy(buffer, buffer + 2);
	}

	if (strncmp(buffer, "QUERY ", 6) == 0) {
	    ipnum = inet_addr(buffer + 6);
	    if (ipnum != -1) {
		host = gethostbyaddr(&ipnum, 4, AF_INET);
		if (host == (struct hostent *) NULL) {
		    sleep(5);
		    host = gethostbyaddr(&ipnum, 4, AF_INET);
		}
		if (host != (struct hostent *) NULL) {
		    fprintf(output, "say %s=%s\n", buffer + 6, host->h_name);
		    fflush(output);
		}
	    }
	}
    }
}
