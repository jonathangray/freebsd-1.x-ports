#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <sys/un.h>
#include <sys/time.h>
#include <fcntl.h>

int doConnect(host, port)
char *host;
int port;
{
    struct hostent *hostent, mkHost;
    struct sockaddr_in sockaddr;
    int sock, status;
    int hostaddr, hostaddrPtr[2];
    extern int errno;

    if ((hostent = gethostbyname(host)) == NULL)
    {
	if ((hostaddr = inet_addr(host)) == -1) { return -1; }
	mkHost.h_addr_list = (char **) hostaddrPtr;
	mkHost.h_addr_list[0] = (char *) &hostaddr;
	mkHost.h_addr_list[1] = NULL;
	mkHost.h_length = sizeof(hostaddr);
	mkHost.h_addrtype = AF_INET;
	hostent = &mkHost;
    }
    if ((sock = socket(PF_INET, SOCK_STREAM, 0)) < 0) { return -1; }
    bzero((char *) &sockaddr, sizeof(sockaddr));
    sockaddr.sin_family = AF_INET;
    memcpy((char *) &(sockaddr.sin_addr.s_addr),
	   (char *) hostent->h_addr_list[0],
	   (size_t) hostent->h_length);
    sockaddr.sin_port = htons(port);

    if (connect(sock, (struct sockaddr *) &sockaddr, sizeof(sockaddr)) < 0)
    {
	close(sock);
	return -1;
    }
    return sock;
}

int main(argc, argv)
int argc;
char *argv[];
{
    int fd, info;
    int port;
    char buffer[4096], host[128];
    int l, tl = 0, ntl, pid, out;
    time_t st;

    if ((l = fork()) != 0) 
    {
	printf("%d\n", l);
	fclose(stdout);
	exit(0);
    }
    close(1);
    close(2);
    gethostname(host, 128);
    pid = getpid();
    port = atoi(argv[2]);
    if ((fd = doConnect(argv[1], port)) < 0 )
    {
	exit(1);
    }
    if ((info = doConnect(host, atoi(argv[4]))) < 0 )
    {
	exit(1);
    }
    sprintf(host, "%d DCC Get connection to %s established.\n", pid, argv[5]);
    send(info, host, strlen(host), 0);
    st = time((time_t *) 0);
    out = open(argv[3], O_WRONLY | O_CREAT | O_TRUNC, 0600);
    while ((l = read(fd, buffer, 4096)) > 0) 
    {
	tl += l;
	if (write(out, buffer, l) != l) 
	{
	    l = -1;
	    break;
	}
	ntl = htonl(tl);
	if (write(fd, (char *) &ntl, sizeof(int)) != sizeof(int))
	{
	    l = -1;
	    break;
	}
    }
    close(out);
    shutdown(fd, 2);
    close(fd);    
    if (l < 0)
    {
	sprintf(host, "%d DCC Get: %s from %s Error on transfer.\n", pid,
		argv[3], argv[5]);
	unlink(argv[3]);
    }
    else 
    {
	if ((st = time((time_t *) 0) - st) == 0)
	{
	    st = 1;
	}
	sprintf(host, "%d DCC Get: %s from %s completed. %f Kbytes/sec\n", pid,
	    argv[3], argv[5], ((float) tl /1000.0) / (float) st);
    }
    send(info, host, strlen(host), 0);
    shutdown(info, 2);
    close(info);
    exit(0);
}
