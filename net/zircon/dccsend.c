#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>
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

int doServer(port)
int *port;
{
    struct hostent *hostent, mkHost;
    struct sockaddr_in sockaddr;
    int sock, status;
    int hostaddr, hostaddrPtr[2];

    if ((sock = socket(PF_INET, SOCK_STREAM, 0)) < 0)
    {
	return -1;
    }
    hostaddr = INADDR_ANY;
    mkHost.h_addr_list = (char **) hostaddrPtr;
    mkHost.h_addr_list[0] = (char *) &hostaddr;
    mkHost.h_addr_list[1] = NULL;
    mkHost.h_length = sizeof(hostaddr);
    mkHost.h_addrtype = AF_INET;
    hostent = &mkHost;

    bzero((char *) &sockaddr, sizeof(sockaddr));
    sockaddr.sin_family = AF_INET;
    memcpy((char *) &(sockaddr.sin_addr.s_addr),
	   (char *) hostent->h_addr_list[0],
	   (size_t) hostent->h_length);
    sockaddr.sin_port = htons(0);

    if (bind(sock, (struct sockaddr *) &sockaddr, sizeof(sockaddr)) < 0)
    {
	close(sock);
	return -1;
    }
    status = sizeof(sockaddr);
    getsockname (sock, (struct sockaddr *) &sockaddr, &status);
    *port = ntohs(sockaddr.sin_port);
    return sock;
}

int doaccept(fdi)
int fdi;
{
    struct sockaddr_in sockaddr;
    int len = sizeof sockaddr;
    int fd;

    fd = accept (fdi, (struct sockaddr *) &sockaddr, &len);
    shutdown(fdi, 2);
    close(fdi);
    return (fd < 0) ? -1 : fd;
}

int main(argc, argv)
int argc;
char *argv[];
{
    int fd, info, g;
    int port;
    char host[128];
    char buffer[4096];
    int l, tl = 0, i, pid;
    struct timeval timeout;
    fd_set rdset;

    time_t st;
    

    if ((fd = doServer(&port)) < 0 || port == 0)
    {
	exit(1);
    }
    if ((l = fork()) != 0)
    {
	printf("%d %d\n", port, l);
	exit(0);
    }
    close(0);
    close(1);
    close(2);
    listen(fd, 5);
    pid = getpid();
    gethostname(host, 128);
    info = doConnect(host, atoi(argv[2]));
    sprintf(host, "%d DCC Send accept to %s established.\n", pid, argv[3]);
    send(info, host, strlen(host), 0);
    if ((fd = doaccept(fd)) < 0) 
    {
	exit(1);
    }
    sprintf(host, "%d DCC Send connection to %s established.\n", fd, argv[3]);
    send(info, host, strlen(host), 0);
    if ((g = open(argv[1], O_RDONLY)) < 0)
    {
	exit(1);
    }
    st = time((time_t *) 0);
    
    while ((l = read(g, buffer, 2048)) > 0)
    {
	tl += l;
	if (write(fd, buffer, l) != l) 
	{
	    l = -1;
	    break;
	}
	do
	{
	    FD_ZERO(&rdset);
	    FD_SET(fd, &rdset);
	    timeout.tv_sec = 10;
	    timeout.tv_usec = 0;
	    if (select(fd + 1, &rdset, NULL, NULL, &timeout) != 1) 
	    {
		l = -1;
		break;
	    }
	    else
	    {
	        if (read(fd, (char *) &i, sizeof(int)) != sizeof(int)) 
		{
		    l = -1;
		    break;
		}
		
	    }
	}
	while (ntohl(i) != tl);
	
    }
    shutdown(fd, 2);
    close(fd);
    close(g);
    if (l < 0) 
    {
	sprintf(host, "%d DCC Send %s to %s: Error on transfer.\n", pid, 
	        argv[1], argv[3]);
    }
    else
    {
	if ((st = time((time_t *) 0) - st) == 0)
	{
	    st = 1;
        }
	sprintf(host, "%d DCC Send %s to %s completed. %f Kbytes/sec", pid, 
		argv[1], argv[3], (((float) tl / 1000.0) / (float) st));
    }
    send(info, host, strlen(host), 0);
    shutdown(info, 2);
    close(info);
    exit(0);
}
