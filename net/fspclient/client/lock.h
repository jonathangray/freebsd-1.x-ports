#ifndef ANSI_PROTOTYPES
extern int client_get_key();
extern void client_put_key();
extern void client_init_key();
extern void client_finish_key();

#else /* ANSI_PROTOTYPES */

extern int client_get_key(void);
extern void client_put_key(u_int key);
extern void client_init_key(u_long server_addr, u_int server_port, u_int key);
extern void client_finish_key(void);
#endif /* ANSI_PROTOTYPES */
