struct state_struct ;

int sim_clear_breakpoints ();
int sim_fetch_register ();

void sim_write PARAMS ((long int, char *, int));
void sim_read PARAMS ((long int, char *, int));
int sim_clear_breakpoints PARAMS ((void));
void sim_set_pc PARAMS ((int));
void sim_store_register PARAMS ((int, int));
void sim_resume PARAMS ((int, int));
int sim_stop_signal PARAMS ((void));
void sim_info PARAMS ((struct state_struct *));
void sim_info_print PARAMS ((struct state_struct  *));

#define SIM_SINGLE_STEP 1
#define SIM_DONE 2
#define SIM_BREAKPOINT 3
#define SIM_INTERRUPT 4
#define SIM_BAD_INST 5
#define SIM_DIV_ZERO 6
#define SIM_BAD_SYSCALL 7
#define SIM_BAD_ALIGN 8
#define SIM_BAD_ADDR 9
