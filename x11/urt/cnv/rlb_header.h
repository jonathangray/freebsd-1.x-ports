/* Header file for Wavefront 'rla' image files. */


typedef struct {
   short left, right, bottom, top;
} WINDOW_S;

typedef struct {
   WINDOW_S	window;
   WINDOW_S	active_window;
   short	frame;
   short	storage_type;
   short	num_chan;
   short	num_matte;
   short	num_aux;
   short	aux_mask;
   char		gamma[16];
   char		red_pri[24];
   char		green_pri[24];
   char		blue_pri[24];
   char		white_pt[24];
   long		job_num;
   char		name[128];
   char		desc[128];
   char		program[64];
   char		machine[32];
   char		user[32];
   char		date[20];
   char		aspect[24];
   char		aspect_ratio[8];
   char		chan[32];
   short	field;
   short	filter_type;
   long		magic_number;
   long		lut_size;
   long		user_space_size;
   long		wf_space_size;
   short	lut_type;
   short	mix_type;
   short	encode_type;
   short	padding;
   char		space[100];
} RLB_HEADER;
