/*  st_priv.h - private header file for the symbol table reading routines */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)st_priv.h	1.16 15/9/92 (UKC) */

#if defined(OS_SUNOS) && defined(ARCH_SUN386)
#define COFF_SUN386
#define ST_COFF
#endif

/*  The DS3100 has it's own symbol table format, designed
 *  by Third Eye Software.  Set a cpp flag to indicate this.
 */
#if defined(ARCH_MIPS) && (defined(OS_ULTRIX) || defined(OS_RISCOS) \
							|| defined(OS_NEWSOS))
#define ST_TE
#define ST_COFF
#endif

#ifdef COFF_SUN386
typedef struct syment nlist_t;
#define SYMSIZE		SYMESZ
#else
#ifdef ST_TE
typedef SYMR nlist_t;
#else
typedef struct nlist nlist_t;
#endif
#define SYMSIZE		sizeof(nlist_t)
#endif /* !COFF_SUN386 */

/*  We fake N_DATA, N_TEXT and N_EXT in getsym and findsym, so define
 *  them here.
 */
#ifdef COFF_SUN386
#define N_UNDF		0x0
#define N_TEXT		0x4
#define N_DATA		0x6
#define N_BSS		0x8
#define N_EXT		1
#else
#define n_offset	n_un.n_strx
#define n_dbx_type	n_type
#endif

typedef struct fsyminfo {
	int fs_symno;		/* Start of syms in file for this func */
	int fs_symlim;			/* ditto */
	long fs_cblist;			/* used only in st_cb.c */
#if defined(ARCH_SUN386) && defined(OS_SUNOS)
	long fs_coff_lno_start;		/* see st_read.c */
	long fs_coff_lno_lim;		/* ditto */
#endif
#ifdef ST_TE
	/*  Fields for extracting line number information.
	 */
	int fs_lno_base;
	int fs_first_lnum;
	int fs_last_lnum;

	frame_t fs_frbuf;
#endif
} fsyminfo_t;

#define FU_SYMNO(f)	       ((fsyminfo_t *)(f)->fu_fsyminfo_id)->fs_symno
#define FU_SYMLIM(f)	       ((fsyminfo_t *)(f)->fu_fsyminfo_id)->fs_symlim
#define FU_CBLIST(f)	       ((fsyminfo_t *)(f)->fu_fsyminfo_id)->fs_cblist
#define FU_COFF_LNO_START(f)   ((fsyminfo_t *)(f)->fu_fsyminfo_id)->fs_coff_lno_start
#define FU_COFF_LNO_LIM(f)     ((fsyminfo_t *)(f)->fu_fsyminfo_id)->fs_coff_lno_lim

#ifdef ST_TE
typedef struct extsymst {
	const char *es_name;
	taddr_t es_addr;
	struct stfst *es_stf;
	int es_symno;
} extsym_t;
#else
typedef struct ftypest {
	int ft_tnum;
	type_t ft_type;
	struct ftypest *ft_next;
} ftype_t;

typedef struct snlistst {
	int sn_symno;
	const char *sn_name;
#ifdef ultrix
	taddr_t sn_addr;
#endif
	struct snlistst *sn_next;
} snlist_t;
#endif

/*  Element in the linked list of global variable names and 
 *  addresses for a symbol table.
 */
typedef struct addrlistst {
	const char *al_name;
	taddr_t al_addr;
	var_t *al_var;
	struct addrlistst *al_next;
} addrlist_t;

typedef enum symtab_typeen { STT_MAIN, STT_SHLIB } symtab_type_t;

typedef struct symio_idst { int dummy_member; } *symio_id_t;

typedef struct functab_idst { int dummy_member; } *functab_id_t;

typedef struct cblist_idst { int dummy_member; } *cblist_id_t;

typedef struct {
	const char *ti_name;		/* Name of target binary */
	long ti_mod_time;		/* Mod time of executable file */
	int ti_highlighted_lnum;	/* Current highlighted line of source */
	fil_t *ti_highlighted_fil;	/* File the above line is in */
} target_info_t;

/*  Symbol table structure.  One per a.out format file.  Normally there
 *  is only one of these, but if we are using Sun shared libararies
 *  there will be one per shared library file as well as one for the
 *  main executable.
 */
typedef struct symtabst {
	const char *st_name;		/* Name of the a.out file */
	symtab_type_t st_type;		/* Type (STT_MAIN or STT_SHLIB) */
	int st_dynamic;			/* TRUE if dynamically linked */
	target_info_t *st_target_info;	/* Per target information */

	alloc_id_t st_alloc_id;		/* Id for alloc pool of this symtab */

	long st_text_addr_offset;	/* Offset in process of start of text */

	symio_id_t st_symio_id;		/* Symbol input from a.out file stuff */
	fil_t *st_sfiles;		/* List of source files */
	cblist_id_t st_cblist_id;	/* List of FORTRAN common blocks */
	functab_id_t st_functab_id;	/* Addr --> func mapping table */
	addrlist_t *st_addrlist;	/* List of addresses of globals */
	func_t *st_funclist;		/* List of functions */

#ifdef ST_TE
	extsym_t *st_exttab;		/* External symbols */
	int st_exttab_size;		/* # external symbols */
	char *st_lnotab;		/* The whole line number table */
	long st_lnotab_size;		/* Size of the table */
	strcache_id_t st_aux_strcache;	/* Aux symbols (TIRs etc) */
	struct stfst **st_stftab;	/* For rndx mapping */
	int st_stftab_size;		/* # entries in st_stftab */
#endif

	struct symtabst *st_next;	/* Next symbol table */
} symtab_t;

#ifdef ST_TE
typedef struct aggrlistst {
	type_t *al_type;
	int al_symno;
	struct aggrlistst *al_next;
} aggrlist_t;
#endif

typedef struct stfst {
	const char *stf_name;
	language_t stf_language;
	symtab_t *stf_symtab;
	fil_t *stf_fil;
	int stf_symno;
	int stf_symlim;
	taddr_t stf_addr;
	unsigned stf_flags;
#ifdef ARCH_CLIPPER
	addrlist_t *stf_addrlist;
#endif
#ifdef ST_TE
	aggrlist_t *stf_aggrlist;
	long *stf_rfdtab;
	int stf_rfdtab_size;
	long stf_aux_base;
	long stf_strings_base;
	int stf_lno_base;
	int stf_lno_lim;
#else
	snlist_t *stf_sn;
	ftype_t *stf_types;
	struct hfst **stf_fmap;
	int stf_mapsize;
	int stf_fnum;
#endif
} stf_t;

/*  Flag bits in stf_flags.
 */
#define STF_LNOS_PRECEDE_FUNCS	0x01	/* for gcc */
#define STF_HIDE		0x02	/* don't display this under source files */

typedef struct hfst {
	stf_t *hf_stf;
	int hf_id;
	struct hfst *hf_next;
} hf_t;

#ifdef OS_SUNOS_4
/*  Shared library header on Suns.
 */
typedef struct shlibst {
	const char *sh_name;
	taddr_t sh_addr;
	struct shlibst *sh_next;
} shlib_t;
#endif /* OS_SUNOS_4 */

/* Function prototypes.
 */

/*  st_read.c
 */
symio_id_t make_symio PROTO((const char *execfile, int fd,
			     off_t syms_offset, int nsyms, off_t strings_offset,
			     long addr_to_fpos_offset, alloc_id_t alloc_id));
int findsym PROTO((symio_id_t symio_id, int symno,
						nlist_t *p_res, const char *symset));
void getsym PROTO((symio_id_t symio_id, int symno, nlist_t *p_nm));
const char *symstring PROTO((symio_id_t symio_id, int symno));
const char *get_cont_symstring PROTO((symio_id_t symio_id, int *p_symno));
const char *si_get_string PROTO((symio_id_t symio_id, off_t offset));
void close_symio PROTO((symio_id_t symio_id));
void adjust_symio_addr_offset PROTO((symio_id_t symio_id, long delta));
int get_symio_nsyms PROTO((symio_id_t symio_id));
void get_func_coff_lno_offsets PROTO((symio_id_t symio_id, fil_t *sfiles));
lno_t *read_func_coff_lnos PROTO((symio_id_t symio_id, func_t *f, lno_t **p_last));

/*  st_fmap.c
 */
int addrcmp PROTO((func_t *f1, func_t *f2));
void adjust_functab_text_addr_base PROTO((functab_id_t functab_id,
					  func_t *funclist, long delta));
funclist_t *new_flist PROTO((void));
void free_flist_list PROTO((funclist_t *fl));
func_t *addr_and_functab_to_func PROTO((functab_id_t functab_id, taddr_t addr));
functab_id_t make_functab PROTO((symtab_t *st, func_t *flist, int flist_len,
					taddr_t first_addr, taddr_t last_addr));

/*  st_cb.c
 */
void free_cblist_strings PROTO((cblist_id_t cblist_id));
void global_and_cblist_to_var PROTO((cblist_id_t cblist_id, const char *name,
				     func_t *f, common_block_id_t *p_cblock,
				     fil_t **p_fil, var_t **p_var));
void iterate_over_cblist PROTO((cblist_id_t cblist_id,
			       void (*func)PROTO((common_block_id_t cblock))));
cblist_id_t create_empty_cblist PROTO((void));
cblist_id_t add_common_block PROTO((cblist_id_t cblist_id, stf_t *stf,
					func_t *f, symtab_t *st, int *p_symno));
void finish_common_blocks PROTO((cblist_id_t cblist_id, symtab_t *st,
						int *p_have_common_blocks));


/*  st_stab.c
 */
addrlist_t *insert_global_addr PROTO((alloc_id_t alloc_id, addrlist_t *addrlist,
						const char *name, taddr_t addr));
taddr_t lookup_global_addr PROTO((symtab_t *st, const char *name));
taddr_t find_addr_in_addrlist PROTO((addrlist_t *addrlist, const char *name));
symtab_t *get_main_st PROTO((void));
symtab_t *get_symtab_cache_list PROTO((void));


/*  st_skim.c
 */
stf_t *make_stf PROTO((const char *name, symtab_t *st, int symno,
					language_t language, taddr_t addr));
fil_t *make_fil PROTO((stf_t *stf, block_t *parblock,
					const char *path_hint, fil_t *next));
fsyminfo_t *make_fsyminfo PROTO((alloc_id_t alloc_id, int symno));
void fix_register_params PROTO((func_t *f));
void push_typedefs_and_aggrs PROTO((alloc_id_t alloc_id,	
						block_t *hdrbl, block_t *bl));
#ifndef ST_TE
void skim_symtab PROTO((symtab_t *st, taddr_t first_addr, taddr_t last_addr,
			block_t *rootblock,
			int *p_have_common_blocks, fil_t **p_sfiles,
			functab_id_t *p_functab_id, cblist_id_t *p_cblist_id));
type_t *tnum_to_type PROTO((stf_t *stf, int tnum));
type_t *lookup_tnum PROTO((stf_t *stf, int tnum));
ftype_t *insert_ftype PROTO((stf_t *stf, int tnum));
const char *save_fname PROTO((symtab_t *st, const char *name, int ext));
#endif

/*  st_parse.c
 */
type_t *make_undef_type PROTO((symtab_t *st, const char *tag, typecode_t typecode));
#ifndef ST_TE
type_t *TypeId PROTO((stf_t *stf, int *p_symno, const char **p_s, int eval));
void Typenum PROTO((const char **p_s, int *p_fnum, int *p_tnum));
type_t *Class PROTO((stf_t *stf, int *p_symno, const char **p_s, class_t *p_class));
int parse_num PROTO((const char **p_s));
void scheck PROTO((const char **p_s, int ch));
const char *parse_name PROTO((const char **p_s, alloc_id_t alloc_id));
#endif /* !ST_TE */

/*  st_shlib.c
 */
#ifdef OS_SUNOS_4
int get_preload_shlib_list PROTO((alloc_id_t alloc_id, const char *execfile,
							shlib_t **p_shlibs));
int get_shlibs_and_global_addrs PROTO((alloc_id_t alloc_id, symtab_t *st,
				     taddr_t addr_dynamic, shlib_t **p_shlibs));
#endif

/*  st_te.c
 */
#ifdef ST_TE
int skim_te_symtab PROTO((symtab_t *st, int fd,
			  off_t syms_offset, off_t addr_to_fpos_offset,
			  taddr_t first_addr, taddr_t last_addr,
			  block_t *rootblock,
			  bool *p_have_common_blocks, fil_t **p_sfiles,
			  functab_id_t *p_functab_id, cblist_id_t *p_cblist_id));
#endif
