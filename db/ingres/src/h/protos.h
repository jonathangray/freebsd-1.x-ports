/*-
 * Copyright (c) 1994 Westley Computing Ltd.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by Westley Computing Ltd
 * 4. Neither the name of Westley Computing Ltd nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY WESTLEY COMPUTING LTD. ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL WESTLEY COMPUTING LTD OR CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
 * THE POSSIBILITY OF SUCH DAMAGE.
 *
 *	Written by: Alistair G. Crooks, (agc@uts.amdahl.com)
 *
 *	$Id: protos.h,v 1.2 1994/06/19 05:38:24 alm Exp $
 */

#ifndef	INGRES_PROTOTYPES_H_
#define	INGRES_PROTOTYPES_H_

char *getsysident(void);

#ifdef INGRES_IUTIL
long new_page(char *tree);
char *trim_relname(char *name);
char *batchname(void);
void null_fn(void);
RETSIGTYPE rubcatch(int);
void markopen(long *ovect);
void closeall(int tell, long ovect);
int printatt(char type, int length, void *value);
void acc_init(int dozippo, int fake);
int setdbl(char act, char mod);
void opencatalog(char *name, int mode);
int getuser(char *code, char *buf);
int getnuser(char *name, char *buf);
int start_up_lock_driver(void);
void addmarkopen(long *pvect, int fd);
void wrbatch(char *cp, int count);
void flushbatch(void);
int acc_err(int errnum);
void checkadmin(register int fd);
void ingresname(char *iname, char *id, char *outname);
int icompare(char *ax, char *bx, char frmt, char frml);
int delete_btree(long *lid, register int level);
int newlino(int len);
void readadmin(int fake);
void create_btree(char *relname);
void rubon(void);
void ruboff(char *why);
void printeol(void);
void printeh(void);
void printhdr(char type, int length, char *value);
int rmbatch(void);
void beginhdr(void);
int unlall(void);
int initucode(int argc, char **argv, int dbflag, char **paramv, int waitmode);
int initdbpath(char *database, char *dbpath, int follow);
void db_lock(int mode);
char *errfilen(int digit);
int unldb(void);
int setdbl(char act, char mod);
int acc_close(void);
char *getparmvect(int n);
char *getflagvect(int n);
#ifdef INGRES_INGRES_H_
int setrll(char act, tid_t *rtid, char mod);
int unlrl(tid_t *rtid);
int setcsl(tid_t *rtid);
int unlcs(tid_t *rtid);
int stuff_page(tid_t *t, long *var);
void btreename(char *relname, char *outname);
int tup_len(tid_t *tid);
void del_tuple(tid_t *tid, int width);
void get_lid(tid_t *tidloc, long *lid);
int invalid(tid_t *tid);
int pluck_page(tid_t *t, long *var);
int dumptid(tid_t *tid);
void search_btree(tid_t mtid, register tid_t *tidloc);
void replace_btree(tid_t tid, register tid_t *tidloc);
void put_tuple(tid_t *tid, char *tuple, int length);
char *get_addr(tid_t *tid);
int addbatch(tid_t *oldtid, char *newtuple, char *oldtuple);
void lin_search(int level, long tid, tid_t *tidloc, long *lid, long tupcnt);
#endif
#ifdef INGRES_RANGE_H_
void ingres_setkey(desc_t *d, void *key, void *value, int dom);
int replace(desc_t *d, tid_t *tid, void *tuple, int checkdups);
int openr(desc_t *d, int mode, char *name);
int find(desc_t *d, int mode, tid_t *lotid, tid_t *hightid, void *key);
int get(desc_t *d, tid_t *tid, tid_t *limtid, void *tuple, int getnxt);
int kcompare (desc_t *dx, void *tuple, void *key);
int delete(desc_t *dx, tid_t *tidx);
int clearkeys(desc_t *d);
int getequal(desc_t *d, void *keyval, void *tupleval, tid_t *tid);
int flush_rel(register desc_t *d, int resetflag);
int closer(desc_t *d);
int noclose(desc_t *d);
int canonical(desc_t *d, char *tuple);
int get_page(desc_t *d, tid_t *tid);
void printup(desc_t *d, void *tuple);
int findbest(desc_t *dx, tid_t *tidx, char *tuple, int need, bool checkdups);
int insert_mbtree(desc_t *d, char *btree, long *lid, long *tid, tid_t *tidpos);
void get_tuple(desc_t *d, tid_t *tid, char *tuple);
int get_reltup(desc_t *d, char *name);
int get_attuples(desc_t *d);
void printdesc(desc_t *d);
int scan_dups(desc_t *d, tid_t *tid, char *tuple);
int dup_check(desc_t *d, tid_t *tid, char *tuple1);
int ndxsearch(desc_t *d, tid_t *tid, char *tuple, int mode, int keyok);
int add_ovflo(desc_t *dx, tid_t *tid);
void sortfile(char *infile, desc_t *d, int del);
int formatpg(desc_t *d, long n);
int insert(desc_t *d, tid_t *tid, void *tuple, bool checkdups);
int add_prim(desc_t *d, tid_t *tidx);
int cleanrel(desc_t *d);
void clr_tuple(desc_t *desc, void *tuple);
long rhash(register desc_t *d, char *key);
int openbatch(desc_t *rel_desc, desc_t *index_desc, int mode);
void btreerange(desc_t *d, long *low_lid, long *high_lid, tid_t *lotid, tid_t *hitid);
int closebatch(void);
char *getint_tuple(desc_t *d, tid_t *tid, char *tuple);
#endif /* INGRES_RANGE_H_ */
#ifdef INGRES_ACCESS_H_
void acc_addbuf(accbuf_t *bufs, int cnt);
int pageflush(accbuf_t *buf);
int resetacc(accbuf_t *buf);
int space_left(accbuf_t *bp);
accbuf_t *choose_buf(desc_t *dx, long pageid);
int last_page(desc_t *d, tid_t *tid, accbuf_t *buf);
int top_acc(accbuf_t *buf);
int setpgl(accbuf_t *buf);
int unlpg(accbuf_t *buf);
#endif /* INGRES_ACCESS_H_ */
#ifdef INGRES_LOCK_H_
int dolock(lock_req_t *lock);
#endif /* INGRES_LOCK_H_ */
#ifdef INGRES_BTREE_H_
long get_tid(long rootpage, long lid, locator_t *tid_id);
void write_node(long pagenum, register bt_node_t *buffer);
void get_node(long pagenum, register bt_node_t *buffer);
void tracepath(long rootpage, register locator_t *block, register int inc);
void return_page(long pagenum);
void repl_btree(register char *replbatch, long count);
int parentkey(long block, register locator_t *prt);
long last_lid(long rootpage);
void insert_btree(char *tree, long rootpage, long lid, long *tid, register tid_t *tidpos, char create);
#endif /* INGRES_BTREE_H_ */
#ifdef INGRES_AUX_H_
int paramd(desc_t *d, acc_param_t *ap);
#ifdef INGRES_CATALOG_H_
int parami(index_t *ip, acc_param_t *param);
#endif /* INGRES_CATALOG_H_ */
#endif /* INGRES_AUX_H_ */
#ifdef INGRES_USEFUL_H_
void closecatalog(bool really);
#endif /* INGRES_USEFUL_H_ */
#endif

#ifdef INGRES_GUTIL
char *iocv(int i);
char *need(char *bf, int nbytes);
char *ztack(char *pathname, char *file);
void bmove(void *src, void *dest, int len);
void syserr(char *fmt, ...);
void lprintf(char *fmt, ...);
void setprocname(char *s);
char *getprocname(void);
int markbuf(void *bf);
void freebuf(void *bf, int bytes);
void seterr(void *bf, int errnum, int (*err_func)(int, int));
void *xalloc(int sz, char zeroit, char errquit);
void *xrealloc(void *p, int sz);
void xfree(void *p);
void clrmem(register void *p, register int l);
void xputchar(register char c);
int set_so_buf(void);
void prargs(int pc, char **pv);
void initbuf(char *bf, int size, int err_num, int (*err_func)(int, int));
int tTrace(char **argv, char tflag, short *tvect, int tsize);
int tTamper(char *line, char tflag, short *tvect, int tsize);
void tTfp(int m, int n, char *fmt, ...);
int itoa(register int i, register char *a);
int bequal(register void *a, register void *b, register int l);
char *pmove(char *s1, char *b1, int l1, char c);
void ingres_perror(char *s);
int ingres_atof(char *str, double *val);
int smove(register char *a, register char *b);
char *getufield(char *buf, int num);
char *concat(char *s1, char *s2, char *s3);
int oatoi(char *a);
void capital(char *lower, char *upper);
int scompare(void *a_ptr, int a_len, void *b_ptr, int b_len);
char *locv(long i);
int ftoa(double value, char *ascii, int width, int prec, char ch);
void pad(char *s, int n);
int fullwait(int child, char *name);
int ingres_atol(register char *a, long *i);
int bitcnt(register int var);
int setflag(char **argv, char flagch, int def);
int bitpos(register int wd);
int cat(char *file);
int set_si_buf(void);
void noise(int n);
#ifndef HAVE_STRERROR
char *strerror(int errnum);
#endif /* !HAVE_STRERROR */
void setglobalptr(char *name, void *val);
void *getglobalptr(char *name);
void setglobalint(char *name, int i);
int getglobalint(char *name);
#ifdef INGRES_RESP_H_
resp_t *getresp(void);
#endif /* INGRES_RESP_H_ */
#ifdef INGRES_INGRES_H_
int tid2int(tid_t t);
void int2tid(tid_t *tp, int i);
#endif /* INGRES_INGRES_H_ */
#endif

#ifdef INGRES_DECOMP
qtree_t *makavar(qtree_t *node, int varnum, int attnum);
qtree_t **mksqlist(qtree_t *tree, int var);
desc_t *openr1(int var);
qtree_t *ckvar(register qtree_t *t);
void de_init(int argc, char **argv);
void init_decomp(void);
void startdecomp(void);
int endovqp(int ack);
void reinit(void);
int findwid(qtree_t *tree);
void derror(int eno);
int mak_t_rel(qtree_t *tree, char *prefix, int rnum);
void removedups(int var);
int call_ovqp(register qtree_t *tree, int mode, int resultnum);
int mapvar(register qtree_t *t, int tl);
void openrs(qtree_t *root);
void closers(void);
void closer1(int var);
void initrange(void);
void savrang(int *locrang, int var);
void rstrang(int *locrang, int var);
int new_range(int var, int relnum);
void newquery(int *locrang);
void endquery(int *locrang, int reopen);
char *rangename(int var);
void de_rubproc(void);
desc_t *readopen(int var);
desc_t *writeopen(int var);
char *rnum_convert(int num);
int rnum_assign(char *name);
int rnum_alloc(void);
int rnum_last(void);
int rnum_findadd(char *name);
void rnum_init(void);
void rnum_remove(int num);
void initdesc(int mode);
void lockit(qtree_t *root, int resvar);
void mk_unique(qtree_t *root);
void aggregate(qtree_t *root);
int decomp(qtree_t *q, int qmode, int result_num);
void pr_unique(qtree_t *root1, int var1);
int call_dbu(int code, bool errflag);
void init_decomp(void);
int lnode(qtree_t *nod, qtree_t **lnodv, int count);
qtree_t *ageval(qtree_t *tree, qtree_t **result);
qtree_t *byeval(qtree_t *root, qtree_t *aghead, int agvar);
int prime(qtree_t *aop);
void dstr_rel(int relnum);
void readagg_result(qtree_t **result);
qtree_t *copytree(register qtree_t *r, char *buf);
void domnam(qtree_t **lnp, char *pre);
int varfind(qtree_t *root, qtree_t *aghead);
qtree_t *makroot(char *buf);
qtree_t *makresdom(char *buf, qtree_t *node);
void specclose(int relnum);
desc_t *specopen(int relnum);
void pull_sq(qtree_t *tree1, qtree_t **sqlist, int *locrang, int *sqrange, char *buf);
int exec_sq(qtree_t **sqlist, int *sqrange, int *disj);
void undo_sq(qtree_t **sqlist, int *locrang, int *sqrange, int limit, int maxlimit, int reopen);
void reset_sq(qtree_t **sqlist, int *locrang, int limit);
int decision(qtree_t *root, int qmode, int result_num, char *buf);
int selectv(register qtree_t *root);
void setvar(register qtree_t *tree, int var, tid_t *intid, char *tuple);
void reformat(int var, qtree_t **sqlist, int *locrang, char *buf, qtree_t *tree);
int pull_const(qtree_t *root, char *buf);
qtree_t *copy_ands(qtree_t *root, char *buf);
void origvar(register qtree_t *t, qtree_t **sqlist);
void clearvar(register qtree_t *tree, register int var);
void tempvar(register qtree_t *node, qtree_t **sqlist, char *buf);
void dfind(register qtree_t *tree, char *buf, qtree_t **sqlist);
int execsq1(qtree_t *sq, int var, int relnum);
int dstr_mark(int relnum);
int findlinks(qtree_t *node, int selv, int linkv, char *linkmap);
void prlinks(char *label, char *linkmap);
long rel_pages(long tupcnt, int width);
int ckpkey(char *linkmap, int var);
int decompy(qtree_t *q, int qmode, int result_num, char *sqbuf);
void mklist(qtree_t *tree);
void ovqpnod(register qtree_t *q);
int rnum_temp(int rnum);
void ov_err(int code);
desc_t *openindex(char *name);
#ifdef INGRES_DECOMP_GLOBS_H_
comp_list_t *buildlist(qtree_t *root1, char *buf);
int algorithm(comp_list_t *clist, int varmap);
comp_list_t *order(comp_list_t *clist, int ovlapvar);
qtree_t *construct(qtree_t *root, comp_list_t *listhead, char *buf);
#endif /* INGRES_DECOMP_GLOBS_H_ */
#ifdef INGRES_PV_H_
int qryproc(int pc, paramv_t *pv);
#endif /* !INGRES_PV_H_ */
#endif

#ifdef INGRES_MONITOR
char *getfilenm(void);
char *getname(void);
char *macmocv(char *m);
char *macplkup(char name);
char *macprim(int n);
char *macro(char *name);
char *macsstr(int from, int to, char *string);
char *mactcvt(int raw, char *paramdefined);
char *mcall(char *mac);
char getch(void);
int exp_op(int op, int lv, int rv);
int expr(void);
int exprfind(void);
int exprgch(void);
int getescape(int copy);
int macallscan(void);
int macckch(void);
int macdelim(void);
int macfetch(int quote);
int macgch(void);
int macgetch(void);
int macmatch(char template);
int macmmatch(char *name, char *temp, int flag);
int macmmchew(char **pp);
int macmode(char ch);
int macnumber(char *s);
int macparam(char mode, char name, char delim);
int macsget(char **pp);
int numberget(char cx);
int opfind(void);
int pageflush(char *x);
int popnum(void);
int popop(void);
int tm_mon(void);
int valof(int terminator);
void branch(void);
int branchto(char *label);
void cgprompt(void);
void clear(char f);
void clrline(int noprompt);
void edit(void);
void eval(int pr);
void go(void);
void include(char *filename);
void macdefine(char *template, char *subs, int raw);
void macdnl(void);
void macdump(char *name);
void macinit(int (*rawget)(char **val), char **rawpar, int endtrap);
void macload(char *str, int flag);
void macnewev(int (*rawget)(char **val), char **rawpar);
void macpopev(void);
void macprescan(char **pp);
void macread(void);
void macremove(char *name);
void macrescan(void);
void macschng(char ch);
void macspring(char *trap);
void macunget(int mask);
void monitor(bool recurs);
void newdirec(void);
void print(void);
void prompt(char *msg);
void pushnum(int num);
void pushop(int op);
void putch(char ch);
RETSIGTYPE quit(int i);
void reset(void);
void shell(void);
void tm_init(int argc, char **argv);
void tm_intr(int typ);
void trace(void);
void writeout(void);
void xwait(void);
#ifdef INGRES_MONITOR_BUF_H_
char *bufalloc(int size);
char *bufcrunch(buf_t **buffer);
char *bufflatten(buf_t *buf, int length);
char bufget(buf_t **buffer);
void bufflush(buf_t **buffer);
void buffree(char *ptr);
void bufpurge(buf_t **buffer);
void bufput(char c, buf_t **buffer);
#endif /* INGRES_MONITOR_BUF_H_ */
#ifdef INGRES_PV_H_
#ifdef INGRES_CTLMOD_PIPES_H_
int proc_err(pb_t *ppb, int pc, paramv_t *pv);
#endif /* INGRES_CTLMOD_PIPES_H_ */
#endif /* INGRES_PV_H_ */
#ifdef INGRES_RESP_H_
void trapquery(resp_t *resp, char *name);
#endif /* INGRES_RESP_H_ */
#endif

#ifdef INGRES_CTLMOD
int error(int num, ...);
int nferror(int num, ...);
void initp(void);
void setp(register int type, void *val, register int len);
void rubproc(void);
void clrrange(void);
int av_files(void);
void resetp(void);
void init_qt(void);
int readmon(char *buf, int nbytes);
int call(int entpt, int (*errfn)(void));
int calln(int entpt, int (*errfn)(void));
void cm_cleanup(int typ);
void cm_close(void);
void cm_reset(void);
char *cvt_time(long t);
int readmon(char *buf, int nbytes);
void dump_cm(int argc, char **argv);
void writesym(int typ, int len, char *value, int (*wrfn)(), char *fnparam);
#ifdef INGRES_PV_H_
void prvect(int pc, register paramv_t *pv);
paramv_t *getp(void);
void pr_parm(register paramv_t *pv);
void call_fn(int fno, int pc, paramv_t *pv);
int sysdump(int pc, register paramv_t *pv);
#ifdef INGRES_CTLMOD_PIPES_H_
int do_st(register pb_t *ppb, int pc, paramv_t *pv);
int proc_err(pb_t *ppb, int pc, paramv_t *pv);
int read_arg(register pb_t *ppb, register paramv_t *pparm);
void send_arg(register paramv_t *pparm, register pb_t *ppb);
pb_t *getmonppb(void);
void setmonppb(pb_t *pp);
#endif /* INGRES_CTLMOD_PIPES_H_ */
#endif /* INGRES_PV_H_ */
#ifdef INGRES_RANGE_H_
int declare(int varno, desc_t *desc);
#endif /* INGRES_RANGE_H_ */
#ifdef INGRES_TREE_H_
void nodepr(qtree_t *tree);
void mapvars(qtree_t *tree);
void treepr(qtree_t *tree);
bool leaf(qtree_t *p);
void writeqry(qtree_t *root, int (*wrfn)(), char *fnparam);
void writetree(qtree_t *q1, int (*wrfn)(), char *fnparam);
qtree_t *gettree(char *treerelid, char *treeowner, char treetype, int treeid, int init);
#ifdef INGRES_CTLMOD_PIPES_H_
qtree_t *readqry(int (*rdfn)(pb_t *s, char *nam, int cc), int fnparam, bool init);
qtree_t *readsym(int (*rdfn)(pb_t *s, char *nam, int cc), char *fnparam);
qtree_t *readtree(qtree_t *tresym, int (*rdfn)(pb_t *s, char *nam, int cc), char *fnparam);
qtree_t *trbuild(qtree_t *bufptr);
int pb_get(register pb_t *ppb, char *dp, register int len);
void pb_dump(register pb_t *ppb, int full);
void pb_put(register char *dp, register int len, register pb_t *ppb);
void pb_prime(register pb_t *ppb, int type);
void pb_flush(register pb_t *ppb);
void pb_read(register pb_t *ppb);
void pb_write(register pb_t *ppb);
int pb_rphys(register pb_t *ppb, register int fd);
void pb_wphys(register pb_t *ppb, register int fd);
void pb_tput(int tag, char *dp, int len, register pb_t *ppb);
void pb_trace(pb_t *ppb);
void readinput(register pb_t *ppb);
void call_setup(register pb_t *ppb, int state, int (*errfn)(void));
void do_seq(register pb_t *ppb);
#ifdef INGRES_PV_H_
void send_off(register pb_t *ppb, int pc, register paramv_t *pv);
#endif /* INGRES_PV_H_ */
#endif /* INGRES_CTLMOD_PIPES_H_ */
#endif /* INGRES_TREE_H_ */
#ifdef INGRES_PMON_H_
monitor_t *markperf(register monitor_t *mbuf);
void add_mon(register monitor_t *a, register monitor_t *b);
#endif /* INGRES_PMON_H_ */
#endif

#ifdef INGRES_LIBQ
int IIret_err(int err);
RETSIGTYPE IIresync(int n);
void (*IIinterrupt)(int n);
char *IIerrfilen(int num, char *buf);
char *IIconcatv(char *str, ...);
char *IIitos(int i);
void IIexit(void);
char *IIbmove(char *s, char *d, int l);
char *IIneed(register int i);
int IIatoi(char *a1, int *i);
int IIconvert(char *inp, char *outp, int sf, int slen, int df, int dlen);
int IIerrtest(void);
int IIgettup(char *file_name, int line_no);
int IIlength(char *s);
int IIn_get(char *file_name, ...);
int IIno_err(int err);
int IIret_err(int err);
int IIsequal(char *s1, char *s2);
void IIcvar(char *obj, int type, int len);
void IIerror(int errno, int argc, char **argv);
void IIflushtup(char *file_name, ...);
void IIgetpath(void);
void IIingres(char *p1, ...);
int IIn_ret(void *addr, int type);
void IInd_init(void);
int IIp_err(int err_num, int argc, char **argv);
void IIretrieve(char *addr, int type);
void IIsetup(void);
void IIsync(char *file_name, ...);
void IIsyserr(char *fmt, ...);
void IIw_right(char *string, char **argv);
void IIwrite(char *str);
#ifdef INGRES_CTLMOD_PIPES_H_
void IIpb_flush(register pb_t *ppb);
void IIpb_prime(register pb_t *ppb, int type);
void IIpb_put(register char *dp, register int len, register pb_t *ppb);
void IIpb_read(register pb_t *ppb);
void IIpb_write(register pb_t *ppb);
void IIreadinput(register pb_t *ppb);
int IIpb_get(register pb_t *ppb, void *dp, register int len);
int IIpb_rphys(register pb_t *ppb, register int fd);
void IIpb_wphys(register pb_t *ppb, int fd);
int IIread_arg(register pb_t *ppb, char **parm);
#endif /* INGRES_CTLMOD_PIPES_H_ */
#ifdef INGRES_IIGLOBALS_H_
int IIclose(iob_t *buf);
int IIfopen(char *file, iob_t *iobuf);
int IIgetc(iob_t *iobuf);
#endif /* INGRES_IIGLOBALS_H_ */
#endif

#ifdef INGRES_PARSER
int parser_cmap(char ch);
int ack_err(void);
void attinit(void);
void call_tree(register int qmode, int dest, int (*err_fcn)(void));
int init_quelst(void);
int endquelst(register int op);
int startgo(void);
void endgo(void);
void endretrieve(int (*err_fcn)(void));
int printtrail(void);
int get_scan(int mode);
void attinit(void);
void par_init(int argc, char **argv1);
int timeofday(short *hrs, short *mins);
int patmat(char *str);
void permcom(int a);
char *makestr(register char *str);
int qualindex(void);
int parser(void);
void ctlmod_decl(int slot);
void rnginit(void);
int rnglook(char *name, int type);
void rngdel(register char *rel);
void rngfront(int slot);
void rngback(int slot);
void rngget(int slot);
int rngold(void);
void rngreset(void);
void checkupd(int entnum);
void rngfresh(int op);
void printtable(void);
void printslot(int slot);
int comment(void);
void backup(char chr);
int name(char chr);
int number(char chr);
int operator(char chr);
char *syment(void *ptr, int len1);
void freesym(void);
int yylex(void);
int check_bnf(char *str);
char *make_dmap(char *str, int len, char *b);
void set(char *map, int n);
void reset(char *map, int n);
int test(char *map, int n);
void predef_delims(void);
int shrink_list(char *group);
void par_error(int num, int result, char *a, char *b, char *c);
void neederr(int errnum);
void yyerror(char *errmessage);
int attcount(int slot);
#ifdef INGRES_TREE_H_
qtree_t *xdot(int slot);
void format(qtree_t *result1);
qtree_t *norm(register qtree_t *p);
qtree_t *travers(qtree_t *p1);
qtree_t *nnorm(qtree_t *p1);
qtree_t *notpush(qtree_t *p1);
void adjust(qtree_t **pp);
qtree_t *treedup(qtree_t *p1);
qtree_t *tlprepend(qtree_t *a, qtree_t *b);
void windup(qtree_t *ptr);
qtree_t *addresdom(qtree_t *lptr, qtree_t *rptr);
STRKEEPER *substring(char *str,int isname);
STRKEEPER *endvals(STRKEEPER *interval, int left, int right);
void setnumber(STRKEEPER *interval, short *num);
void groupstrings(STRKEEPER *left, STRKEEPER *right);
qtree_t *norml(qtree_t *ptr);
#ifdef INGRES_PARSER_PARSER_H_
qtree_t *par_tree(qtree_t *lptr, qtree_t *rptr, char typ, int len, register int valu, register att_ent_t *attnum);
att_ent_t *attfind(int slot, register char *attrib);
att_ent_t *attadd(int slot, attr_t *tuple);
att_ent_t *attlookup(int slot, char *attrib);
void attcheck(register att_ent_t *aptr);
void attfree(att_ent_t *aptr);
#endif /* INGRES_PARSER_PARSER_H_ */
#endif /* INGRES_TREE_H_ */
#ifdef INGRES_RANGE_H_
int make_tuples(desc_t *desc, char *group, char *delim, char *str);
void create_tup(desc_t *desc, int order, char *group, char *delim, int type, char *str, int strlen);
int make_list(desc_t *desc, char *group);
int destroy_delim(desc_t *desc, char *group);
int rngent(int type, char *var, register desc_t *desc);
#endif /* INGRES_RANGE_H_ */
#ifdef INGRES_PV_H_
void header(paramv_t *pv);
#endif /* INGRES_PV_H_ */
#ifdef INGRES_INGRES_H_
int prlist(struct delimlist *d);
#endif /* INGRES_INGRES_H_ */
#ifdef INGRES_PARSER_SCANNER_H_
int string(struct optab *op);
#endif /* INGRES_PARSER_SCANNER_H_ */
#endif

#ifdef INGRES_EQUEL
int equel_cmap(char ch);
struct disp_node *addsym(char *s);
char **argproc(char **argv);
char *nalloc(int s);
char *salloc(char *s);
int getch(void);
int if_c_code(char chr);
int include(char *buf, char *start, char *chp, char *end);
int name(char chr);
int number(char chr);
int operator(char chr);
int restoref(void);
int tst_include(void);
void xfree(void *cp);
int yylex(void);
int yyparse(void);
void add_ret(char *s, int type);
void backup(char ch);
void begin_quote(void);
void cant(char *operation, char *filename);
void copy_c_code(void);
void decl_cvar(char *name, int type, int indir_level, int block_level);
void decl_field(char *name, int type, int indir_level, int block_level);
void end_quote(void);
void equate_lines(void);
void equel(char *filename);
void free_ret(void);
void w_con(int type, char *string);
void w_file(void);
void w_flush(void);
void w_key(char *string);
void w_new(char *string);
void w_op(char *string);
void w_raw(char *string);
void w_ret(void);
void w_string(char *string, int type);
void w_sync(void);
void yyerror(char *s);
void yysemerr(char *s, char *i);
void yyserror(char *s, struct disp_node *d);
int comment(void);
void symspfree(void);
#ifdef INGRES_EQUEL_GLOBALS_H_
struct optab *getkey(char *key);
int string(struct optab *op);
void eat_display(struct display *disp, char left_ch, char right_ch);
void w_display(struct display *disp);
void free_display(struct display *disp);
struct disp_node *enter_display(struct display *disp, char *string);
void enter_ret(struct display *disp, int type);
void w_var(struct display *disp, int type);
cvar_t *getcvar(char *name);
cvar_t *getfield(char *id);
cvar_t *dec_var(char *name, int type, int indir_level, int block_level, cvar_t **local_tree, cvar_t **global_tree);
cvar_t *get_var(char *id, cvar_t *local_tree, cvar_t *global_tree);
cvar_t *getcvar(char *id);
cvar_t *getfield(char *id);
int c_enter(cvar_t *node, cvar_t **root);
void f_cvar(cvar_t *root);
void freecvar(cvar_t **rootp);
#endif /*  INGRES_EQUEL_GLOBALS_H_ */
#endif

#ifdef INGRES_DBU
int modupdate(void);
void purgetup(register desc_t *d, char *key1, int dom1, char *key2, int dom2);
void userdestroy(relation_t *reltup);
int convert(void *inp, void *outp, int sf, int slen, int df, int dlen);
void secupdate(register desc_t *r);
int getbatch(void *loc, int count);
void putbatch(char *cp, int count);
void batchflush(void);
int readbatch(void);
void btreeupdate(register desc_t *r);
void get_p_tid(register desc_t *d, register tid_t *tp);
int seq_attributes(register desc_t *a, register desc_t *r, register attr_t *atup);
void seq_init(register desc_t *a, register desc_t *r);
void pr_range(void);
void pr_rv(register int re);
void pr_attname(char *rel, int attno);
#ifdef INGRES_PV_H_
int create(int pc, paramv_t *pv);
int destroy(int pc, paramv_t *pv);
int copy(int pc, paramv_t *pv);
int print(int parmc, paramv_t *parmv);
int rupdate(int pc, paramv_t *pv);
int help(int parmc, paramv_t *parmv);
int save(int parmc, paramv_t *parmv);
int resetrel(int pc, paramv_t *pv);
int sysfunc(int pc, paramv_t *pv);
int display(int pc, paramv_t *pv);
int ksort(int pc, paramv_t *pv);
int update(int pc, paramv_t *pv);
int indexx(int pc, paramv_t *pv);
int dest_const(int pc, paramv_t *pv);
#endif /* INGRES_PV_H_ */
#ifdef INGRES_TREE_H_
void pr_tree(qtree_t *root);
void pr_qual(register qtree_t *q);
#endif /* INGRES_TREE_H_ */
#ifdef INGRES_INGRES_H_
int pr_prot(char *relid, register relation_t *r);
#endif /* INGRES_INGRES_H_ */
#endif

#ifdef INGRES_SUPPORT
int test(char *map, int n);
char *getnxtdb(void);
int initialize(int argc, char **argv);
int ask(char *prompt);
int init_socket(void);
#endif

#ifdef INGRES_OVQP
int strategy(void);
int scan(void);
void equeleol(int code);
void startovqp(void);
/* privates */
#ifdef INGRES_TREE_H_
sym_t *interpret(int istlist, sym_t **list);
char *grabstring(STRKEEPER *strinfo, char *str, int *len, int *startptr);
int addbatch(tid_t *oldtid, char *newtuple, char *oldtuple);
int btreekey(key_t *lkey, key_t *hkey);
int check(sym_t *sym);
int closebatch(void);
int cmove(sym_t *sym, char *dest);
int findsimps(void);
int getend(int len, int dropend, int howmany);
int getsymbol(sym_t *ts, sym_t ***p);
int indexcheck(void);
int keycheck(key_t *keys, char *keytuple, int mode);
int lexcomp(register char *s1, register int l1, register char *s2, register int l2, int x);
int lmatch(char *pat, int plen, char *str, int slen);
int openbatch(desc_t *rel_desc, desc_t *index_desc, int mode);
int pmatch(char patarg, char *pat, int plen,  char *str, int slength);
int relop(sym_t *s, int reverse);
int relop_interp(int opval, int l1);
int scanstr(char *str, int slen, char *pat, int plen, int getstart, char num);
int setallkey(key_t *relkey, char *keytuple);
int size(register sym_t *s);
int specdelim(char *str, int slen, char *dname, int getstart, char num, int *plen);
int strategy(void);
int tid_only_test(void);
sym_t *cpsym(sym_t *constv, int len, int op);
void add_glob(char *str, int slen);
void aop_interp(int opval, register sym_t *tos);
void ascii(register sym_t *s);
void backlink(char *pat, int plen);
void createlink(char *pat, int plen);
void dec_tid(tid_t *tid);
void equelatt(sym_t *ss);
void f8tof4(sym_t *pp);
void ftoi2(register sym_t *p);
void ftoi4(register sym_t *p);
int gmatch(register char *s1, int l1,register char *s2, int l2);
void i2toi4(sym_t *pp);
void i4toi2(sym_t *pp);
void insert_chars(sym_t *op);
void insert_glob(char **start, char *rest, int *slen, int rlen);
void itof(register sym_t *p);
void newstring(register sym_t *op1, register sym_t *op2);
void pat_insert(char *str, int slen, char where, int no_blanks);
void prstack(register sym_t *s);
void prstr(register char *p, register int l);
void prsym(register sym_t *s);
void pwritesym(register sym_t *s);
void rcvt(register sym_t *tos, int restype, int reslen);
void startequel(void);
void tout(register sym_t *s, char *offp, int rlen);
void typecheck(sym_t *pp1, sym_t *pp2, int opval);
void typecoerce(sym_t *tosx, int ntype, int nlen);
int add_simp(sym_t *constv, int rel, int attno);
void prsimp(simp_t *ss);
void concatsym(register sym_t *s1, register sym_t *s2);
#endif /* INGRES_TREE_H_ */
#ifdef INGRES_AUX_H_
int exactkey(acc_param_t *ap, key_t *key);
int rangekey(acc_param_t *ap, key_t *l, key_t *h);
#endif /*  INGRES_AUX_H_ */
#endif

#ifdef INGRES_QRYMOD
int aggcheck(qtree_t *root);
int cvt_dow(char *sdow);
int isttyname(register char *n);
int puttree(qtree_t *root, char *trelid, char *towner, char ttype);
int relntrwr(char *ptr, int len, char *treerelid, char *treeowner, int treetype, int treeid);
int varset(qtree_t *root);
qtree_t *integrity(qtree_t *root);
qtree_t *makezero(int typ);
qtree_t *nnorm(qtree_t *p1);
qtree_t *norm(qtree_t *p1);
qtree_t *norml(qtree_t *ptr);
qtree_t *notpush(qtree_t *p1);
qtree_t *protect(qtree_t *root);
qtree_t *qscan(qtree_t *root, int vn, int an);
qtree_t *travers(qtree_t *p1);
qtree_t *treedup(register qtree_t *p);
qtree_t *trimqlend(qtree_t *qual);
qtree_t *vfind(int vn, qtree_t *vtree);
qtree_t *view(qtree_t *root);
qtree_t *qm_tree(qtree_t *lptr, qtree_t *rptr, char typ, int len, int value);
void adjust(qtree_t **pp);
void appqual(qtree_t *qual, qtree_t *root);
void dopro(int varno, qtree_t *root, int qmode, int *byset);
void issue(int state, qtree_t *tree);
void issueinvert(qtree_t *root);
void lsetbit(int bitno, long *xset);
void makedset(int vn, qtree_t *tree, int *uset, int *rset, int *aset, int *qset);
void makeidset(int varno, qtree_t *tree, short *dset);
void mergevar(register int a, register int b, qtree_t *root);
void mvands(qtree_t *andp);
void pr_set(int *xset, char *labl);
void qm_init(int argc, char **argv);
void qmerror(int errno, int qmode, int vn, ...);
void subsvars(qtree_t **proot, int vn, qtree_t *transtree, int vmode);
void treeprn(qtree_t *p, char *str);
void vrscan(qtree_t *root, qtree_t *vtree);
#ifdef INGRES_PV_H_
int d_integ(int pc, paramv_t *pv);
int d_integ(int pc, paramv_t *pv);
int d_prot(int pc, paramv_t *pv);
int d_view(int pc, paramv_t *pv);
int qrymod(int pc, paramv_t *pv);
#endif
#ifdef INGRES_CATALOG_H_
int proappl(protect_t *protup);
int prochk(int inbit, int *domset, protect_t *protup);
#endif
#endif

#endif /* INGRES_PROTOTYPES_H_ */
