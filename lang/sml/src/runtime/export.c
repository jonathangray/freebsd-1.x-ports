/* export.c
 *
 * COPYRIGHT (c) 1989 by AT&T Bell Laboratories.
 */

#include "tags.h"
#include "ml_os.h"

#define bulletproofWrite(a,b,c) if (bulletproofWrite0(a,b,c)) return 1;else{}

#ifndef SOLARIS
#ifdef THINK_C
#include <unix.h>
#include <fcntl.h>
#include <errno.h>
#else
#ifdef NeXT
#ifdef NeXT_3_0
#include <mach-o/loader.h>
#else
#include <sys/loader.h>
#endif
#else
#include <a.out.h>
#endif NeXT
#endif THINK_C

#define CEIL(x,quantum) ((((int)(x))+(quantum)-1)&~((quantum)-1))

#ifdef FreeBSD
#undef BSD386
#endif

#ifdef MIPS
#define N_DATADDR 0x10000000
#define N_TXTADDR 0x400000 /* start of headers; text segment follows */
#define getpagesize() 4096
#endif

#ifdef AUX
#define N_TXTADDR 0x0 /* start of headers; text segment follows */
#define N_DATADDR ah->data_start
#endif

#ifdef HPUX
#  define N_DATADDR DATA_OFFSET
#endif HPUX

#ifdef NS32
#  define N_DATADDR(x)	0x400000
#endif NS32

#if defined(I386) && defined(MACH)
#  define N_DATADDR(x)	ETEXT
#endif

#ifndef N_DATADDR
#  define N_DATADDR(x)  CEIL((x).a_text,getpagesize())
#endif

#if !defined(N_TXTADDR)
#  ifdef NeXT
#    ifdef NeXT_3_0
#      include <mach/m68k/vm_param.h>
#    else
#      include <machine/vm_param.h>
#    endif
#    ifdef USRTEXT
#      define N_TXTADDR(x)  USRTEXT
#    endif USRTEXT
#  endif
#  ifdef VAX
#    define N_TXTADDR(x)  0
#  endif
#  ifdef NS32
#    define N_TXTADDR(x)  0
#    define exec aouthdr
#    define a_magic		magic
#    define	a_text		tsize
#    define a_data		dsize
#    define	a_bss		bsize
#    define	a_entry		entry
#    define	a_trsize	text_start
#    define	a_drsize	data_start
#  endif
#  ifdef SPARC
#    include <machine/vmparam.h>
#    define N_TXTADDR(x)  USRTEXT
#  endif
#  ifdef I386
#    ifdef DYNIX
#     define N_TXTADDR(x) N_ADDRADJ(x)
#    else
#     define N_TXTADDR(x) ((int) startptr)
#    endif
#  endif
#  ifndef sony_news
#    ifdef M68
#      ifdef HPUX
#        define N_TXTADDR TEXT_OFFSET
#      else
#        ifdef MORE
#          define N_TXTADDR(x) 0
#        else
#          define N_TXTADDR(x)  getpagesize()
#        endif MORE
#      endif HPUX
#    endif M68
#  else sony_news
#    define N_TXTADDR(x)  0
#  endif /* !sony_news */
#endif /* !N_TXTADDR */

#ifdef HPUX
#  define ZMAGIC SHARE_MAGIC
#endif HPUX

#ifdef AUX
#  define ZMAGIC 0413
#endif

#ifdef BSD386
# ifdef ZMAGIC
# undef ZMAGIC
# endif ZMAGIC
#define ZMAGIC QMAGIC
#endif BSD386

 
/* Garbage collection is already done.
 * Data to be saved is:
 *  0 -> ceil(etext)		 text
 *  ceil(etext) -> arenabase	 data
 *  arenabase -> old_high	 heap
 *  
 *  > set a_entry as address of start procedure
 */

extern int old_high;

static int textstart,datastart;

#ifdef MIPS
extern long startptr[];		/* fool damn global pointer stuff */
#else
extern int startptr;
#endif

#if defined(SPARC) && defined(MACH)
getpagesize(){return 8192;}
#endif

#if defined(HPUX) || defined(AUX)
getpagesize(){return 4096;}
#endif /* HPUX || AUX */

#ifdef V9
getpagesize(){return 1024;}
#endif

#if defined(VAX) || defined(MIPS) || defined(SPARC) || defined(M68) || defined(I386)
#ifndef THINK_C
#ifndef NeXT
#ifndef MIPS
#ifndef AUX
int export (filid)  /* nonzero return means error occurred, check errno */
    int filid;
{
    int bytcount;
   static struct exec E;  /* make it static so all fields=0 */

#ifdef HPUX
    E.a_magic.file_type = ZMAGIC;
    E.a_magic.system_id = HP9000S200_ID;
#ifdef M_DATA_WTHRU
    E.a_miscinfo |= M_DATA_WTHRU;
#endif
#else
#if defined(sun3) || (defined(SPARC) && !defined(MACH))
    E.a_magic = NMAGIC;
#else
#  if defined(NS32)
    E.a_magic = NS32GMAGIC;
#  else
    E.a_magic = ZMAGIC;
#  endif
#endif
#endif

#if defined(M68) && (! defined(HPUX))
#  ifndef sony_news
    E.a_machtype = 2;	/* M_68020 */
#  endif sony_news
#  ifdef MORE
    E.a_machtype = MID_HP300; /* M_68020 */
#  else
    E.a_machtype = M_68020; /* M_68020 */
#  endif MORE
#endif
#ifdef SPARC
    E.a_toolversion = 1;
    E.a_machtype = M_SPARC;
#endif

    textstart = N_TXTADDR(E);
#ifdef HPUX
    E.a_text = (int) CEIL((ETEXT+textstart),getpagesize())-textstart;
#else
# if defined(DYNIX)
    E.a_text = (int) CEIL(ETEXT,getpagesize());
# else
    E.a_text = (int) CEIL(ETEXT,getpagesize())-textstart;
# endif
#if defined(SPARC) && defined(MACH)
    datastart = (int) CEIL(ETEXT,getpagesize());
#else
    datastart = N_DATADDR(E);
#endif /* !defined(SPARC) || !defined(MACH) */
    E.a_bss = 0;
#ifndef NS32
    E.a_syms = 0;
#endif NS32
#endif HPUX
    E.a_entry = startptr;
    E.a_trsize = 0;
    E.a_drsize = 0;
#ifdef HPUX
    E.a_data = CEIL(old_high-ETEXT, getpagesize());
#else
    E.a_data = CEIL(old_high-datastart, getpagesize());
#endif HPUX
    fchmod(filid,0755);
#ifdef NS32
    coff_cough (filid, &bytcount, E.a_text, E.a_data, E.a_entry);
#else
    bulletproofWrite(filid,&E,sizeof(E));
    bytcount = sizeof(E);
#endif NS32
#if VAX || NS32
    {int i, nzeros = getpagesize()-sizeof(E);
	char zeros[1024];
        for(i=0;i<nzeros;i++) zeros[i]=0;
        bulletproofWrite(filid,zeros,nzeros);

    }
#endif
#ifdef FreeBSD
    {int i, nzeros = getpagesize()-sizeof(E);
	char zeros[__LDPGSZ];
        for(i=0;i<nzeros;i++) zeros[i]=0;
        bulletproofWrite(filid,zeros,nzeros);
    }
#endif
#if sony_news || MORE
    {int i, nzeros = getpagesize()-bytcount;
     char zeros[4096];
     for(i=0;i<nzeros;i++) zeros[i]=0;
     bulletproofWrite(filid,zeros,nzeros);
    }
#endif sony_news
#ifdef HPUX
    lseek(filid,N_TXTADDR(E),0);
    bulletproofWrite(filid,0,E.a_text);
    lseek(filid,N_DATADDR(E),0);
    bulletproofWrite(filid,CEIL(ETEXT,getpagesize()),E.a_data);
#else
#if defined(SPARC) && defined(MACH) || defined(BSD386)
    bulletproofWrite(filid,textstart+sizeof(E),E.a_text-sizeof(E));
#else
# if defined(DYNIX)
     bulletproofWrite(filid,textstart+sizeof(E),E.a_text-sizeof(E)-N_ADDRADJ(E))
# else
     bulletproofWrite(filid,textstart,E.a_text);
# endif
#endif /* !defined(SPARC) || !defined(MACH) && !defined(BSD386) */
    bulletproofWrite(filid,datastart,E.a_data);
#endif HPUX
    return 0;
}

#ifdef NS32
coff_cough(fd,countp,tsize,dsize,entry)
int fd, *countp, tsize, dsize, entry;
{
static struct filehdr fhdr;
static struct aouthdr ahdr;
static struct scnhdr thdr;
static struct scnhdr dhdr;
static struct scnhdr bhdr;
int allhdrsize = sizeof(fhdr) + sizeof(ahdr) + 3*sizeof(thdr);
int pagesize = getpagesize();

fhdr.f_magic = NS32GMAGIC;
fhdr.f_nscns = 3;
fhdr.f_timdat = /* don't care */ 0;
fhdr.f_symptr = /* null? */ 0;
fhdr.f_nsyms = 0;
fhdr.f_opthdr = sizeof(struct aouthdr);
fhdr.f_flags = F_RELFLG|F_EXEC|F_LNNO|F_LSYMS|F_AR32WR;

ahdr.magic = /* OOPS */ 0x010b;
ahdr.vstamp = /* don't care */ 0;
ahdr.tsize = tsize;
ahdr.dsize = dsize;
ahdr.bsize = 0;
ahdr.msize = /* OOPS */ 0x10;
ahdr.mod_start = /* OOPS */ 0x20;
ahdr.entry = entry;
ahdr.text_start = 0;
ahdr.data_start = 0x400000;
ahdr.entry_mod = /* unused? */ 0;
ahdr.flags = U_SYS_42|U_AL_4096;

strncpy(thdr.s_name,_TEXT,8);
thdr.s_paddr = thdr.s_vaddr = ahdr.data_start;
thdr.s_size = ahdr.tsize;
thdr.s_scnptr = CEIL(allhdrsize,pagesize);
thdr.s_relptr = /* null? */ 0;
thdr.s_lnnoptr = /* null? */ 0;
thdr.s_nreloc = 0;
thdr.s_nlnno = 0;
thdr.s_flags = STYP_TEXT;
thdr.s_symptr = /* null? */ 0;
thdr.s_modno = /* OOPS */ 0;
thdr.s_pad = /* don't care */ 0;

strncpy(dhdr.s_name,_DATA,8);
dhdr.s_paddr = dhdr.s_vaddr = ahdr.data_start;
dhdr.s_size = ahdr.dsize;
dhdr.s_scnptr = thdr.s_scnptr + CEIL(thdr.s_size,pagesize);
dhdr.s_relptr = /* null? */ 0;
dhdr.s_lnnoptr = /* null? */ 0;
dhdr.s_nreloc = 0;
dhdr.s_nlnno = 0;
dhdr.s_flags = STYP_DATA;
dhdr.s_symptr = /* null? */ 0;
dhdr.s_modno = /* OOPS */ 0;
dhdr.s_pad = /* don't care */ 0;

strncpy(bhdr.s_name,_BSS,8);
bhdr.s_paddr = bhdr.s_vaddr = ahdr.data_start + ahdr.dsize;
bhdr.s_size = /* none */ 0;
bhdr.s_scnptr = /* null */ 0;
bhdr.s_relptr = /* null? */ 0;
bhdr.s_lnnoptr = /* null? */ 0;
bhdr.s_nreloc = 0;
bhdr.s_nlnno = 0;
bhdr.s_flags = STYP_BSS;
bhdr.s_symptr = /* null? */ 0;
bhdr.s_modno = /* OOPS */ 0;
bhdr.s_pad = /* don't care */ 0;
 bulletproofWrite(fd,&fhdr,sizeof(fhdr));
 bulletproofWrite(fd,&ahdr,sizeof(ahdr));
 bulletproofWrite(fd,&thdr,sizeof(thdr));
 bulletproofWrite(fd,&dhdr,sizeof(dhdr));
 bulletproofWrite(fd,&bhdr,sizeof(bhdr));
 *countp = allhdrsize;
 return 0;
}
#endif NS32
#else AUX

#include <stdio.h>
#ifdef __STDC__
#define insist(condition) do { if (!(condition)) { \
    chatting("==> Inconsistent object for export: !(%s)\n", #condition); \
    return 1; } } while(0)
#else
#define insist(condition) do { if (!(condition)) { \
    chatting("==> Inconsistent object for export: !(%s)\n", "condition"); \
    return 1; } } while(0)
#endif

int export (filid) /*nonzero return means error, check errno */
    int filid;
{
    int i;
    int dataremaining;		/* # of bytes left in data *segment* */
    int textcount;		/* # of bytes in text segment */
    int seendata;		/* true iff have seen .data *section* hdr */
#define HDBUF 512
    char headerbuffer[HDBUF];

#define AOUTHSZ sizeof(AOUTHDR)

    /* A/UX stores header information at N_TXTADDR */
    struct filehdr *fh = (FILHDR *) headerbuffer;
    struct aouthdr *ah = (AOUTHDR *) (headerbuffer + FILHSZ);
    struct scnhdr *sh = (SCNHDR *) (headerbuffer + FILHSZ + AOUTHSZ);

    int headersize = FILHSZ+AOUTHSZ+(((FILHDR *) N_TXTADDR)->f_nscns * SCNHSZ);

    insist(headersize <= HDBUF);

    memcpy(headerbuffer, N_TXTADDR, headersize);  /* copy headers for mods */

    /* validate & adjust file header */
    insist(fh->f_magic == MC68MAGIC);
    fh->f_symptr = 0;
    fh->f_nsyms = 0;
    insist(fh->f_opthdr == sizeof(*ah));
    fh->f_flags = F_RELFLG|F_EXEC|F_LNNO|F_LSYMS|F_AR32W; /* stripped */

    /* validate & adjust optional header */
    insist(ah->magic == ZMAGIC);
    insist(ah->tsize == ETEXT - (N_TXTADDR + headersize));
    ah->dsize = old_high - N_DATADDR;
    ah->bsize = 0;
    insist(ah->entry == startptr);
    insist(ah->text_start == N_TXTADDR + headersize);

    /* adjust section headers */
    textcount = 0;
    dataremaining = ah->dsize;
    seendata = 0;
    for (i=0; i<fh->f_nscns; i++, sh++) {
	insist(sh->s_paddr == sh->s_vaddr);
	insist(sh->s_relptr == 0);
	sh->s_lnnoptr = 0;
	insist(sh->s_nreloc == 0);
	sh->s_nlnno = 0;
	switch(sh->s_flags) {
	  case STYP_TEXT:
	    insist(sh->s_scnptr == headersize);
	    textcount += sh->s_size;
	    break;
	  case STYP_DATA:
	  case STYP_BSS:
	    if (seendata) { /* turn into a dummy section */
		sh->s_paddr = sh->s_vaddr = ah->data_start + ah->dsize;
		sh->s_size = 0;
		sh->s_scnptr = 0;
	    } else {
		insist((sh->s_flags & STYP_BSS) == 0);
		if (sh->s_flags == STYP_DATA) {
		    sh->s_size = dataremaining;
		    seendata = 1;
		} else {
		    dataremaining -= sh->s_size;
		}
	    }
	    break;
	  default: insist(0);
	}
    }
    insist(textcount == ah->tsize);

    fchmod(filid,0755);
    bulletproofWrite(filid,fh,headersize); /* headers */
    bulletproofWrite(filid,N_TXTADDR+headersize,ah->tsize); /* text */
    bulletproofWrite(filid,N_DATADDR,ah->dsize); /* data */
    return 0;
}
#endif AUX
#else MIPS

/* See MIPS Assembly language programmer's guide, 
   Chapter 9 (Object file format) */

#include <stdio.h>
#ifdef __STDC__
#define insist(condition) do { if (!(condition)) { \
    chatting("==> Inconsistent object for export: !(%s)\n", #condition); \
    return 1; } } while(0)
#else
#define insist(condition) do { if (!(condition)) { \
    chatting("==> Inconsistent object for export: !(%s)\n", "condition"); \
    return 1; } } while(0)
#endif
/* #define verbose chatting */
static verbose(){}

#undef export

int export (filid) /*nonzero return means error, check errno */
    int filid;
{
     /* plan: increase .data section to get up to old_high, make following
       sections zero */

    int bytcount;
    int i;
    int dataremaining;		/* # of bytes left in data *segment* */
    int textcount;		/* # of bytes in text segment */
    int seendata;		/* true iff have seen .data *section* hdr */
#define HDBUF 512
   char headerbuffer[HDBUF];
    

    /* MIPS stores header information at N_TXTADDR */
    struct filehdr *fh = (struct filehdr *) headerbuffer;
    struct aouthdr *ah = (struct aouthdr *) (headerbuffer + sizeof(*fh));
    struct scnhdr *sh = (struct scnhdr *) (headerbuffer + sizeof(*fh) + sizeof(*ah));

    int headersize = sizeof(*fh)+sizeof(*ah)+
                       (((struct filehdr *) N_TXTADDR)->f_nscns * sizeof(*sh));



    headersize = CEIL(headersize,16);

    insist(headersize <= HDBUF);

    memcpy(headerbuffer, N_TXTADDR, headersize);  /* copy headers for mods */

    /* validate & adjust file header */
#ifdef MIPSEB
    insist(fh->f_magic == MIPSEBMAGIC);  /* big-endian */
#else
    insist(fh->f_magic == MIPSELMAGIC);  /* little-endian */
#endif

    if (fh->f_symptr || fh->f_nsyms) {
	verbose("Discarding %d bytes of symbolic header & symbol table info\n",
		fh->f_nsyms);
	fh->f_symptr = 0;
	fh->f_nsyms = 0;
    }
    insist(fh->f_opthdr == sizeof(*ah));
    fh->f_flags = F_RELFLG | F_EXEC | F_LNNO | F_LSYMS; /* stripped */
    verbose("validated file header\n");

    /* validate & adjust optional header */
    insist(ah->magic == ZMAGIC);
    insist(ah->tsize == CEIL (((int) &etext)-N_TXTADDR, getpagesize()));
    ah->dsize = CEIL(old_high - N_DATADDR,getpagesize());
    ah->bsize = 0;
    insist(ah->entry == startptr[0]);
    insist(ah->text_start == N_TXTADDR);
    insist(ah->data_start == N_DATADDR);
    ah->bss_start = CEIL(ah->data_start+ah->dsize, getpagesize());
    /* don't change masks or gp value */
    verbose("validated optional header\n");

    /* adjust section headers */
    textcount = 0;
    dataremaining = ah->dsize;
    seendata = 0;
    for (i=0; i<fh->f_nscns; i++, sh++) {
	insist(sh->s_paddr == sh->s_vaddr);
	insist(sh->s_relptr == 0);
	sh->s_lnnoptr = 0;	/* Destroy gp tables */
	insist(sh->s_nreloc == 0);
	insist(sh->s_nlnno == 0);
	switch(sh->s_flags) {
	  case STYP_TEXT:
	    insist(sh->s_scnptr == headersize);	/* fall through */
	  case STYP_INIT:
	    textcount += sh->s_size;
	    break;
	  case STYP_RDATA:
	  case STYP_DATA:
	  case STYP_LIT8:
	  case STYP_LIT4:
	  case STYP_SDATA:
	  case STYP_SBSS:
	  case STYP_BSS:
	    if (seendata) { /* turn into a dummy section */
		sh->s_paddr = sh->s_vaddr = ah->data_start + ah->dsize;
		sh->s_size = 0;
		sh->s_scnptr = 0;
	    } else {
		insist((sh->s_flags & (STYP_SBSS | STYP_BSS)) == 0);
		if (sh->s_flags == STYP_DATA) {
		    sh->s_size = dataremaining;
		    seendata = 1;
		} else {
		    dataremaining -= sh->s_size;
		}
	    }
	    break;
#if defined(SGI)  && defined(_STYP_RESOURCE)
 	  case _STYP_RESOURCE: break;	/* appears under v 4.0 */
#endif
#if defined(STYP_COMMENT)
	/* the following appears on SGI, compiler version 3.10 */
	  case STYP_COMMENT: break;
#endif
	  default: insist(0);
	}
	verbose("Adjusted %s section\n", sh->s_name);
    }
    insist(CEIL(textcount + headersize,getpagesize())==ah->tsize);

    fchmod(filid,0755);
    bulletproofWrite(filid,fh,headersize); /* headers */
    bulletproofWrite(filid,N_TXTADDR+headersize,ah->tsize-headersize); /* text */
    bulletproofWrite(filid,N_DATADDR,ah->dsize); /* data */
 return 0;
}
#endif MIPS
#else NeXT
#ifdef NeXT_3_0
#include <mach/m68k/thread_status.h>
#define NeXT_THREAD_STATE_REGS M68K_THREAD_STATE_REGS
#define NeXT_THREAD_STATE_REGS_COUNT M68K_THREAD_STATE_REGS_COUNT
#define NeXT_thread_state_regs m68k_thread_state_regs
#endif
extern int mach_maplimit;
int export(filid) int filid;  /* nonzero return means error, check errno */
{static struct mach_header E;  /* make it static so all fields=0 */
static struct segment_command tcmd;
static struct section tsectn;
static struct segment_command dcmd;
static struct section dsectn;
static struct section bsectn;
static struct thread_command uthr;
static unsigned long thflavor;
static unsigned long thcount;
static struct NeXT_thread_state_regs ntregs;
static unsigned int hdrsize;
static unsigned int lcmd[] = {
      /* this is the hex representation of the load commands to load the C shared
       * library (contributed by oneill@cs.sfu.ca)
       */
	0x00000006, 0x00000030, 0x00000014, 0x0000002c,
	0x05000000, 0x2f757372, 0x2f73686c, 0x69622f6c,
	0x69627379, 0x735f732e, 0x422e7368, 0x6c696200
    };
int datasize, bsssize;

E.magic = MH_MAGIC;
#ifdef CPU_SUBTYPE_NeXT
/** NeXT 1.0 **/  /* this probably doesn't work anymore */
E.cputype = CPU_TYPE_MC68030;
E.cpusubtype = CPU_SUBTYPE_NeXT;
#else
/** NeXT 2.0 **/
E.cputype = CPU_TYPE_MC680x0;
E.cpusubtype = CPU_SUBTYPE_MC68040;
#endif
E.filetype = MH_EXECUTE;
E.ncmds = 4;
E.sizeofcmds = sizeof(tcmd) + sizeof(tsectn)
	+ sizeof(dcmd) + sizeof(dsectn) + sizeof(bsectn) + sizeof(lcmd)
	+ sizeof(uthr) + sizeof(thflavor) + sizeof(thcount) + sizeof(ntregs);
E.flags = MH_NOUNDEFS;

hdrsize = E.sizeofcmds + sizeof(E);
 textstart = N_TXTADDR(E);


tcmd.cmd = LC_SEGMENT;
tcmd.cmdsize = sizeof(tcmd) + sizeof(tsectn);
strcpy(tcmd.segname,SEG_TEXT);
tcmd.vmaddr = textstart;
tcmd.vmsize = (int) CEIL(ETEXT,getpagesize())-textstart;
tcmd.fileoff = 0;
tcmd.filesize = tcmd.vmsize;
tcmd.maxprot = VM_PROT_ALL;
tcmd.initprot = VM_PROT_READ|VM_PROT_EXECUTE;
tcmd.nsects = 1;
tcmd.flags = 0;

strcpy(tsectn.sectname,SECT_TEXT);
strcpy(tsectn.segname,tcmd.segname);
tsectn.addr = tcmd.vmaddr + hdrsize;
tsectn.size = tcmd.vmsize - hdrsize;
tsectn.offset = hdrsize;
tsectn.align = 2;
tsectn.reloff = 0;
tsectn.nreloc = 0;
tsectn.flags = 0;
tsectn.reserved1 = tsectn.reserved2 = 0;


dcmd.cmd = LC_SEGMENT;
dcmd.cmdsize = sizeof(dcmd) + sizeof(dsectn) + sizeof(bsectn);
strcpy(dcmd.segname,SEG_DATA);
dcmd.vmaddr = tcmd.vmaddr + tcmd.vmsize;
datasize = CEIL(old_high-dcmd.vmaddr, getpagesize());
bsssize = mach_maplimit-old_high;
dcmd.vmsize = bsssize+datasize;
dcmd.fileoff = tcmd.fileoff + tcmd.filesize;
dcmd.filesize = datasize;
dcmd.maxprot = VM_PROT_ALL;
dcmd.initprot = VM_PROT_ALL;
dcmd.nsects = 2;
dcmd.flags = 0;

strcpy(dsectn.sectname,SECT_DATA);
strcpy(dsectn.segname,dcmd.segname);
dsectn.addr = dcmd.vmaddr;
dsectn.size = datasize;
dsectn.offset = dcmd.fileoff;
dsectn.align = 2;
dsectn.reloff = 0;
dsectn.nreloc = 0;
dsectn.flags = 0;
dsectn.reserved1 = dsectn.reserved2 = 0;

strcpy(bsectn.sectname,SECT_BSS);
strcpy(bsectn.segname,dcmd.segname);
bsectn.addr = dsectn.addr + dsectn.size;
bsectn.size = bsssize;
bsectn.offset = 0;
bsectn.align = 2;
bsectn.reloff = 0;
bsectn.nreloc = 0;
bsectn.flags = S_ZEROFILL;
bsectn.reserved1 = bsectn.reserved2 = 0;

uthr.cmd = LC_UNIXTHREAD;
uthr.cmdsize = sizeof(uthr) + sizeof(thflavor) + sizeof(thcount) + sizeof(ntregs);

thflavor = NeXT_THREAD_STATE_REGS;

thcount = NeXT_THREAD_STATE_REGS_COUNT;

ntregs.areg[0] = 0xfeadface;
ntregs.pc = startptr;

 fchmod(filid,0755);
 bulletproofWrite(filid,&E,sizeof(E));
 bulletproofWrite(filid,&tcmd,sizeof(tcmd));
 bulletproofWrite(filid,&tsectn,sizeof(tsectn));
 bulletproofWrite(filid,&dcmd,sizeof(dcmd));
 bulletproofWrite(filid,&dsectn,sizeof(dsectn));
 bulletproofWrite(filid,&bsectn,sizeof(bsectn));
 bulletproofWrite(filid,&lcmd,sizeof(lcmd));
 bulletproofWrite(filid,&uthr,sizeof(uthr));
 bulletproofWrite(filid,&thflavor,sizeof(thflavor));
 bulletproofWrite(filid,&thcount,sizeof(thcount));
 bulletproofWrite(filid,&ntregs,sizeof(ntregs));
 bulletproofWrite(filid,tsectn.addr,tsectn.size);
 bulletproofWrite(filid,dsectn.addr,dsectn.size);
 return 0;
}
#endif NeXT
#else THINK_C
/* export defined in MacExpImp.c */
#endif THINK_C
#else 
#ifdef AIX
#include "export-rs6000-aix"
#else
#ifdef HPPA
#include "export-hppa-hpux8"
#else
int export(filid) /* nonzero return means error, check errno */
int filid;
{ extern int errno;
  errno=76; /*   76  ENOSYS  Function not implemented */
  return 1;
}
#endif HPPA
#endif AIX
#endif

#else  /* SOLARIS */
#define ELF_TARGET_SPARC
#include <elf.h>
#include <unistd.h>

int getpagesize() { sysconf(_SC_PAGESIZE); }

extern int startptr;
extern int old_high;
static int *_elf_export_numexitfns = 0;
#define HDR_TOTAL (sizeof(Elf32_Ehdr) + 2*sizeof(Elf32_Phdr))
#define CEIL(x,quantum) ((((int)(x))+(quantum)-1)&~((quantum)-1))

int export (fid)
  int fid;
{
  Elf32_Ehdr ehdr;
  Elf32_Phdr phdr;
  int text_addr, text_sz;
  int data_addr, data_sz;
  
  fchmod(fid,0755);

  if (_elf_export_numexitfns) *_elf_export_numexitfns = 0;

  memcpy(ehdr.e_ident, ELFMAG, SELFMAG);
  ehdr.e_ident[EI_CLASS] = ELFCLASS32;
  ehdr.e_ident[EI_DATA] = ELFDATA2MSB;
  ehdr.e_ident[EI_VERSION] = EV_CURRENT;
  memset(&ehdr.e_ident[EI_PAD], 0, EI_NIDENT-EI_PAD);
  ehdr.e_type = ET_EXEC;
  ehdr.e_machine = EM_SPARC;
  ehdr.e_version = EV_CURRENT;
  ehdr.e_entry = startptr; 
  ehdr.e_phoff = sizeof(ehdr);
  ehdr.e_shoff = 0;
  ehdr.e_flags = 0;
  ehdr.e_ehsize = sizeof(ehdr);
  ehdr.e_phentsize = sizeof(phdr);
  ehdr.e_phnum = 2;
  ehdr.e_shentsize = sizeof(Elf32_Shdr);
  ehdr.e_shnum = 0;
  ehdr.e_shstrndx = SHN_UNDEF;
  bulletproofWrite(fid, &ehdr, sizeof(ehdr));

  text_addr = ELF_SPARC_MAXPGSZ + HDR_TOTAL;
  text_sz = ETEXT - text_addr;
  phdr.p_type = PT_LOAD;
  phdr.p_offset = HDR_TOTAL;
  phdr.p_vaddr = text_addr;
  phdr.p_paddr = 0;
  phdr.p_filesz = text_sz;
  phdr.p_memsz = phdr.p_filesz;
  phdr.p_flags = PF_R | PF_X;
  phdr.p_align = ELF_SPARC_MAXPGSZ;
  bulletproofWrite(fid, &phdr, sizeof(phdr));

  phdr.p_type = PT_LOAD;
  phdr.p_offset = CEIL(text_sz + HDR_TOTAL,8);
  data_addr = CEIL(ETEXT,ELF_SPARC_MAXPGSZ) + (phdr.p_offset & (ELF_SPARC_MAXPGSZ - 1));
  data_sz = CEIL(old_high-data_addr, 4);  /* Is 4 byte align necessary? */
  phdr.p_vaddr = data_addr;
  phdr.p_paddr = 0;
  phdr.p_filesz = data_sz;
  phdr.p_memsz = phdr.p_filesz;
  phdr.p_flags = PF_R | PF_W | PF_X;
  phdr.p_align = ELF_SPARC_MAXPGSZ;
  bulletproofWrite(fid, &phdr, sizeof(phdr));

    /* Is writing the pad from text safe; should we write 0 pad bytes? */
  bulletproofWrite(fid, text_addr, phdr.p_offset - HDR_TOTAL);
  bulletproofWrite(fid, data_addr, data_sz);

  return (0);
}

#endif SOLARIS

/* bulletproofWrite:
 * Write nbytes from buffer to the file fd, taking care of incomplete writes.
 *
 * NOTE: return non-zero if i/o error occurs
 */
int bulletproofWrite0 (fd, buf, nbytes)
    int		fd;
    char	*buf;
    int		nbytes;
{
    extern int	errno;
    int		bytesWritten = 0, i;

    do {
	if ((i = write(fd, buf, nbytes-bytesWritten)) < 0) return 1;
	bytesWritten += i;
	buf += i;
	if (bytesWritten < nbytes)
	    chatting("[Write incomplete (%d/%d), retrying]\n", bytesWritten, nbytes);
    } while (bytesWritten < nbytes);
  return 0;

} /* end of bulletproofWrite */




