#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <libelf.h>

char   *file;            /* Patch file */

void
fatal(message)
    char * message;
{
    extern int errno;
    fprintf(stderr,"patch: file %s: %s, errno = %d\n", file, message, errno);
    exit(-1);
}
    
int
readShdr(elf, strndx, sname, shead)
    Elf* elf;
    int  strndx;
    char* sname;
    Elf32_Shdr** shead;
{
    Elf_Scn    *scn = 0;
    int        idx = 0;

    while ((scn = elf_nextscn(elf, scn)) != 0) {
        idx++;
        if ((*shead = elf32_getshdr(scn)) != 0)
            if (strcmp(sname, elf_strptr(elf, strndx, (size_t)(*shead)->sh_name)) == 0)
                break;
    }

    return idx;
}

main(argc, argv)
    int argc;
    char *argv[];
{
    Elf        *elf;
    Elf32_Ehdr *ehead;
    Elf32_Shdr *shead;
    Elf32_Addr *symbol;
    Elf32_Off  offset_data;
    Elf32_Addr addr_data;
    Elf32_Addr addr_numexitfns;
    Elf32_Addr addr_export;
    int        addr_cnt = 0;
    int        fid, fid1;
    int        x_sym, sym_cnt;
    int        strndx, symndx, symsz;
    int        filepos;
    int        debug = 0;

    if (argc == 3 && strcmp(argv[1], "-d") == 0) {
        file = argv[2];
        debug = 1;
    } else if (argc == 2) file = argv[1];
    else fatal("usage: patch file");

    if ((fid = open(file, O_RDWR)) < 0)
        fatal("cannot open file");
    fid1 = open(file, O_RDONLY);

    if (elf_version(EV_CURRENT) == EV_NONE)
        fatal("Elf version out-of-date");

    if ((elf = elf_begin(fid1, ELF_C_READ, (Elf*)0)) == 0)
        fatal("cannot elf_begin file");

    if ((ehead = elf32_getehdr( elf )) == 0) {
        elf_end( elf );
        fatal("cannot get elf header");
    }

    strndx = ehead->e_shstrndx;

    if (readShdr( elf, strndx, ".data", &shead) == 0)
        fatal("cannot get .data header");
    
    offset_data = shead->sh_offset;
    addr_data = shead->sh_addr;

    if (debug) {
        printf("data in file: 0x%x (%d)\n", offset_data, offset_data);
        printf("        addr: 0x%x (%d)\n", addr_data, addr_data);
    }

    if ((symndx = readShdr( elf, strndx, ".strtab", &shead)) == 0)
        fatal("cannot get .strtab header");

    if (readShdr( elf, strndx, ".symtab", &shead) == 0)
        fatal("cannot get symbol table");

    symsz = shead->sh_entsize;
    lseek( fid, shead->sh_offset, 0 );

    if (debug) {
        printf("symbol table in file: 0x%x (%d)\n", 
          shead->sh_offset, shead->sh_offset);
        printf("                addr: 0x%x (%d), num=%d\n", 
          shead->sh_addr, shead->sh_addr, shead->sh_info);
    }

    sym_cnt = shead->sh_size / symsz;

    for ( x_sym = 0; (addr_cnt < 2) && (x_sym < sym_cnt); ++x_sym ) {
        char      *str;
        Elf32_Sym sym;

        if ( read( fid, (char *)&sym, symsz ) != symsz)
            break;

        if (sym.st_value  == 0)
            continue;

        str = elf_strptr( elf, symndx, (size_t)sym.st_name);

        if (str[0] == '_') {
            if (strcmp(str, "_elf_export_numexitfns") == 0) {
                if (debug)
                    printf("_elf_export_numexitfns found at 0x%x (%d)\n",
                    sym.st_value, sym.st_value);
                addr_export = sym.st_value;
                addr_cnt++;
            }
        }
        else if (str[0] == 'n') {
            if (strcmp(str, "numexitfns") == 0) {
                if (debug)
                    printf("numexitfns found at 0x%x (%d)\n", 
                    sym.st_value, sym.st_value);
                addr_numexitfns = sym.st_value;
                addr_cnt++;
            }
        }

    }

    elf_end( elf );
    close( fid1 );

    if ( addr_cnt != 2) {
        fatal(" symbol not found");
    }

    /* patch the symbol
     * seek to: physical adr. of symbol
     *  - physical adr of start of data
     *  + file offset of start of data
     */

    filepos = addr_export - addr_data + offset_data;
    if (lseek(fid, filepos, SEEK_SET) < 0)
        fatal("can't seek");

    if(debug)
        printf("Write 0x%x at offset 0x%x\n", addr_numexitfns, filepos );

    if(write(fid, &addr_numexitfns, sizeof(Elf32_Addr)) != sizeof(Elf32_Addr))
        fatal("can't write file");

    close(fid);

    exit(0);
}

