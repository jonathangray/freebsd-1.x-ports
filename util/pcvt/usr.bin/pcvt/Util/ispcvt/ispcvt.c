/*
 * Copyright (c) 1992,1993,1994 Hellmuth Michaelis
 *
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
 *	This product includes software developed by Hellmuth Michaelis
 * 4. The name authors may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

static char *id =
	"@(#)ispcvt.c, 3.00, Last Edit-Date: [Mon Feb 14 16:42:02 1994]";

/*---------------------------------------------------------------------------*
 *
 *	history:
 *
 *	-hm	upgraded to report pcvt compile time configuration
 *	-hm	PCVT_INHIBIT_NUMLOCK patch from Joerg
 *	-hm	PCVT_META_ESC patch from Joerg
 *	-hm	PCVT_PCBURST
 *
 *---------------------------------------------------------------------------*/

#include <stdio.h>
#include <machine/pcvt_ioctl.h>

main(argc,argv)
int argc;
char *argv[];
{
	extern int optind;
	extern int opterr;
	extern char *optarg;

	struct pcvtid pcvtid;
	int c;
	char *p;
	int verbose = 0;
	int config = 0;	

	while( (c = getopt(argc, argv, "vc")) != EOF)
	{
		switch(c)
		{
			case 'v':
				verbose = 1;
				break;
				
			case 'c':
				config = 1;
				break;
				
			case '?':
			default:
				usage();
				break;
		}
	}

	if(ioctl(0, VGAPCVTID, &pcvtid) == -1)
	{
		if(verbose)
			perror("ispcvt - ioctl VGAPCVTID failed, error");
		exit(1);
	}

	if(!strcmp(pcvtid.name, PCVTIDNAME))
	{
		if(pcvtid.rmajor == PCVTIDMAJOR)
		{
			if(pcvtid.rminor != PCVTIDMINOR)
			{
				if(verbose)
					fprintf(stderr,"ispcvt - minor revision: expected %d, got %d\n", PCVTIDMINOR, pcvtid.rminor);
				exit(4);	/* minor revision mismatch */
			}
		}
		else
		{
			if(verbose)
				fprintf(stderr,"ispcvt - major revision: expected %d, got %d\n", PCVTIDMAJOR, pcvtid.rmajor);
			exit(3);	/* major revision mismatch */
		}
	}
	else
	{
		if(verbose)
			fprintf(stderr,"ispcvt - name check: expected %s, got %s\n", PCVTIDNAME, pcvtid.name);
		exit(2);	/* name mismatch */
	}

	if(verbose)
	{
		fprintf(stderr,"\nispcvt - success: driver name [%s], revision [%1.1d.%02.2d]\n\n",pcvtid.name,pcvtid.rmajor,pcvtid.rminor);
	}

	if(config == 0)
		exit(0);

	if(verbose)
	{
		switch(pcvtid.opsys)
		{
			case CONF_386BSD:
				p = "PCVT_386BSD";
				break;
				
			case CONF_NETBSD:
				p = "PCVT_NETBSD";
				break;
				
			case CONF_FREEBSD:
				p = "PCVT_FREEBSD";
				break;
	
			default:
			case CONF_UNKNOWNOPSYS:
				p = "UNKNOWN";
				break;
				
		}			
		fprintf(stderr,"Operating System     = %s\t", p);
		fprintf(stderr,"Release Id           = %u\n", pcvtid.opsysrel);
		fprintf(stderr,"PCVT_NSCREENS        = %u\t\t", pcvtid.nscreens);
		fprintf(stderr,"PCVT_UPDATEFAST      = %u\n", pcvtid.updatefast);
		fprintf(stderr,"PCVT_UPDATESLOW      = %u\t\t", pcvtid.updateslow);
		fprintf(stderr,"PCVT_SYSBEEPF        = %u\n", pcvtid.sysbeepf);

		fprintf(stderr,"PCVT_PCBURST         = %u\n", pcvtid.pcburst);
	
	/* config booleans */

		fprintf(stderr,"PCVT_VT220KEYB       = %s\t\t",
			((u_int)pcvtid.compile_opts & (u_int)CONF_VT220KEYB) ? "ON" : "OFF");
		
		fprintf(stderr,"PCVT_SCREENSAVER     = %s\n",
			(pcvtid.compile_opts & CONF_SCREENSAVER) ? "ON" : "OFF");
	
		fprintf(stderr,"PCVT_PRETTYSCRNS     = %s\t\t",
			(pcvtid.compile_opts & CONF_PRETTYSCRNS) ? "ON" : "OFF");
	
		fprintf(stderr,"PCVT_CTRL_ALT_DEL    = %s\n",
			(pcvtid.compile_opts & CONF_CTRL_ALT_DEL) ? "ON" : "OFF");
	
		fprintf(stderr,"PCVT_USEKBDSEC       = %s\t\t",
			(pcvtid.compile_opts & CONF_USEKBDSEC) ? "ON" : "OFF");
	
		fprintf(stderr,"PCVT_24LINESDEF      = %s\n",
			(pcvtid.compile_opts & CONF_24LINESDEF) ? "ON" : "OFF");
	
		fprintf(stderr,"PCVT_EMU_MOUSE       = %s\t\t",
			(pcvtid.compile_opts & CONF_EMU_MOUSE) ? "ON" : "OFF");
	
		fprintf(stderr,"PCVT_SHOWKEYS        = %s\n",
			(pcvtid.compile_opts & CONF_SHOWKEYS) ? "ON" : "OFF");
	
		fprintf(stderr,"PCVT_KEYBDID         = %s\t\t",
			(pcvtid.compile_opts & CONF_KEYBDID) ? "ON" : "OFF");
	
		fprintf(stderr,"PCVT_SIGWINCH        = %s\n",
			(pcvtid.compile_opts & CONF_SIGWINCH) ? "ON" : "OFF");
	
		fprintf(stderr,"PCVT_NULLCHARS       = %s\t\t",
			(pcvtid.compile_opts & CONF_NULLCHARS) ? "ON" : "OFF");
	
		fprintf(stderr,"PCVT_BACKUP_FONTS    = %s\n",
			(pcvtid.compile_opts & CONF_BACKUP_FONTS) ? "ON" : "OFF");
	
		fprintf(stderr,"PCVT_FORCE8BIT       = %s\t\t",
			(pcvtid.compile_opts & CONF_FORCE8BIT) ? "ON" : "OFF");
	
		fprintf(stderr,"PCVT_NEEDPG          = %s\n",
			(pcvtid.compile_opts & CONF_NEEDPG) ? "ON" : "OFF");
	
		fprintf(stderr,"PCVT_SETCOLOR        = %s\t\t",
			(pcvtid.compile_opts & CONF_SETCOLOR) ? "ON" : "OFF");
	
		fprintf(stderr,"PCVT_132GENERIC      = %s\n",
			(pcvtid.compile_opts & CONF_132GENERIC) ? "ON" : "OFF");
	
		fprintf(stderr,"PCVT_PALFLICKER      = %s\t\t",
			(pcvtid.compile_opts & CONF_PALFLICKER) ? "ON" : "OFF");
	
		fprintf(stderr,"PCVT_WAITRETRACE     = %s\n",
			(pcvtid.compile_opts & CONF_WAITRETRACE) ? "ON" : "OFF");
	
		fprintf(stderr,"PCVT_XSERVER         = %s\t\t",
			(pcvtid.compile_opts & CONF_XSERVER) ? "ON" : "OFF");
	
		fprintf(stderr,"PCVT_USL_VT_COMPAT   = %s\n",
			(pcvtid.compile_opts & CONF_USL_VT_COMPAT) ? "ON" : "OFF");
	
		fprintf(stderr,"PCVT_FAKE_SYSCONS10  = %s\t\t",
			(pcvtid.compile_opts & CONF_FAKE_SYSCONS10) ? "ON" : "OFF");

		fprintf(stderr,"PCVT_INHIBIT_NUMLOCK = %s\n",
			(pcvtid.compile_opts & CONF_INHIBIT_NUMLOCK) ? "ON" : "OFF");

		fprintf(stderr,"PCVT_META_ESC        = %s\n\n",
			(pcvtid.compile_opts & CONF_META_ESC) ? "ON" : "OFF");
	}
	else /* !verbose */
	{
		fprintf(stderr,"BSD Version      = %u\n", pcvtid.opsys);
		fprintf(stderr,"PCVT_NSCREENS    = %u\n", pcvtid.nscreens);
		fprintf(stderr,"PCVT_UPDATEFAST  = %u\n", pcvtid.updatefast);
		fprintf(stderr,"PCVT_UPDATESLOW  = %u\n", pcvtid.updateslow);
		fprintf(stderr,"PCVT_SYSBEEPF    = %u\n", pcvtid.sysbeepf);
		fprintf(stderr,"Compile options  = 0x%08X\n", pcvtid.compile_opts);
	}
}

usage()
{
	fprintf(stderr,"\nispcvt - verify current video driver is the pcvt-driver\n");
	fprintf(stderr,"usage: ispcvt [-v] [-c]\n");
	fprintf(stderr,"       -v   be verbose\n");
	fprintf(stderr,"       -c   print compile time configuration\n\n");	
	exit(5);
}

