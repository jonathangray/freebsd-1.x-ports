Kterm is an X11 terminal emulator that can handle Japanese text.
This release is based on xterm in the core part of X11R5 distribution.


Configuration
----
Kterm have some optional features and all of them are enabled by default.
Edit Imakefile and kterm.h to disable them.

In Imakefile, -DKTERM, -DSTATUSLINE and -DKEEPALIVE are set in the
DEFINES line. They mean respectively:
 o KTERM:       enables Kana, Kanji, colored text and other supports.
 o STATUSLINE:  enables status line support.
 o KEEPALIVE:   enables keepalive socket option for the server connection.
You may remove some or all of them unneeded. If you remove all options,
your kterm is identical with xterm.

If you define -DKTERM in Imakefile, kterm.h defines some text related
options, KTERM_MBCS, KTERM_KANJI, KTERM_KCONV, KTERM_COLOR and KTERM_MBCC.
 o KTERM_COLOR: enables colored text support.
 o KTERM_MBCS:  enables support for MBCS text including Kanji.
 o KTERM_KANJI: enables support for Kanji specific encodings like Shift-JIS
                and related menu entries. KTERM_MBCS must be defined.
 o KTERM_KCONV: enables support for Kanji text input using kinput or kinput2.
                You don't need to apply the kterm-4 patch included in kinput2
        	source. KTERM_MBCS must be defined.
 o KTERM_MBCC:  enables MB character class support. It is used for word
                selection. KTERM_MBCS must be defined.
You may remove some or all of them unneeded. But make sure that you have
to remove KTERM_KANJI, KTERM_KCONV and KTERM_MBCC if you remove KTERM_MBCS.


Compilation
----
To compile kterm, you have to provide X11R5 environment, at least its
header files. Additionaly, it is recommended to link kterm with X11R5
libraries. Compiling kterm without X11R5 is not tested.

Then, just type:
 % xmkmf; make


Installation
----
Install kterm and KTerm.ad:
 % make install

Install kterm.man manual page:
 % make install.man

If your system supports Japanese manual pages and you want to install
kterm.jman, copy it to appropriate directory and filename with appropriate
encoding by your hand. Original kterm.jman is encoded by JIS.

If termcap or terminfo in your system does not have kterm entry and you
want to use status line or uum or some other software, install termcap.kt
or terminfo.kt manually.


Bugs
----
If you find a bug, please check first if it is not an xterm bug, then report
it to kagotani@cs.titech.ac.jp. Don't forget to include your kterm version
and informatin about your system. "kterm -version" prints its version.

						1991/Oct./4
						Hiroto Kagotani
						kagotani@cs.titech.ac.jp
