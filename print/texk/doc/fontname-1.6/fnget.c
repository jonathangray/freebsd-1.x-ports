/*
FONTNAME.C
Graham Asher, 10th December 1992

Converts fonts named according to Karl Berry's scheme into full names.

The scheme for using the eight letters of a filename is:

STTWVEDD

where S = source (foundry), TT = typeface name, W = weight, V = variant,
E = width/expansion and DD = design size. DD is omitted if the font is
scaled, E is omitted if normal, and V is omitted if it and E are normal.

At present only one variant code is read but future versions may allow
for more than one variant.
*/

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>

/* Table of 36 (0-9, a-z) sources and foundries. */
static char *S[36] =
{
"unknown source ", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
"Autologic ", "Bitstream ", "Compugraphic ", NULL, NULL, "public domain ",
"Free Software Foundation (GNU) ", "Bigelow & Holmes ",
"International Typeface Corporation ", NULL, NULL, NULL, "Monotype ", "IBM ",
NULL, "Adobe ", NULL, "raw ", "Sun ", NULL, "URW ", NULL, NULL,
"American Mathematical Society ", NULL, "bizarre "
};

/*
Table of typeface codes and names, sorted on the two-letter code and
terminated by a NULL.
*/
static char *TT[] =
{
"aaAachen ", "abArnold Boecklin ", "acAdobe Caslon ", "adAdobe Garamond ",
"agAvant Garde ", "alAlbertus ", "amAmericana ", "aoAntique Olive ",
"arArcadia ", "asAldus ", "atAmerican Typewriter ", "auAuriol ", "avAvenir ",
"azAkzidenz Grotesque ", "b0Blackoak ", "bbBembo ", "bcBanco ", "bdBodoni ",
"bgBenguiat ", "bhBauhaus ", "biBirch ", "bkBookman ", "blBelwe ", "boBalloon ",
"bpBundesbahn Pi ", "brBerling ", "bsBakerSignet ", "buBrush ", "bvBaskerville ",
"bwBroadway ", "byBerkeley ", "c0Carolina ", "c2Caslon 224 ", "c3Caslon 3 ",
"c5Caslon 540 ", "caCaslon ", "cbCooper Black ", "ccCascade ", "cdClarendon ",
"ceCentennial ", "cfClearface ", "cgCharlemagne ", "chCharter ", "ciCandida ",
"clCloister ", "cmComputer Modern ", "cnCentury ", "coCochin ", "cpCopperplate ",
"cqCheq ", "crCourier ", "csCentury Schoolbook ", "ctCheltenham ",
"cuCentury Old Style ", "cvClairvaux ", "cwCottonwood ", "cxCaxton ", "cyCity ",
"dcDom Casual ", "ddDuc de Berry ", "drDoric ", "efEgyptienneF ",
"egStempel Garamond ", "ehEngschrift ", "epEuropean Pi ", "erEras ",
"esEurostile ", "euEuler ", "exExcelsior ", "faFormata ", "fgFranklin Gothic ",
"fnFenice ", "foFolio ", "fqFriz Quadrata ", "frFrutiger ", "ftFette Fraktur ",
"fuFutura ", "fyFlyer ", "g1Gothic Thirteen ", "g3Garamond No. 3 ",
"gbGothic BBB ", "ggGarth Graphic ", "gjGranjon ", "glGalliard ", "gmGaramond ",
"goGoudy Old Style ", "gpGlypha ", "gsGill Sans ", "gtGoudy Text ", "guGuardi ",
"gvGiovanni ", "gyGoudy ", "hbHobo ", "hcHerculanum ", "hgHiroshige ",
"hiHelvetica Inserat ", "hmCharme ", "hnHelvetica Neue ", "hrHelvetica Rounded ",
"hvHelvetica ", "iaInsignia ", "icImpact ", "igSimoncini Garamond ",
"ilCaecilia ", "imImago ", "inIndustria ", "ipImpressum ", "itItalia ",
"iwIronwood ", "jnJanson ", "joJoanna ", "jpJuniper ", "kbKabel ", "krKorinna ",
"ksKuenstler Script ", "kuKaufmann ", "lbLubalin Graph ", "lcLucida ", "lfLife ",
"lgLetter Gothic ", "lnLino ", "loLithos ", "lsMittelschrift ", "ltLutetia ",
"lwLeawood ", "mdMeridien ", "mhMachine ", "miMedici ", "mlMelior ", "mnMinion ",
"mpMemphis ", "mqMesquite ", "mrMadrone ", "msMistral ", "mtMinister ", "mvMalvern ",
"mxMaximus ", "naNew Aster ", "nbNew Baskerville ", "ncNew Century Schoolbook ",
"ndNew Caledonia ", "nfNofret ", "ngNews Gothic ", "ntTimes New Roman ",
"nuNuptial ", "nvNovarese ", "nzNeuzeit S ", "o7Old Style 7 ", "oaOCR-A ",
"obOCR-B ", "ocConcorde ", "omOmnia ", "onCorona ", "opOptima ", "orOrator ",
"otCoronet ", "oyOlympian ", "p0Poplar ", "paPark Avenue ", "pePrestige Elite ",
"pgPeignot ", "piPlantin ", "plPalatino ", "poPonderosa ", "ppPerpetua ",
"pqPost Antiqua ", "psParisian ", "ptPresent ", "pxPoppl Pontifex ", "qrQuorum ",
"raRaleigh ", "roRotis ", "rpReporter ", "rqRussell Square ", "rtCarta ",
"rvRevue ", "rwRockwell ", "ryRyumin ", "s0San Marco ", "sbSabon ", "scSlimbach ",
"sfSerifa ", "sgSerif Gothic ", "shShannon ", "slStencil ", "snSpartan ",
"soSonata ", "spSerpentine ", "srSnell Roundhand ", "ssStempel Schneidler ",
"stStone ", "svSouvenir ", "sxSyntax ", "sySymbol ", "tbBerthold Bodoni ",
"teTimes Europa ", "tfTiffany ", "tgTrade Gothic ", "tiTech Phonetic ",
"tjTrajan ", "tkTektok ", "tlCastellar ", "tmTimes ", "tpTempo ",
"tvTrump Mediaeval ", "ubBauer Bodoni ", "ugBenguiat Gothic ", "uhCushing ",
"ulUniversal ", "umUmbra ", "unUnivers ", "urCentaur ", "utUtopia ", "uwUsherwood ",
"uyUniversity ", "vcVectora ", "vjVeljovic ", "vlVersailles ", "vrVAG Rounded ",
"wbWalbaum ", "wdWeidemann ", "wgWilhelmKlingsporGotisch ", "wkWilke ",
"woWood ", "wsWeiss ", "wwWillow ", "ymStymie ", "zcZapf Chancery ",
"zdZapf Dingbats ", "zgNeuzeit Grotesk ", NULL
};

/* Table of 26 weights. */
static char *W[26] =
{
"thin ", "bold ", "black ", "demi bold ", NULL, NULL, NULL, "heavy ",
"extra light ", NULL, "book ", "light ", "medium ", NULL, NULL, "poster ",
NULL, "normal weight ", "semi bold ", NULL, "ultra ", NULL, NULL, "extra bold ",
NULL, NULL
};

/* Table of 36 variants. */
static char *V[36] =
{
"Adobe standard encoding ", "semi sans ", "changed characters only ",
"fractions ", NULL, NULL, NULL, NULL, NULL, "oldstyle digits ",
"Adobe alternate encoding ", "bright ", "small caps ", "display ", "engraved ",
"Fraktur ", "grooved ", "shadow ", "italic ", "invisible ", "Greek ",
"outline ", "math italic ", "informal ", "oblique ", "ornament ",
"GNU text complement encoding ", "normal variant ", "sans serif ", "typewriter ",
"unslanted italic ", "math extension ", "script ", "Adobe expert encoding ",
"symbol ", "Cyrillic "
};

/* Table of 26 widths (expansions). */
static char *E[26] =
{
NULL, NULL, "condensed ", NULL, "expanded ", NULL, NULL, NULL, NULL, NULL, NULL,
NULL, NULL, "narrow ", "extra condensed ", "compressed ", "extra compressed ",
"normal width ", NULL, "thin ", "ultra compressed ", NULL, "wide ", "extended ",
NULL, NULL
};

static void normalise(char *s);
static void index26(char *p);
static void index36(char *p);
static int compare_TTs(const char **j,const char **k);

/*
A main() for testing.
*/
#ifdef TESTING
int
main(int argc,char **argv)
{
int i;
char buffer[80];

if (argc < 3)
  {
  puts("usage: fontname <template> <filename(s)>");
  exit(1);
  }

for (i = 2; i < argc; i++)
  {
  FNget(buffer,sizeof(buffer),argv[i],argv[1]);
  printf("%s = %s\n",argv[i],buffer);
  }
return (0);
}
#endif

/*
Convert a filename to a font name. The template, an eight-character string
in the form STTWVEDD, instructs FNget() which parts to include and which to
omit: '-' means omit, '*' means include, and any other letter means `include
if not equal to this letter'. The default template is "***rrr**". If the
template is null or shorter than eight characters the missing parts are
taken to be the defaults. For example, to omit the foundry unless
non-Adobe, and always omit the variant, use the template "p***-"; and
to omit source unless non-public domain, and the typeface unless
non-Computer Modern, use "fcm". (BUT for now any names starting `cm..',
`eu..' or `ms..' (Computer Modern, Euler and American Mathematical Society)
are left untranslated.) The decoded name is placed in <dest> as a
null-terminated string of <size> characters or less including the null.
If decoding is successful 1 is returned, otherwise 0 is returned and the
filename is simply copied into <dest>.
*/
int
FNget(char *dest,size_t size,char *filename,char *template)
{
char t[9], f[9], name[2];
int i, dpos;
char *s = "", *tt = "", *w = "", *v = "", *e = "", *dd = "", *key = name;
char **b;
static int TT_count;

/* Extend the template and filename to 8 characters and force to lowercase. */
strcpy(t,"***rrr**");
if (template)
  for (i = 0; i < 8 && template[i]; i++)
    t[i] = tolower(template[i]);
strcpy(f,"   rrr  ");
if (filename)
  for (i = 0; i < 8 && isalnum(filename[i]); i++)
    f[i] = tolower(filename[i]);

/*
Reject Computer Modern, Euler and American Mathematical Society fonts:
we want to be able to use these without translating them or changing their
names.
*/
if (!strncmp(f,"cm",2) || !strncmp(f,"eu",2) || !strncmp(f,"ms",2))
  {
  strncpy(dest,filename,size);
  return (0);
  }

/*
Find the position of the design size. This method means that variant codes
which are digits only work if an expansion code follows.
*/
if (isdigit(f[4]) && (!filename[5] || isdigit(f[5])))
  dpos = 4;
else if (isdigit(f[5]))
  dpos = 5;
else if (isdigit(f[6]))
  dpos = 6;
else
  dpos = 8;

/* Normalise the template and filename. */
normalise(t);
normalise(f);

/* Restore design size if obliterated by normalisation. */
if (dpos < 6)
  strcpy(&f[dpos],&filename[dpos]);

/* Get source. */
if (f[0] > 35)
  s = NULL;
else if (t[0] != '-' && t[0] != f[0])
  s = S[f[0]];

/* Get typeface name. */
if (t[1] != '-' && (t[1] != f[1] || t[2] != f[2]))
  {
  while (TT[TT_count])
    TT_count++;
  name[0] = f[1];
  name[1] = f[2];
  b = (char **)bsearch(&key,TT,TT_count,sizeof(TT[0]),compare_TTs);
  tt = b ? (*b) + 2 : NULL;
  }

/* Get weight. */
if (f[3] > 25)
  w = NULL;
else if (t[3] != '-' && t[3] != f[3])
  w = W[f[3]];

/* Get variant. */
if (dpos > 4)
  {
  if (f[4] > 35)
    v = NULL;
  else if (t[4] != '-' && t[4] != f[4])
    v = V[f[4]];
  }

/* Get expansion. */
if (dpos > 5)
  {
  if (f[5] > 25)
    e = NULL;
  else if (t[5] != '-' && t[5] != f[5])
    e = E[f[5]];
  }

/* Get design size. */
if (t[6] != '-' && dpos < 8 && strcmp(&t[6],&f[dpos]))
  dd = &f[dpos];

/* If there are any errors return the filename untranslated. */
if (!s || !tt || !w || !v || !e || !dd)
  {
  strncpy(dest,filename,size);
  return (0);
  }

/* Construct the full name in the destination buffer. */
i = 0;
size--;
while (*s && i < size)
  *dest++ = *s++, i++;
while (*tt && i < size)
  *dest++ = *tt++, i++;
while (*w && i < size)
  *dest++ = *w++, i++;
while (*v && i < size)
  *dest++ = *v++, i++;
while (*e && i < size)
  *dest++ = *e++, i++;
if (*dd)
  {
  while (*dd && *dd != ' ' && i < size)
    *dest++ = *dd++, i++;
  if (i < size - 1)
    strcpy(dest,"pt"), dest += 2, i += 2;
  }
if (dest[-1] == ' ')
  dest--;
*dest = 0;
return (1);
}

/* Normalise a filename or template. */
static void
normalise(char *s)
{
index36(&s[0]);
index26(&s[3]);
index36(&s[4]);
index26(&s[5]);
}

/*
Set a character to the range 0..25 if it is a letter,
or to '*' unless it is '-';
*/
static void
index26(char *p)
{
if (islower(*p))
  *p -= 'a';
else if (*p != '-')
  *p = '*';
}

/*
Set a character to the range 0..9 if it is a digit, 10..35 if it is a letter,
or to '*' unless it is '-';
*/
static void
index36(char *p)
{
if (isdigit(*p))
  *p -= '0';
else if (islower(*p))
  *p -= ('a' - 10);
else if (*p != '-')
  *p = '*';
}

/*
Compare two two-letter typeface codes.
*/
static int
compare_TTs(const char **j,const char **k)
{
const char *x = *j, *y = *k;

return (strncmp(x,y,2));
}
