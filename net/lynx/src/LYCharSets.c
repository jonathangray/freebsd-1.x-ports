/* INSTRUCTIONS for adding new character sets !!!!
 *
 * Make up a character set and add it in the same
 * style as the ISO_LATIN1 set below, giving it a unique name.
 *
 * Near the end of this file is a place marked "Add your character sets HERE".
 *
 * Add the name of the set to LYCharSets at the bottom of this file, and
 * also add it to the LYchar_set_names table below LYCharSets.
 * LYCharSets and LYchar_set_names MUST have the same order.
 */

/* this is the default character set */
int current_char_set = 0;
   
/* 	Entity values -- for ISO Latin 1 local representation
**
**	This MUST match exactly the table referred to in the DTD!
*/
static char * ISO_Latin1[] = {
  	"\306",	/* capital AE diphthong (ligature) */ 
  	"\301",	/* capital A, acute accent */ 
  	"\302",	/* capital A, circumflex accent */ 
  	"\300",	/* capital A, grave accent */ 
  	"\305",	/* capital A, ring */ 
  	"\303",	/* capital A, tilde */ 
  	"\304",	/* capital A, dieresis or umlaut mark */ 
  	"\307",	/* capital C, cedilla */ 
  	"\320",	/* capital Eth, Icelandic */ 
  	"\311",	/* capital E, acute accent */ 
  	"\312",	/* capital E, circumflex accent */ 
  	"\310",	/* capital E, grave accent */ 
  	"\313",	/* capital E, dieresis or umlaut mark */ 
  	"\315",	/* capital I, acute accent */ 
  	"\316",	/* capital I, circumflex accent */ 
  	"\314",	/* capital I, grave accent */ 
  	"\317",	/* capital I, dieresis or umlaut mark */ 
  	"\321",	/* capital N, tilde */ 
  	"\323",	/* capital O, acute accent */ 
  	"\324",	/* capital O, circumflex accent */ 
  	"\322",	/* capital O, grave accent */ 
  	"\330",	/* capital O, slash */ 
  	"\325",	/* capital O, tilde */ 
  	"\326",	/* capital O, dieresis or umlaut mark */ 
  	"\336",	/* capital THORN, Icelandic */ 
  	"\332",	/* capital U, acute accent */ 
  	"\333",	/* capital U, circumflex accent */ 
  	"\331",	/* capital U, grave accent */ 
  	"\334",	/* capital U, dieresis or umlaut mark */ 
  	"\335",	/* capital Y, acute accent */ 
  	"\341",	/* small a, acute accent */ 
  	"\342",	/* small a, circumflex accent */ 
  	"\346",	/* small ae diphthong (ligature) */ 
  	"\340",	/* small a, grave accent */ 
  	"\046",	/* ampersand */ 
  	"\345",	/* small a, ring */ 
  	"\343",	/* small a, tilde */ 
  	"\344",	/* small a, dieresis or umlaut mark */ 
  	"\347",	/* small c, cedilla */ 
  	"\351",	/* small e, acute accent */ 
  	"\352",	/* small e, circumflex accent */ 
  	"\350",	/* small e, grave accent */
	"\002",	/* emsp, em space - not collapsed NEVER CHANGE THIS */
	"\002",	/* ensp, en space - not collapsed NEVER CHANGE THIS */
  	"\360",	/* small eth, Icelandic */ 
  	"\353",	/* small e, dieresis or umlaut mark */ 
  	"\076",	/* greater than */ 
  	"\355",	/* small i, acute accent */ 
  	"\356",	/* small i, circumflex accent */ 
  	"\354",	/* small i, grave accent */ 
  	"\357",	/* small i, dieresis or umlaut mark */ 
  	"\074",	/* less than */ 
	"\001", /* nbsp non-breaking space NEVER CHANGE THIS*/
  	"\361",	/* small n, tilde */ 
  	"\363",	/* small o, acute accent */ 
  	"\364",	/* small o, circumflex accent */ 
  	"\362",	/* small o, grave accent */ 
  	"\370",	/* small o, slash */ 
  	"\365",	/* small o, tilde */ 
  	"\366",	/* small o, dieresis or umlaut mark */ 
	"\042", /* quote '"' */
  	"\337",	/* small sharp s, German (sz ligature) */ 
  	"\376",	/* small thorn, Icelandic */ 
  	"\372",	/* small u, acute accent */ 
  	"\373",	/* small u, circumflex accent */ 
  	"\371",	/* small u, grave accent */ 
  	"\374",	/* small u, dieresis or umlaut mark */ 
  	"\375",	/* small y, acute accent */ 
  	"\377",	/* small y, dieresis or umlaut mark */ 
};
static char * DEC_Multinational[] = {
  	"\306",	/* capital AE diphthong (ligature) */ 
  	"\301",	/* capital A, acute accent */ 
  	"\302",	/* capital A, circumflex accent */ 
  	"\300",	/* capital A, grave accent */ 
  	"\305",	/* capital A, ring */ 
  	"\303",	/* capital A, tilde */ 
  	"\304",	/* capital A, dieresis or umlaut mark */ 
  	"\307",	/* capital C, cedilla */ 
  	"DH",	/* capital Eth, Icelandic */ 
  	"\311",	/* capital E, acute accent */ 
  	"\312",	/* capital E, circumflex accent */ 
  	"\310",	/* capital E, grave accent */ 
  	"\313",	/* capital E, dieresis or umlaut mark */ 
  	"\315",	/* capital I, acute accent */ 
  	"\316",	/* capital I, circumflex accent */ 
  	"\314",	/* capital I, grave accent */ 
  	"\317",	/* capital I, dieresis or umlaut mark */ 
  	"\321",	/* capital N, tilde */ 
  	"\323",	/* capital O, acute accent */ 
  	"\324",	/* capital O, circumflex accent */ 
  	"\322",	/* capital O, grave accent */ 
  	"\330",	/* capital O, slash */ 
  	"\325",	/* capital O, tilde */ 
  	"\326",	/* capital O, dieresis or umlaut mark */ 
  	"TH",	/* capital THORN, Icelandic */ 
  	"\332",	/* capital U, acute accent */ 
  	"\333",	/* capital U, circumflex accent */ 
  	"\331",	/* capital U, grave accent */ 
  	"\334",	/* capital U, dieresis or umlaut mark */ 
  	"\335",	/* capital Y, acute accent */ 
  	"\341",	/* small a, acute accent */ 
  	"\342",	/* small a, circumflex accent */ 
  	"\346",	/* small ae diphthong (ligature) */ 
  	"\340",	/* small a, grave accent */ 
  	"\046",	/* ampersand */ 
  	"\345",	/* small a, ring */ 
  	"\343",	/* small a, tilde */ 
  	"\344",	/* small a, dieresis or umlaut mark */ 
  	"\347",	/* small c, cedilla */ 
  	"\351",	/* small e, acute accent */ 
  	"\352",	/* small e, circumflex accent */ 
  	"\350",	/* small e, grave accent */
	"\002",	/* emsp, em space - not collapsed NEVER CHANGE THIS */
	"\002",	/* ensp, en space - not collapsed NEVER CHANGE THIS */
  	"dh",	/* small eth, Icelandic */ 
  	"\353",	/* small e, dieresis or umlaut mark */ 
  	"\076",	/* greater than */ 
  	"\355",	/* small i, acute accent */ 
  	"\356",	/* small i, circumflex accent */ 
  	"\354",	/* small i, grave accent */ 
  	"\357",	/* small i, dieresis or umlaut mark */ 
  	"\074",	/* less than */ 
	"\001", /* nbsp non-breaking space NEVER CHANGE THIS*/
  	"\361",	/* small n, tilde */ 
  	"\363",	/* small o, acute accent */ 
  	"\364",	/* small o, circumflex accent */ 
  	"\362",	/* small o, grave accent */ 
  	"\370",	/* small o, slash */ 
  	"\365",	/* small o, tilde */ 
  	"\366",	/* small o, dieresis or umlaut mark */ 
	"\042", /* quote '"' */
  	"\337",	/* small sharp s, German (sz ligature) */ 
  	"th",	/* small thorn, Icelandic */ 
  	"\372",	/* small u, acute accent */ 
  	"\373",	/* small u, circumflex accent */ 
  	"\371",	/* small u, grave accent */ 
  	"\374",	/* small u, dieresis or umlaut mark */ 
  	"y'",	/* small y, acute accent */ 
  	"\375",	/* small y, dieresis or umlaut mark */ 
};
static char * PC_charset[] = {
        "\222", /* capital AE diphthong (ligature) */
        "\265", /* capital A, acute accent */
        "\266", /* capital A, circumflex accent */
        "\267", /* capital A, grave accent */
        "\217", /* capital A, ring */
        "\307", /* capital A, tilde */
        "\216", /* capital A, dieresis or umlaut mark */
        "\200", /* capital C, cedilla */
        "\321", /* capital Eth, Icelandic */
        "\220", /* capital E, acute accent */
        "\322", /* capital E, circumflex accent */
        "\324", /* capital E, grave accent */
        "\323", /* capital E, dieresis or umlaut mark */
        "\326", /* capital I, acute accent */
        "\327", /* capital I, circumflex accent */
        "\336", /* capital I, grave accent */
        "\330", /* capital I, dieresis or umlaut mark */
        "\245", /* capital N, tilde */
        "\340", /* capital O, acute accent */
        "\342", /* capital O, circumflex accent */
        "\343", /* capital O, grave accent */
        "\235", /* capital O, slash */
        "\345", /* capital O, tilde */
        "\231", /* capital O, dieresis or umlaut mark */
        "\350", /* capital THORN, Icelandic */
        "\351", /* capital U, acute accent */
        "\352", /* capital U, circumflex accent */
        "\353", /* capital U, grave accent */
        "\232", /* capital U, dieresis or umlaut mark */
        "\355", /* capital Y, acute accent */
        "\240", /* small a, acute accent */
        "\203", /* small a, circumflex accent */
        "\221", /* small ae diphthong (ligature) */
        "\205", /* small a, grave accent */
        "\046", /* ampersand */
        "\206", /* small a, ring */
        "\306", /* small a, tilde */
        "\204", /* small a, dieresis or umlaut mark */
        "\207", /* small c, cedilla */
        "\212", /* small e, acute accent */
        "\210", /* small e, circumflex accent */
        "\212", /* small e, grave accent */
        "\002", /* emsp NEVER CHANGE THIS*/
        "\002", /* ensp NEVER CHANGE THIS*/
        "\320", /* small eth, Icelandic */
        "\211", /* small e, dieresis or umlaut mark */
        "\076", /* greater than */
        "\241", /* small i, acute accent */
        "\214", /* small i, circumflex accent */
        "\215", /* small i, grave accent */
        "\213", /* small i, dieresis or umlaut mark */
        "\074", /* less than */
        "\001", /* nbsp non-breaking space NEVER CHANGE THIS*/
        "\244", /* small n, tilde */
        "\242", /* small o, acute accent */
        "\223", /* small o, circumflex accent */
        "\225", /* small o, grave accent */
        "\233", /* small o, slash */
        "\344", /* small o, tilde */
        "\224", /* small o, dieresis or umlaut mark */
        "\042", /* quote '"' */
        "\341", /* small sharp s, German (sz ligature) */
        "\347", /* small thorn, Icelandic */
        "\243", /* small u, acute accent */
        "\226", /* small u, circumflex accent */
        "\227", /* small u, grave accent */
        "\201", /* small u, dieresis or umlaut mark */
        "\354", /* small y, acute accent */
        "\230", /* small y, dieresis or umlaut mark */
};
/*      Entity values -- 7 bit character approximations
**
**      This MUST match exactly the table referred to in the DTD!
*/
static char * SevenBitApproximations[] = {
        "AE", /* capital AE diphthong (ligature) */
        "A", /* capital A, acute accent */
        "A", /* capital A, circumflex accent */
        "A", /* capital A, grave accent */
        "A", /* capital A, ring */
        "A", /* capital A, tilde */
        "A", /* capital A, dieresis or umlaut mark */
        "C", /* capital C, cedilla */
        "Eth", /* capital Eth, Icelandic */
        "E", /* capital E, acute accent */
        "E", /* capital E, circumflex accent */
        "E", /* capital E, grave accent */
        "E", /* capital E, dieresis or umlaut mark */
        "I", /* capital I, acute accent */
        "I", /* capital I, circumflex accent */
        "I", /* capital I, grave accent */
        "I", /* capital I, dieresis or umlaut mark */
        "N", /* capital N, tilde */
        "O", /* capital O, acute accent */
        "O", /* capital O, circumflex accent */
        "O", /* capital O, grave accent */
        "O", /* capital O, slash */
        "O", /* capital O, tilde */
        "O", /* capital O, dieresis or umlaut mark */
        "P", /* capital THORN, Icelandic */
        "U", /* capital U, acute accent */
        "U", /* capital U, circumflex accent */
        "U", /* capital U, grave accent */
        "U", /* capital U, dieresis or umlaut mark */
        "Y", /* capital Y, acute accent */
        "a", /* small a, acute accent */
        "a", /* small a, circumflex accent */
        "ae", /* small ae diphthong (ligature) */
        "`a", /* small a, grave accent */
        "&", /* ampersand */
        "a", /* small a, ring */
        "a", /* small a, tilde */
        "a", /* small a, dieresis or umlaut mark */
        "c", /* small c, cedilla */
        "e", /* small e, acute accent */
        "e", /* small e, circumflex accent */
        "e", /* small e, grave accent */
        "\002", /* emsp NEVER CHANGE THIS*/
        "\002", /* ensp NEVER CHANGE THIS*/
        "e", /* small eth, Icelandic */
        "e", /* small e, dieresis or umlaut mark */
        ">", /* greater than */
        "i", /* small i, acute accent */
        "i", /* small i, circumflex accent */
        "`i", /* small i, grave accent */
        "i", /* small i, dieresis or umlaut mark */
        "<", /* less than */
        "\001", /* nbsp non-breaking space NEVER CHANGE THIS*/
        "n", /* small n, tilde */
        "o", /* small o, acute accent */
        "o", /* small o, circumflex accent */
        "o", /* small o, grave accent */
        "o", /* small o, slash */
        "o", /* small o, tilde */
        "o", /* small o, dieresis or umlaut mark */
        "\"", /* quote '"' */
        "s", /* small sharp s, German (sz ligature) */
        "p", /* small thorn, Icelandic */
        "u", /* small u, acute accent */
        "u", /* small u, circumflex accent */
        "u", /* small u, grave accent */
        "u", /* small u, dieresis or umlaut mark */
        "y", /* small y, acute accent */
        "y", /* small y, dieresis or umlaut mark */
};


/*
 * Add your new character sets HERE 
 */

/* 
 * Add the array name to LYCharSets
 */

PUBLIC char ** LYCharSets[]={
        ISO_Latin1,
	DEC_Multinational,
        PC_charset,
        SevenBitApproximations
};

/* Add the name that the user will see below.
 * The order of LYCharSets and char_set_names MUST be the same
 */

PUBLIC char * LYchar_set_names[]={
        "ISO Latin 1         ",
	"DEC Multinational   ",
        "IBM PC character set",
        "7 bit approximations",
        (char *) 0
};

