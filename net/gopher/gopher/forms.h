/*
 * This defines the form type.
 */

struct form_struct {
     int  numfields;
     char *Title;
     char **tags;
     char **values;
     int  *lengths;
     int  *xloc;
     int  *yloc;
};

typedef struct form_struct Form;

/** Operations on Forms ***/

BOOLEAN NewForm(/* Form* */);
void ModifyForm(/* Form* */);
void DisplayForm(/* Form* */);
void PrintForm(/* Form * */);
