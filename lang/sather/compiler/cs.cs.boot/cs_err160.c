/* cs_err160.c : Sather class: CS_ERROR, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern void ERR96_format_error_msg_(ptr self__, int ln__, ptr s__);
extern void ERR96_format_error_msg_file_(ptr self__, ptr file__, int ln__, ptr cls__, ptr s__);
extern void ERR96_format_error_exit_(ptr self__, int ln__, ptr s__);
extern ptr ERR96_filename_(ptr self__);
extern /*shared*/ ptr GLO68_str_table_;
extern ptr STR69_at_index_(ptr self__, int i__);
extern void ERR96_error_msg_(ptr self__, ptr s__);
extern ptr STR20_create_(ptr self__);
extern ptr STR20_s_(ptr self__, ptr st__);
extern ptr STR20_c_(ptr self__, char ch__);
#include "macros_.h"



void CS_160_undefined_feature_(ptr self__, int name__, int line__);
void CS_160_unimplemented_feature_(ptr self__, int name__, int line__);
void CS_160_repeated_type_parameters_(ptr self__, ptr file__, int line__, int p1__, int p2__);
void CS_160_wrong_number_of_type_parameters_(ptr self__, ptr file__, int line__);
void CS_160_cyclic_inheritance_(ptr self__, ptr file__, int line__, ptr name__);
void CS_160_basic_with_attributes_(ptr self__, int lineno__);
void CS_160_non_inheritable_(ptr self__, ptr name__, int line__);
void CS_160_incompatible_inheritance_(ptr self__, ptr name__, ptr c1__, ptr c2__);
void CS_160_basic_cross_inheritance_(ptr self__, ptr name__);
ptr CS_160_initialize_(ptr self__, ptr initarg__);
extern int attr_ent_CS_160[];

void CS_160_undefined_feature_(ptr self__, int name__, int line__)
{
   SATHER_STR_(20,41,ls1474_,"Error (CLASSOB_S): Cannot find feature \"");
   SATHER_STR_(20,3,ls632_,"\"\n");

   ERR96_format_error_msg_(0,line__,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1474_)),STR69_at_index_(GLO68_str_table_,name__)),(ptr)(&ls632_)));

   ret0__:
   return;
}

void CS_160_unimplemented_feature_(ptr self__, int name__, int line__)
{
   SATHER_STR_(20,38,ls1475_,"Error (CLASSOB_S): Abstract feature \"");
   SATHER_STR_(20,22,ls1476_,"\" in concrete class.\n");

   ERR96_format_error_msg_(0,line__,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1475_)),STR69_at_index_(GLO68_str_table_,name__)),(ptr)(&ls1476_)));

   ret0__:
   return;
}

void CS_160_repeated_type_parameters_(ptr self__, ptr file__, int line__, int p1__, int p2__)
{
   SATHER_STR_(20,10,ls262_,"CLASSOB_S");
   SATHER_STR_(20,27,ls1480_,"Repeated type parameters \"");
   SATHER_STR_(20,8,ls1481_,"\" and \"");
   SATHER_STR_(20,2,ls785_,"\"");

   ERR96_format_error_msg_file_(0,file__,line__,(ptr)(&ls262_),STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1480_)),STR69_at_index_(GLO68_str_table_,p1__)),(ptr)(&ls1481_)),STR69_at_index_(GLO68_str_table_,p2__)),(ptr)(&ls785_)));

   ret0__:
   return;
}

void CS_160_wrong_number_of_type_parameters_(ptr self__, ptr file__, int line__)
{
   SATHER_STR_(20,10,ls262_,"CLASSOB_S");
   SATHER_STR_(20,32,ls1482_,"Wrong number of type parameters");

   ERR96_format_error_msg_file_(0,file__,line__,(ptr)(&ls262_),(ptr)(&ls1482_));

   ret0__:
   return;
}

void CS_160_cyclic_inheritance_(ptr self__, ptr file__, int line__, ptr name__)
{
   SATHER_STR_(20,10,ls262_,"CLASSOB_S");
   SATHER_STR_(20,32,ls1483_,"Cycle in class inheritance of \"");
   SATHER_STR_(20,2,ls785_,"\"");

   ERR96_format_error_msg_file_(0,file__,line__,(ptr)(&ls262_),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1483_)),name__),(ptr)(&ls785_)));

   ret0__:
   return;
}

void CS_160_basic_with_attributes_(ptr self__, int lineno__)
{
   SATHER_STR_(20,69,ls1485_,"(CLASSOB_S): Descendent of basic class must not have any attributes\n");

   ERR96_format_error_exit_(0,lineno__,STR20_s_(STR20_create_(0),(ptr)(&ls1485_)));

   ret0__:
   return;
}

void CS_160_non_inheritable_(ptr self__, ptr name__, int line__)
{
   SATHER_STR_(20,10,ls262_,"CLASSOB_S");
   SATHER_STR_(20,20,ls1486_," is not inheritable");

   ERR96_format_error_msg_file_(0,ERR96_filename_(0),line__,(ptr)(&ls262_),STR20_s_(STR20_s_(STR20_create_(0),name__),(ptr)(&ls1486_)));

   ret0__:
   return;
}

void CS_160_incompatible_inheritance_(ptr self__, ptr name__, ptr c1__, ptr c2__)
{
   SATHER_STR_(20,44,ls1487_," (CLASSOB_S): Incompatible inheritance -- \"");
   SATHER_STR_(20,8,ls1481_,"\" and \"");
   SATHER_STR_(20,3,ls632_,"\"\n");

   ERR96_error_msg_(0,STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_c_(STR20_s_(STR20_c_(STR20_s_(STR20_create_(0),ERR96_filename_(0)),'('),name__),')'),(ptr)(&ls1487_)),c1__),(ptr)(&ls1481_)),c2__),(ptr)(&ls632_)));

   ret0__:
   return;
}

void CS_160_basic_cross_inheritance_(ptr self__, ptr name__)
{
   SATHER_STR_(20,27,ls1488_,"(CLASSOB_S): Basic class \"");
   SATHER_STR_(20,41,ls1489_,"\" inherits from class of different root\n");

   ERR96_error_msg_(0,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1488_)),name__),(ptr)(&ls1489_)));

   ret0__:
   return;
}

ptr CS_160_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

