/* lst_ex120.c : Sather class: LST_EXPROB_S, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern ptr STR20_create_(ptr self__);
extern ptr STR20_s_(ptr self__, ptr st__);
extern ptr STR20_c_(ptr self__, char ch__);
extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern ptr LIS98_append_(ptr self__, ptr list__);
extern ptr SAT99_c_(ptr self__, char ch__);
extern ptr LST102_create_(ptr self__, int init_size__);
extern ptr LST102_push_(ptr self__, ptr e__);
extern void EXP117_out_of_line_(ptr self__, ptr fn__);
extern ptr EXP117_dup_(ptr self__);
extern ptr EXP117_sather_code_(ptr self__);
extern void EXP117_resolve_predef_types_(ptr self__, int index__);
extern void EXP117_semant_(ptr self__, ptr symtab__);
extern void EXP117_cprint_init_code_(ptr self__, ptr outfile__);
extern void EXP117_get_ext_strs_(ptr self__);
extern char EXP117_valid_init_expr_(ptr self__);
extern ptr EXP117_gen_temps_(ptr self__);
extern void EXP117_cprint_pre_code_(ptr self__, ptr outfile__);
extern void EXP117_cprint_act_code_(ptr self__, ptr outfile__);
#include "macros_.h"



/*constant*/ int LST120_print_indent_ = 2;
ptr LST120_create_(ptr self__, int init_size__);
void LST120_out_of_line_(ptr self__, ptr fn__);
ptr LST120_dup_(ptr self__);
void LST120_put_kwdname_(ptr self__, int nm__);
ptr LST120_sather_code_(ptr self__);
ptr LST120_initialize_(ptr self__, ptr initarg__);
void LST120_resolve_predef_types_(ptr self__, int index__);
void LST120_semant_(ptr self__, ptr symtab__);
ptr LST120_typeof_(ptr self__);
int LST120_get_offset_(ptr self__);
void LST120_cprint_offset_(ptr self__, ptr outfile__);
ptr LST120_get_constval_(ptr self__);
void LST120_cont_cprint_code_(ptr self__, ptr outfile__, int cont__);
void LST120_cprint_cname_(ptr self__, ptr outfile__);
void LST120_cprint_extern_(ptr self__, ptr outfile__);
void LST120_cprint_access_value_(ptr self__, ptr outfile__);
void LST120_cprint_init_code_(ptr self__, ptr outfile__);
void LST120_clear_(ptr self__);
/*constant*/ int LST120_def_init_size_ = 5;
ptr LST120_push_(ptr self__, ptr e__);
int LST120_size_(ptr self__);
char LST120_is_empty_(ptr self__);
ptr LST120_pop_(ptr self__);
ptr LST120_top_(ptr self__);
void LST120_init_iterate_(ptr self__);
ptr LST120_curr_item_(ptr self__);
ptr LST120_next_item_(ptr self__);
ptr LST120_prev_item_(ptr self__);
ptr LST120_push_unique_(ptr self__, ptr e__);
ptr LST120_append_(ptr self__, ptr list__);
ptr LST120_union_(ptr self__, ptr list__);
char LST120_not_in_(ptr self__, ptr e__);
int LST120_contains_(ptr self__, ptr e__);
void LST120_cprint_code_(ptr self__, ptr outfile__);
void LST120_get_ext_strs_(ptr self__);
char LST120_valid_init_expr_(ptr self__);
ptr LST120_gen_temps_(ptr self__);
void LST120_cprint_pre_code_(ptr self__, ptr outfile__);
void LST120_cprint_act_code_(ptr self__, ptr outfile__);
extern int attr_ent_LST120[];

ptr LST120_create_(ptr self__, int init_size__)
{
   ptr res__ = 0;

   if ((init_size__ <= 0)) {
      res__ = (ptr)new1_(120,5,0);
   }
   else {
      res__ = (ptr)new1_(120,init_size__,0);
   }

   ret0__:
   return (res__);
}

void LST120_out_of_line_(ptr self__, ptr fn__)
{
   ptr gl2948_;
   static int gl2949_;
   static union dtype_ gl2950_;
   int    i__ = S_int_VOID_;

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));
   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_2951_;
      }
      else {
      }
      gl2948_ = PATT_(self__, 28 + ((i__) << 2));
      cache_dispatch_(gl2948_,518,gl2949_,INTVAL_(gl2950_));
      VFN_(gl2950_)(gl2948_,fn__);
      i__ = (int)(i__ + 1);
   }
goto_tag_2951_: ;

   ret0__:
   return;
}

ptr LST120_dup_(ptr self__)
{
   ptr res__ = 0;
   ptr gl2952_;
   static int gl2953_;
   static union dtype_ gl2954_;
   ptr gl291_;
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   res__ = (ptr)LST120_create_(self__,IATT_(self__,12));
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_2955_;
      }
      else {
      }
      gl2952_ = PATT_(self__, 28 + ((i__) << 2));
      cache_dispatch_(gl2952_,471,gl2953_,INTVAL_(gl2954_));
      gl291_ = PFN_(gl2954_)(gl2952_);
      res__ = (ptr)LST120_push_(res__,gl291_);
      i__ = (int)(i__ + 1);
   }
goto_tag_2955_: ;

   ret0__:
   return (res__);
}

void LST120_put_kwdname_(ptr self__, int nm__)
{
   ptr gl2956_;
   static int gl2957_;
   static union dtype_ gl2958_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl2956_ = x__;
   cache_dispatch_(gl2956_,796,gl2957_,INTVAL_(gl2958_));
   IATT_(gl2956_,INTVAL_(gl2958_)) = (int)nm__;

   ret0__:
   return;
}

ptr LST120_sather_code_(ptr self__)
{
   ptr res__ = 0;
   ptr gl2959_;
   static int gl2960_;
   static union dtype_ gl2961_;
   ptr gl292_;
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   res__ = (ptr)STR20_create_(0);
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_2962_;
      }
      else {
      }
      gl2959_ = PATT_(self__, 28 + ((i__) << 2));
      cache_dispatch_(gl2959_,801,gl2960_,INTVAL_(gl2961_));
      gl292_ = PFN_(gl2961_)(gl2959_);
      res__ = (ptr)STR20_c_(STR20_s_(res__,gl292_),',');
      i__ = (int)(i__ + 1);
   }
goto_tag_2962_: ;

   ret0__:
   return (res__);
}

ptr LST120_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

void LST120_resolve_predef_types_(ptr self__, int index__)
{
   ptr gl2963_;
   static int gl2964_;
   static union dtype_ gl2965_;
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_2966_;
      }
      else {
      }
      gl2963_ = PATT_(self__, 28 + ((i__) << 2));
      cache_dispatch_(gl2963_,522,gl2964_,INTVAL_(gl2965_));
      VFN_(gl2965_)(gl2963_,index__);
      i__ = (int)(i__ + 1);
   }
goto_tag_2966_: ;

   ret0__:
   return;
}

void LST120_semant_(ptr self__, ptr symtab__)
{
   ptr gl2967_;
   static int gl2968_;
   static union dtype_ gl2969_;
   ptr gl2970_;
   static int gl2971_;
   static union dtype_ gl2972_;
   ptr gl293_;
   ptr gl2973_;
   static int gl2974_;
   static union dtype_ gl2975_;
   ptr gl294_;
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_2976_;
      }
      else {
      }
      gl2967_ = PATT_(self__, 28 + ((i__) << 2));
      cache_dispatch_(gl2967_,588,gl2968_,INTVAL_(gl2969_));
      VFN_(gl2969_)(gl2967_,symtab__);
      i__ = (int)(i__ + 1);
   }
goto_tag_2976_: ;
   PATT_(self__,20) = (ptr)LST102_create_(PATT_(self__,20),IATT_(self__,12));
   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_2977_;
      }
      else {
      }
      gl2970_ = PATT_(self__, 28 + ((i__) << 2));
      cache_dispatch_(gl2970_,1577,gl2971_,INTVAL_(gl2972_));
      gl293_ = PATT_(gl2970_,INTVAL_(gl2972_));
      if ((gl293_ != 0)) {
         gl2973_ = PATT_(self__, 28 + ((i__) << 2));
         cache_dispatch_(gl2973_,1577,gl2974_,INTVAL_(gl2975_));
         gl294_ = PATT_(gl2973_,INTVAL_(gl2975_));
         PATT_(self__,20) = (ptr)LST102_push_(PATT_(self__,20),gl294_);
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_2977_: ;

   ret0__:
   return;
}

ptr LST120_typeof_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)PATT_(self__,20);

   ret0__:
   return (res__);
}

int LST120_get_offset_(ptr self__)
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

void LST120_cprint_offset_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

ptr LST120_get_constval_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

void LST120_cont_cprint_code_(ptr self__, ptr outfile__, int cont__)
{


   ret0__:
   return;
}

void LST120_cprint_cname_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void LST120_cprint_extern_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void LST120_cprint_access_value_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void LST120_cprint_init_code_(ptr self__, ptr outfile__)
{
   ptr gl2978_;
   static int gl2979_;
   static union dtype_ gl2980_;
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_2981_;
      }
      else {
      }
      gl2978_ = PATT_(self__, 28 + ((i__) << 2));
      cache_dispatch_(gl2978_,724,gl2979_,INTVAL_(gl2980_));
      VFN_(gl2980_)(gl2978_,outfile__);
      i__ = (int)(i__ + 1);
   }
goto_tag_2981_: ;

   ret0__:
   return;
}

void LST120_clear_(ptr self__)
{
   int    i__ = S_int_VOID_;

   i__ = S_int_VOID_;
   while (1) {
      if ((i__ == IATT_(self__,24))) {
         goto goto_tag_2982_;
      }
      else {
      }
      PATT_(self__, 28 + ((i__) << 2)) = (ptr)0;
      i__ = (int)(i__ + 1);
   }
goto_tag_2982_: ;

   ret0__:
   return;
}

ptr LST120_push_(ptr self__, ptr e__)
{
   ptr res__ = 0;

   if ((IATT_(self__,12) < IATT_(self__,24))) {
      res__ = (ptr)self__;
   }
   else {
      res__ = (ptr)extend1_(self__,(2 * IATT_(self__,24)),0);
   }
   PATT_(res__, 28 + ((IATT_(self__,12)) << 2)) = (ptr)e__;
   IATT_(res__,12) = (int)(IATT_(res__,12) + 1);

   ret0__:
   return (res__);
}

int LST120_size_(ptr self__)
{
   int res__ = S_int_VOID_;

   if ((self__ == 0)) {
      res__ = (int)0;
   }
   else {
      res__ = (int)IATT_(self__,12);
   }

   ret0__:
   return (res__);
}

char LST120_is_empty_(ptr self__)
{
   char res__ = S_char_VOID_;

   res__ = (char)(IATT_(self__,12) == 0);

   ret0__:
   return (res__);
}

ptr LST120_pop_(ptr self__)
{
   ptr res__ = 0;

   if (LST120_is_empty_(self__)) {
      res__ = (ptr)0;
   }
   else {
      IATT_(self__,12) = (int)(IATT_(self__,12) - 1);
      res__ = (ptr)PATT_(self__, 28 + ((IATT_(self__,12)) << 2));
      PATT_(self__, 28 + ((IATT_(self__,12)) << 2)) = (ptr)0;
   }

   ret0__:
   return (res__);
}

ptr LST120_top_(ptr self__)
{
   ptr res__ = 0;

   if (LST120_is_empty_(self__)) {
      res__ = (ptr)0;
   }
   else {
      res__ = (ptr)PATT_(self__, 28 + (((IATT_(self__,12) - 1)) << 2));
   }

   ret0__:
   return (res__);
}

void LST120_init_iterate_(ptr self__)
{

   IATT_(self__,16) = (int)0;

   ret0__:
   return;
}

ptr LST120_curr_item_(ptr self__)
{
   ptr res__ = 0;

   if ((IATT_(self__,16) < IATT_(self__,12))) {
      res__ = (ptr)PATT_(self__, 28 + ((IATT_(self__,16)) << 2));
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr LST120_next_item_(ptr self__)
{
   ptr res__ = 0;

   if (((IATT_(self__,16) + 1) < IATT_(self__,12))) {
      IATT_(self__,16) = (int)(IATT_(self__,16) + 1);
      res__ = (ptr)PATT_(self__, 28 + ((IATT_(self__,16)) << 2));
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr LST120_prev_item_(ptr self__)
{
   ptr res__ = 0;

   if (((IATT_(self__,16) - 1) >= 0)) {
      IATT_(self__,16) = (int)(IATT_(self__,16) - 1);
      res__ = (ptr)PATT_(self__, 28 + ((IATT_(self__,16)) << 2));
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr LST120_push_unique_(ptr self__, ptr e__)
{
   ptr res__ = 0;
   int    k__ = S_int_VOID_;

   k__ = (int)LST120_contains_(self__,e__);
   if ((k__ >= 0)) {
      PATT_(self__, 28 + ((k__) << 2)) = (ptr)e__;
      res__ = (ptr)self__;
   }
   else {
      res__ = (ptr)LST120_push_(self__,e__);
   }

   ret0__:
   return (res__);
}

ptr LST120_append_(ptr self__, ptr list__)
{
   ptr res__ = 0;
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;

   if ((list__ == 0)) {
      res__ = (ptr)self__;
      goto ret0__;
   }
   else {
   }
   res__ = (ptr)self__;
   i__ = (int)0;
   sz__ = (int)IATT_(list__,12);
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_2983_;
      }
      else {
      }
      res__ = (ptr)LST120_push_(res__,PATT_(list__, 28 + ((i__) << 2)));
      i__ = (int)(i__ + 1);
   }
goto_tag_2983_: ;

   ret0__:
   return (res__);
}

ptr LST120_union_(ptr self__, ptr list__)
{
   ptr res__ = 0;
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;

   i__ = (int)0;
   sz__ = (int)IATT_(list__,12);
   res__ = (ptr)self__;
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_2984_;
      }
      else {
      }
      if (LST120_not_in_(self__,PATT_(list__, 28 + ((i__) << 2)))) {
         res__ = (ptr)LST120_push_(res__,PATT_(list__, 28 + ((i__) << 2)));
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_2984_: ;

   ret0__:
   return (res__);
}

char LST120_not_in_(ptr self__, ptr e__)
{
   char res__ = S_char_VOID_;
   int    i__ = S_int_VOID_;

   if ((self__ == 0)) {
      res__ = (char)1;
      goto ret0__;
   }
   else {
   }
   i__ = (int)0;
   res__ = (char)1;
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_2985_;
      }
      else {
      }
      if ((PATT_(self__, 28 + ((i__) << 2)) == e__)) {
         res__ = (char)0;
         goto ret0__;
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_2985_: ;

   ret0__:
   return (res__);
}

int LST120_contains_(ptr self__, ptr e__)
{
   int res__ = S_int_VOID_;
   int    i__ = S_int_VOID_;

   if ((self__ == 0)) {
      goto ret0__;
   }
   else {
   }
   i__ = (int)0;
   res__ = (int)(- 1);
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_2986_;
      }
      else {
      }
      if ((PATT_(self__, 28 + ((i__) << 2)) == e__)) {
         res__ = (int)i__;
         goto ret0__;
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_2986_: ;

   ret0__:
   return (res__);
}

void LST120_cprint_code_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void LST120_get_ext_strs_(ptr self__)
{
   ptr gl2987_;
   static int gl2988_;
   static union dtype_ gl2989_;
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_2990_;
      }
      else {
      }
      gl2987_ = PATT_(self__, 28 + ((i__) << 2));
      cache_dispatch_(gl2987_,1677,gl2988_,INTVAL_(gl2989_));
      VFN_(gl2989_)(gl2987_);
      i__ = (int)(i__ + 1);
   }
goto_tag_2990_: ;

   ret0__:
   return;
}

char LST120_valid_init_expr_(ptr self__)
{
   char res__ = S_char_VOID_;
   ptr gl2991_;
   static int gl2992_;
   static union dtype_ gl2993_;
   char gl295_;
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   res__ = (char)1;
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_2994_;
      }
      else {
      }
      gl2991_ = PATT_(self__, 28 + ((i__) << 2));
      cache_dispatch_(gl2991_,1580,gl2992_,INTVAL_(gl2993_));
      gl295_ = CFN_(gl2993_)(gl2991_);
      res__ = (char)(res__ & gl295_);
      i__ = (int)(i__ + 1);
   }
goto_tag_2994_: ;

   ret0__:
   return (res__);
}

ptr LST120_gen_temps_(ptr self__)
{
   ptr res__ = 0;
   ptr gl2995_;
   static int gl2996_;
   static union dtype_ gl2997_;
   ptr gl2998_;
   static int gl2999_;
   static union dtype_ gl3000_;
   ptr gl296_;
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_3001_;
      }
      else {
      }
      if ((res__ == 0)) {
         gl2995_ = PATT_(self__, 28 + ((i__) << 2));
         cache_dispatch_(gl2995_,1582,gl2996_,INTVAL_(gl2997_));
         res__ = (ptr)PFN_(gl2997_)(gl2995_);
      }
      else {
         gl2998_ = PATT_(self__, 28 + ((i__) << 2));
         cache_dispatch_(gl2998_,1582,gl2999_,INTVAL_(gl3000_));
         gl296_ = PFN_(gl3000_)(gl2998_);
         res__ = (ptr)LIS98_append_(res__,gl296_);
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_3001_: ;

   ret0__:
   return (res__);
}

void LST120_cprint_pre_code_(ptr self__, ptr outfile__)
{
   ptr gl3002_;
   static int gl3003_;
   static union dtype_ gl3004_;
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_3005_;
      }
      else {
      }
      gl3002_ = PATT_(self__, 28 + ((i__) << 2));
      cache_dispatch_(gl3002_,1589,gl3003_,INTVAL_(gl3004_));
      VFN_(gl3004_)(gl3002_,outfile__);
      i__ = (int)(i__ + 1);
   }
goto_tag_3005_: ;

   ret0__:
   return;
}

void LST120_cprint_act_code_(ptr self__, ptr outfile__)
{
   ptr gl3006_;
   static int gl3007_;
   static union dtype_ gl3008_;
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_3009_;
      }
      else {
      }
      gl3006_ = PATT_(self__, 28 + ((i__) << 2));
      cache_dispatch_(gl3006_,965,gl3007_,INTVAL_(gl3008_));
      VFN_(gl3008_)(gl3006_,outfile__);
      i__ = (int)(i__ + 1);
      if ((i__ < IATT_(self__,12))) {
         (void)SAT99_c_(outfile__,',');
      }
      else {
      }
   }
goto_tag_3009_: ;

   ret0__:
   return;
}

