# include "xmath.h"
# include "xlisp.h"
# include "statfloat.h"

extern double makedouble();
extern char *calloc();
extern double macheps();

# define realp(x) (floatp(x) || fixp(x))
# define seqlen(x) ((vectorp(x)) ? getsize(x) : llength(x))

#define nil 0L

typedef char **Matrix, *Vector;
typedef int **IMatrix, *IVector;
typedef double **RMatrix, *RVector;
typedef Complex **CMatrix, *CVector;

#define IN 0
#define RE 1
#define CX 2

/* external symbols */
extern LVAL s_true, a_fixnum, a_flonum, a_complex;

/* external functions */
extern LVAL arraydata(), getnextelement(), coerce_to_list(), newarray(),
  integer_list_2(), mklist();
extern IVector ivector();
extern RVector rvector();
extern CVector cvector();
extern IMatrix imatrix();
extern RMatrix rmatrix();
extern CMatrix cmatrix();
