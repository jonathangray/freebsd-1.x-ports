extern char	*malloc();
extern char	*calloc();

#define		Line_malloc(z)		z = (F_line*)malloc(LINOBJ_SIZE)
#define		Spline_malloc(z)	z = (F_spline*)malloc(SPLOBJ_SIZE)
#define		Ellipse_malloc(z)	z = (F_ellipse*)malloc(ELLOBJ_SIZE)
#define		Arc_malloc(z)		z = (F_arc*)malloc(ARCOBJ_SIZE)
#define		Compound_malloc(z)	z = (F_compound*)malloc(COMOBJ_SIZE)
#define		Text_malloc(z)		z = (F_text*)malloc(TEXOBJ_SIZE)
#define		Point_malloc(z)		z = (F_point*)malloc(POINT_SIZE)
#define		Control_malloc(z)	z = (F_control*)malloc(CONTROL_SIZE)
#define		Arrow_malloc(z)		z = (F_arrow*)malloc(ARROW_SIZE)

extern char	Err_mem[];
extern char	Err_incomp[];
