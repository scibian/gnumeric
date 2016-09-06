/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
#ifndef _GNM_PRINT_INFO_H_
# define _GNM_PRINT_INFO_H_

#include "gnumeric.h"
#include <gtk/gtk.h>
#include "print.h"

G_BEGIN_DECLS

GType gnm_print_comment_placement_get_type (void);
#define GNM_PRINT_COMMENT_PLACEMENT_TYPE (gnm_print_comment_placement_get_type ())

GType gnm_print_errors_get_type (void);
#define GNM_PRINT_ERRORS_TYPE (gnm_print_errors_get_type ())


typedef struct {
	GtkUnit   top, bottom, left, right, header, footer;
} DesiredDisplay;

/* Header/Footer definition */
typedef struct {
	char *left_format;
	char *middle_format;
	char *right_format;
} PrintHF;

typedef enum {
	GNM_PRINT_COMMENTS_NONE,
	GNM_PRINT_COMMENTS_IN_PLACE,
	GNM_PRINT_COMMENTS_AT_END
} GnmPrintCommentPlacementType;

typedef enum {
	GNM_PRINT_ERRORS_AS_DISPLAYED,
	GNM_PRINT_ERRORS_AS_BLANK,
	GNM_PRINT_ERRORS_AS_DASHES,
	GNM_PRINT_ERRORS_AS_NA
} GnmPrintErrorsType;

typedef enum {
	GNM_PAGE_BREAK_NONE,    /* Not actually a page break  */
	GNM_PAGE_BREAK_MANUAL,  /* hard page break            */
	GNM_PAGE_BREAK_AUTO,    /* soft (automatic) pagebreak */
	GNM_PAGE_BREAK_DATA_SLICE  /* place holder ?          */
} GnmPageBreakType;
GnmPageBreakType gnm_page_break_type_from_str (char const *str);

typedef struct {
	int		 pos;  /* break before this 0 based position */
	GnmPageBreakType type;
} GnmPageBreak;

typedef struct {
	gboolean is_vert;
	GArray	*details; /* ordered array of GnmPageBreak  */
} GnmPageBreaks;

struct _PrintInformation {
	struct _PrintInfoScaling {
		enum _PrintScaleType {
			PRINT_SCALE_PERCENTAGE,
			PRINT_SCALE_FIT_PAGES
		} type;

		/* We store separate x and y scales internally, for the
		* 'fit-to' printing feature. (They are calculated at print-time)
		* When the user is doing the simple scaling, both these values
		* will be equal.
		*/
		struct _PrintScalePercent {
			double x;
			double y;
		} percentage;

		struct _PrintScaleDim { /* zero == use as many as required */
			int cols;
			int rows;
		} dim;
	} scaling;
	double           edge_to_below_header;
	double           edge_to_above_footer;
        DesiredDisplay   desired_display;
	char            *repeat_top, *repeat_left;
	unsigned int	 print_across_then_down;
	unsigned int     center_vertically:1;
	unsigned int     center_horizontally:1;
	unsigned int     print_grid_lines:1;
	unsigned int     print_titles:1;	/* col/row headers */
	unsigned int     print_black_and_white:1;
	unsigned int     print_as_draft:1;

	/* Gnumeric specific */
	unsigned int     print_even_if_only_styles:1;
	unsigned int     do_not_print:1;

	GnmPrintCommentPlacementType comment_placement;
	GnmPrintErrorsType error_display;

	struct _PrintInfoPageBreaks {
		GnmPageBreaks *h,  /* between rows */
			      *v;  /* between columns */
	} page_breaks;
	PrintHF		*header, *footer;

	int		  start_page; /* < 0 implies auto */
        int              n_copies;

	gchar           *printtofile_uri;
	PrintRange       print_range;

  /* page_setup doubles as a flag whether the defaults are loaded */
        GtkPageSetup     *page_setup;
};

typedef enum {
	HF_RENDER_PRINT
} HFRenderType;

typedef struct {
	Sheet const *sheet;
	int       page;
	int       pages;
	GnmValue *date_time;
	GODateConventions const *date_conv;
	GnmRange  page_area;
	GnmCellPos top_repeating;
} HFRenderInfo;

GType             print_information_get_type (void);
PrintInformation *print_information_new         (gboolean load_defaults);
PrintInformation *print_info_load_defaults (PrintInformation *pi);
PrintInformation *print_info_dup	 (PrintInformation const *pi);
void              print_info_free        (PrintInformation *pi);
void              print_info_save        (PrintInformation *pi);

GtkPageSetup     *print_info_get_page_setup (PrintInformation *pi); /* Does not return a ref! */
void              print_info_set_page_setup (PrintInformation *pi, GtkPageSetup *page_setup);

GType             print_hf_get_type      (void);
PrintHF          *print_hf_new           (char const *left,
					  char const *middle,
				          char const *right);
void              print_hf_free          (PrintHF *print_hf);
PrintHF          *print_hf_copy          (PrintHF const *source);
PrintHF          *print_hf_register      (PrintHF *hf);
gboolean          print_hf_same          (PrintHF const *a, PrintHF const *b);

char             *hf_format_render       (char const *format,
					  HFRenderInfo *info,
					  HFRenderType render_type);

GType             hf_render_info_get_type      (void);
HFRenderInfo     *hf_render_info_new     (void);
void              hf_render_info_destroy (HFRenderInfo *hfi);


GtkUnit     unit_name_to_unit    (char const *name);
char const *unit_to_unit_name (GtkUnit unit);

void        print_init               (void);
void        print_shutdown           (void);

gboolean    page_setup_set_paper (GtkPageSetup *page_setup, char const *paper);
char	   *page_setup_get_paper (GtkPageSetup *page_setup); /* caller frees result */
gboolean    print_info_set_paper (PrintInformation *pi, char const *paper);
char       *print_info_get_paper (PrintInformation *pi); /* caller frees result */
GtkPaperSize *print_info_get_paper_size (PrintInformation *pi);

void	    print_info_set_paper_orientation   (PrintInformation *pi,
						GtkPageOrientation orientation);
char const *print_info_get_paper_display_name (PrintInformation *pi);

double      print_info_get_paper_width     (PrintInformation *pi, GtkUnit unit);
double      print_info_get_paper_height    (PrintInformation *pi, GtkUnit unit);
GtkPageOrientation print_info_get_paper_orientation   (PrintInformation *pi);
void        print_info_get_margins   (PrintInformation *pi,
				      double *top, double *bottom,
				      double *left, double *right,
				      double *edge_to_below_header,
				      double *edge_to_above_footer);
void        print_info_set_margins   (PrintInformation *pi,
				      double header, double footer, double left, double right);
void        print_info_set_margin_header (PrintInformation *pi, double header);
void        print_info_set_margin_footer (PrintInformation *pi, double footer);
void        print_info_set_margin_left   (PrintInformation *pi, double left);
void        print_info_set_margin_right  (PrintInformation *pi, double right);
void        print_info_set_edge_to_above_footer (PrintInformation *pi,
						 double e_f);
void        print_info_set_edge_to_below_header (PrintInformation *pi,
						 double e_h);
void        print_info_set_printtofile_uri (PrintInformation *pi,
					gchar const *uri);
void        print_info_set_printtofile_from_settings
                               (PrintInformation *pi,
				GtkPrintSettings *settings,
				gchar const *default_uri);
void        print_info_set_from_settings
                               (PrintInformation *pi,
				GtkPrintSettings *settings);
char const *print_info_get_printtofile_uri (PrintInformation *pi);
PrintRange  print_info_get_printrange (PrintInformation *pi);
void        print_info_set_printrange (PrintInformation *pi, PrintRange pr);

void        print_info_set_breaks (PrintInformation *pi, GnmPageBreaks *breaks);

gboolean        print_info_has_manual_breaks (PrintInformation *pi);

GType            gnm_page_breaks_get_type       (void);
GnmPageBreaks	*gnm_page_breaks_new		(gboolean is_vert);
GnmPageBreaks	*gnm_page_breaks_dup		(GnmPageBreaks const *src);
void		 gnm_page_breaks_free		(GnmPageBreaks *breaks);
void		 gnm_page_breaks_clean		(GnmPageBreaks *breaks);
gboolean	 gnm_page_breaks_append_break	(GnmPageBreaks *breaks,
						 int pos,
						 GnmPageBreakType type);
gboolean	 gnm_page_breaks_set_break	(GnmPageBreaks *breaks,
						 int pos,
						 GnmPageBreakType type);
GnmPageBreakType gnm_page_breaks_get_break      (GnmPageBreaks *breaks, int pos);
int              gnm_page_breaks_get_next_manual_break  (GnmPageBreaks *breaks, int pos);
int              gnm_page_breaks_get_next_break  (GnmPageBreaks *breaks, int pos);
GnmPageBreaks *  gnm_page_breaks_dup_non_auto_breaks (GnmPageBreaks const *src);

gboolean         print_load_repeat_range (char const *str, GnmRange *r, Sheet const *sheet);



/* Formats known */
extern GList *hf_formats;

G_END_DECLS

#endif /* _GNM_PRINT_INFO_H_ */
