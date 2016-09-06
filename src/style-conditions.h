/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
#ifndef _GNM_STYLE_CONDITIONS_H_
# define _GNM_STYLE_CONDITIONS_H_

#include "gnumeric.h"
#include <dependent.h>

G_BEGIN_DECLS

/* This is persisted directly in .gnumeric files, DO NOT REMOVE OR REORDER */
typedef enum {
	/* Cell Value */
	GNM_STYLE_COND_BETWEEN,
	GNM_STYLE_COND_NOT_BETWEEN,
	GNM_STYLE_COND_EQUAL,
	GNM_STYLE_COND_NOT_EQUAL,
	GNM_STYLE_COND_GT,
	GNM_STYLE_COND_LT,
	GNM_STYLE_COND_GTE,
	GNM_STYLE_COND_LTE,

	/* Arbitrary expr evaluated at EvalPos */
	GNM_STYLE_COND_CUSTOM,

	/* New in Gnumeric 1.8 */
	GNM_STYLE_COND_CONTAINS_STR		= 0x10,
	GNM_STYLE_COND_NOT_CONTAINS_STR,
	GNM_STYLE_COND_BEGINS_WITH_STR,
	GNM_STYLE_COND_NOT_BEGINS_WITH_STR,
	GNM_STYLE_COND_ENDS_WITH_STR,
	GNM_STYLE_COND_NOT_ENDS_WITH_STR,

	GNM_STYLE_COND_CONTAINS_ERR,
	GNM_STYLE_COND_NOT_CONTAINS_ERR,

	GNM_STYLE_COND_CONTAINS_BLANKS,
	GNM_STYLE_COND_NOT_CONTAINS_BLANKS
} GnmStyleCondOp;

typedef struct {
	GnmStyle	 *overlay;
	GnmDependent      deps[2];
	GnmStyleCondOp	  op;
} GnmStyleCond;

GType         gnm_style_cond_get_type (void);
GnmStyleCond *gnm_style_cond_new (GnmStyleCondOp op, Sheet *sheet);
void gnm_style_cond_free (GnmStyleCond *cond);
GnmStyleCond *gnm_style_cond_dup (GnmStyleCond const *src);
gboolean      gnm_style_cond_is_valid (GnmStyleCond const *cond);

void          gnm_style_cond_set_overlay (GnmStyleCond *cond,
					  GnmStyle *overlay);

GnmExprTop const *gnm_style_cond_get_expr (GnmStyleCond const *cond,
					   unsigned idx);
void          gnm_style_cond_set_expr (GnmStyleCond *cond,
				       GnmExprTop const *texpr,
				       unsigned idx);

GnmExprTop const *gnm_style_cond_get_alternate_expr (GnmStyleCond const *cond);
void gnm_style_cond_canonicalize (GnmStyleCond *cond);

Sheet      *gnm_style_cond_get_sheet (GnmStyleCond const *cond);
void        gnm_style_cond_set_sheet (GnmStyleCond *cond, Sheet *sheet);

GType         gnm_style_conditions_get_type (void);
GnmStyleConditions *gnm_style_conditions_new  (Sheet *sheet);
GnmStyleConditions *gnm_style_conditions_dup  (GnmStyleConditions const *sc);
GPtrArray const *gnm_style_conditions_details (GnmStyleConditions const *sc);
void	      gnm_style_conditions_insert  (GnmStyleConditions *sc,
					    GnmStyleCond const *cond,
					    int pos);
void	      gnm_style_conditions_delete  (GnmStyleConditions *sc,
					    guint pos);
GPtrArray    *gnm_style_conditions_overlay (GnmStyleConditions const *sc,
					    GnmStyle const *base);
int	      gnm_style_conditions_eval    (GnmStyleConditions const *sc,
					    GnmEvalPos const *pos);

Sheet      *gnm_style_conditions_get_sheet (GnmStyleConditions const *sc);
void        gnm_style_conditions_set_sheet (GnmStyleConditions *sc,
					    Sheet *sheet);

guint32     gnm_style_conditions_hash      (GnmStyleConditions const *sc);

G_END_DECLS

#endif /* _GNM_STYLE_CONDITIONS_H_ */
