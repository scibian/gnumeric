/* vim: set sw=8:  -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * colrow.c: Utilities for Rows and Columns
 *
 * Copyright (C) 1999-2007 Jody Goldberg (jody@gnome.org)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA
 */
#include <gnumeric-config.h>
#include "gnumeric.h"
#include "colrow.h"

#include "sheet.h"
#include "sheet-view.h"
#include "sheet-private.h"
#include "application.h"
#include "parse-util.h"
#include "selection.h"
#include "ranges.h"
#include "sheet-merge.h"
#include "cell.h"
#include "cellspan.h"
#include "rendered-value.h"
#include <goffice/goffice.h>

/* Making ColRowInfo a boxed type to make introspection happy. using no-op
 * functions for copy and free, and  crossing fingers.
 */
static ColRowInfo *
col_row_info_copy (ColRowInfo *cri)
{
	return cri;
}

GType
col_row_info_get_type (void)
{
	static GType t = 0;

	if (t == 0) {
		t = g_boxed_type_register_static ("ColRowInfo",
			 (GBoxedCopyFunc)col_row_info_copy,
			 (GBoxedFreeFunc)col_row_info_copy);
	}
	return t;
}

double
colrow_compute_pixel_scale (Sheet const *sheet, gboolean horizontal)
{
	return sheet->last_zoom_factor_used *
		gnm_app_display_dpi_get (horizontal) / 72.;
}

void
colrow_compute_pixels_from_pts (ColRowInfo *cri, Sheet const *sheet,
				gboolean horizontal, double scale)
{
	int const margin = horizontal ? 2*GNM_COL_MARGIN : 2*GNM_ROW_MARGIN;

	if (scale == -1)
		scale = colrow_compute_pixel_scale (sheet, horizontal);

	if (horizontal && sheet->display_formulas)
		scale *= 2;

	cri->size_pixels = (int)(cri->size_pts * scale + 0.5);

	if (cri->size_pixels <= margin)
		cri->size_pixels = margin + 1;
}

void
colrow_compute_pts_from_pixels (ColRowInfo *cri, Sheet const *sheet,
				gboolean horizontal, double scale)
{
	if (scale <= 0.)
		scale = colrow_compute_pixel_scale (sheet, horizontal);

	if (horizontal && sheet->display_formulas)
		scale *= 2;

	cri->size_pts = cri->size_pixels / scale;
#if 0
	/* Disable this until we decide how to deal with scaling */
	g_return_if_fail (cri->size_pts >= cri->margin_a + cri->margin_b);
#endif
}

gboolean
colrow_is_default (ColRowInfo const *cri)
{
	g_return_val_if_fail (cri != NULL, FALSE);
	return cri->is_default;
}

/**
 * colrow_is_empty :
 * @cri: #ColRowInfo
 *
 * TRUE if there is no information in col/row @cri.
 **/
gboolean
colrow_is_empty (ColRowInfo const *cri)
{
	if (cri == NULL)
		return TRUE;
	return   cri->is_default &&
		 cri->outline_level == 0 &&
		!cri->is_collapsed &&
		!cri->hard_size;
}

/**
 * colrow_equal :
 * @a: ColRowInfo #1
 * @b: ColRowInfo #2
 *
 * Returns true if the infos are equivalent.
 **/
gboolean
colrow_equal (ColRowInfo const *a, ColRowInfo const *b)
{
	if (a == NULL)
		return b == NULL;
	if (b == NULL)
		return FALSE;

	return  fabs (a->size_pts - b->size_pts) < 1e-5 &&
		a->outline_level == b->outline_level &&
		a->is_collapsed	 == b->is_collapsed &&
		a->hard_size	 == b->hard_size &&
		a->visible	 == b->visible;
}

/**
 * colrow_copy :
 * @dst:
 * @src:
 *
 * Assign all content, except the position of @src to @dst
 */
void
colrow_copy (ColRowInfo *dst, ColRowInfo const *src)
{
	dst->size_pts      = src->size_pts;
	dst->size_pixels   = src->size_pixels;
	dst->outline_level = src->outline_level;
	dst->is_collapsed  = src->is_collapsed;
	dst->hard_size     = src->hard_size;
	dst->visible       = src->visible;
}

ColRowInfo *
col_row_info_new (void)
{
	return g_slice_new (ColRowInfo);
}

void
colrow_free (ColRowInfo *cri)
{
	g_slice_free1 (sizeof (*cri), cri);
}

/**
 * colrow_foreach:
 * @infos:	The Row or Column collection.
 * @first:	start position (inclusive)
 * @last:	stop column (inclusive)
 * @callback: (scope call): A callback function which should return TRUE to stop
 *              the iteration.
 * @user_data:	A bagage pointer.
 *
 * Iterates through the existing rows or columns within the range supplied.
 * Currently only support left -> right iteration.  If a callback returns
 * TRUE iteration stops.
 **/
gboolean
colrow_foreach (ColRowCollection const *infos, int first, int last,
		ColRowHandler callback, gpointer user_data)
{
	GnmColRowIter iter;
	ColRowSegment const *segment;
	int sub, inner_last, i;

	/* TODO : Do we need to support right -> left as an option */

	/* clip */
	if (last > infos->max_used)
		last = infos->max_used;

	for (i = first; i <= last ; ) {
		segment = COLROW_GET_SEGMENT (infos, i);
		sub = COLROW_SUB_INDEX(i);
		inner_last = (COLROW_SEGMENT_INDEX (last) == COLROW_SEGMENT_INDEX (i))
			? COLROW_SUB_INDEX (last)+1 : COLROW_SEGMENT_SIZE;
		iter.pos = i;
		i += COLROW_SEGMENT_SIZE - sub;
		if (segment == NULL)
			continue;

		for (; sub < inner_last; sub++, iter.pos++) {
			iter.cri = segment->info[sub];
			if (iter.cri != NULL && (*callback)(&iter, user_data))
				return TRUE;
		}
	}
	return FALSE;
}

/*****************************************************************************/

typedef struct _ColRowIndex {
	int first, last;
} ColRowIndex;


static void
cb_colrow_index_counter (gpointer data, gpointer user_data)
{
	ColRowIndex *index = data;
	gint *count = user_data;
	if (data != NULL)
		*count += index->last - index->first + 1;
}

gint
colrow_vis_list_length (ColRowVisList *list)
{
	gint count = 0;
	g_slist_foreach (list, cb_colrow_index_counter, &count);
	return count;
}

/**
 * colrow_state_group_destroy:
 * @set: (transfer full): the group to destroy.
 *
 * Returns: (transfer none): %NULL.
 **/
ColRowStateGroup *
colrow_state_group_destroy (ColRowStateGroup *group)
{
	ColRowStateGroup *ptr;
	for (ptr = group; ptr != NULL ; ptr = ptr->next)
		colrow_state_list_destroy (ptr->data);
	g_slist_free (group);
	return NULL;
}

static gint
colrow_index_compare (ColRowIndex const * a, ColRowIndex const * b)
{
	return a->first - b->first;
}

/*
 * colrow_index_list_to_string: Convert an index list into a string.
 *                              The result must be freed by the caller.
 *                              It will be something like : A-B, F-G
 *
 * @list: The list
 * @is_cols: Column index list or row index list?
 * @is_single: If non-null this will be set to TRUE if there's only a single col/row involved.
 */
GString *
colrow_index_list_to_string (ColRowIndexList *list, gboolean is_cols, gboolean *is_single)
{
	ColRowIndexList *ptr;
	GString *result;
	gboolean single = TRUE;

	g_return_val_if_fail (list != NULL, NULL);

	result = g_string_new (NULL);
	for (ptr = list; ptr != NULL; ptr = ptr->next) {
		ColRowIndex *index = ptr->data;

		if (is_cols)
			g_string_append (result, cols_name (index->first, index->last));
		else
			g_string_append (result, rows_name (index->first, index->last));

		if (index->last != index->first)
			single = FALSE;

		if (ptr->next) {
			g_string_append (result, ", ");
			single = FALSE;
		}
	}

	if (is_single)
		*is_single = single;

	return result;
}

/**
 * colrow_get_index_list:
 * @first:
 * @last:
 * @list: (transfer full):
 *
 * Build an ordered list of pairs doing intelligent merging
 * of overlapping regions.
 *
 * Returns: (transfer full): @list.
 */
ColRowIndexList *
colrow_get_index_list (int first, int last, ColRowIndexList *list)
{
	ColRowIndex *tmp, *prev;
	GList *ptr;

	tmp = g_new (ColRowIndex, 1);
	tmp->first = first;
	tmp->last = last;

	list = g_list_insert_sorted (list, tmp,
				     (GCompareFunc)&colrow_index_compare);

	prev = list->data;
	for (ptr = list->next ; ptr != NULL ; ) {
		tmp = ptr->data;

		/* at the end of existing segment or contained */
		if (prev->last+1 >= tmp->first) {
			GList *next = ptr->next;
			if (prev->last < tmp->last)
				prev->last = tmp->last;
			list = g_list_remove_link (list, ptr);
			ptr = next;
		} else {
			ptr = ptr->next;
			prev = tmp;
		}
	}
	return list;
}

/**
 * colrow_index_list_copy:
 * @list: #ColRowIndexList
 *
 * Returns: (transfer full):
 **/
ColRowIndexList *
colrow_index_list_copy (ColRowIndexList *list)
{
	GList *copy = NULL, *ptr;

	for (ptr = list ; ptr != NULL ; ptr = ptr->next) {
		ColRowIndex *tmp = g_new (ColRowIndex, 1);
		ColRowIndex *ex = ptr->data;
		tmp->first = ex->first;
		tmp->last = ex->last;
		copy = g_list_prepend (copy, tmp);
	}
	return g_list_reverse (copy);
}

static void
colrow_set_single_state (ColRowState *state,
			 Sheet *sheet, int i, gboolean is_cols)
{
	ColRowInfo const *info = sheet_colrow_get_info (sheet, i, is_cols);
	state->is_default = colrow_is_default (info);
	state->size_pts	= info->size_pts;
	state->outline_level = info->outline_level;
	state->is_collapsed = info->is_collapsed;
	state->hard_size = info->hard_size;
	state->visible = info->visible;
}

/**
 * colrow_state_list_destroy:
 * @list: (transfer full): the list to destroy.
 *
 * Returns: (transfer none): %NULL.
 **/
ColRowStateList *
colrow_state_list_destroy (ColRowStateList *list)
{
	g_slist_free_full (list, g_free);
	return NULL;
}

/**
 * colrow_get_states:
 * @sheet: #Sheet
 * @is_cols: %TRUE if columns.
 * @first:
 * @last:
 *
 * Returns: (transfer full):
 **/
ColRowStateList *
colrow_get_states (Sheet *sheet, gboolean is_cols, int first, int last)
{
	ColRowStateList *list = NULL;
	ColRowRLEState  *rles;
	ColRowState	 run_state;
	int              i, run_length;

	g_return_val_if_fail (IS_SHEET (sheet), NULL);
	g_return_val_if_fail (first <= last, NULL);

	colrow_set_single_state (&run_state, sheet, first, is_cols);
	run_length = 1;

	for (i = first + 1; i <= last; ++i) {
		ColRowState cur_state;
		colrow_set_single_state (&cur_state, sheet, i, is_cols);

		/* If state changed, start a new block */
		if (cur_state.is_default    != run_state.is_default ||
		    cur_state.size_pts	    != run_state.size_pts ||
		    cur_state.outline_level != run_state.outline_level ||
		    cur_state.is_collapsed  != run_state.is_collapsed ||
		    cur_state.hard_size	    != run_state.hard_size ||
		    cur_state.visible	    != run_state.visible) {
			rles = g_new (ColRowRLEState, 1);
			rles->length = run_length;
			rles->state  = run_state;
			list = g_slist_prepend (list, rles);

			run_state = cur_state;
			run_length = 1;
		} else
			++run_length;
	}

	/* Store the final run */
	rles = g_new (ColRowRLEState, 1);
	rles->length = run_length;
	rles->state = run_state;
	list = g_slist_prepend (list, rles);

	return g_slist_reverse (list);
}

struct resize_closure {
	Sheet *sheet;
	int	 new_size;
	gboolean is_cols;
};

static gboolean
cb_set_colrow_size (GnmColRowIter const *iter, gpointer userdata)
{
	if (iter->cri->visible) {
		struct resize_closure const *c = userdata;

		if (c->is_cols)
			sheet_col_set_size_pixels (c->sheet, iter->pos,
						   c->new_size, TRUE);
		else
			sheet_row_set_size_pixels (c->sheet, iter->pos,
						   c->new_size, TRUE);
	}
	return FALSE;
}

static GnmValue *
cb_clear_variable_width_content (GnmCellIter const *iter,
				 G_GNUC_UNUSED gpointer user)
{
	GnmRenderedValue *rv = gnm_cell_get_rendered_value (iter->cell);
	if (rv && rv->variable_width) {
		iter->ri->needs_respan = TRUE;
		gnm_cell_unrender (iter->cell);
	}
	return NULL;
}

/**
 * colrow_get_sizes:
 * @sheet: #Sheet
 * @is_cols: %TRUE if columns.
 * @src:
 * @new_size:
 *
 * Returns: (transfer full):
 **/
ColRowStateGroup *
colrow_get_sizes (Sheet *sheet, gboolean is_cols,
		  ColRowIndexList *src, int new_size)
{
	ColRowStateGroup *res = NULL;
	ColRowIndexList *ptr;

	for (ptr = src; ptr != NULL ; ptr = ptr->next) {
		ColRowIndex const *index = ptr->data;
		res = g_slist_prepend (res, colrow_get_states (sheet, is_cols,
			index->first, index->last));

		if (new_size > 0 && index->first == 0 &&
		    (index->last+1) >= colrow_max (is_cols, sheet)) {
			ColRowRLEState *rles = g_new0 (ColRowRLEState, 1);

			rles->length = -1; /* Flag as changing the default */

			if (is_cols)
				rles->state.size_pts = sheet_col_get_default_size_pts (sheet);
			else
				rles->state.size_pts = sheet_row_get_default_size_pts (sheet);

			/* Result is a magic 'default' record + >= 1 normal */
			return g_slist_prepend (res, g_slist_append (NULL, rles));
		}
	}

	return res;
}

/**
 * colrow_set_sizes:
 * @sheet: #Sheet
 * @is_cols:
 * @src:
 * @new_size:
 * @from:
 * @to:
 *
 * Returns: (transfer full):
 **/
ColRowStateGroup *
colrow_set_sizes (Sheet *sheet, gboolean is_cols,
		  ColRowIndexList *src, int new_size, int from, int to)
/* from & to are used to restrict fitting to that range. Pass 0, -1 if you want to use the */
/* whole row/column */
{
	int i;
	ColRowStateGroup *res = NULL;
	ColRowIndexList *ptr;

	for (ptr = src; ptr != NULL ; ptr = ptr->next) {
		ColRowIndex const *index = ptr->data;
		res = g_slist_prepend (res, colrow_get_states (sheet, is_cols,
			index->first, index->last));

		/* FIXME :
		 * If we are changing the size of more than half of the rows/col to
		 * something specific (not autosize) we should change the default
		 * row/col size instead.  However, it is unclear how to handle
		 * hard sizing.
		 *
		 * we need better management of rows/cols.  Currently if they are all
		 * defined calculation speed grinds to a halt.
		 */
		if (new_size > 0 && index->first == 0 &&
		    (index->last+1) >= colrow_max (is_cols, sheet)) {
			struct resize_closure closure;
			ColRowRLEState *rles = g_new0 (ColRowRLEState, 1);

			rles->length = -1; /* Flag as changing the default */

			closure.sheet	 = sheet;
			closure.new_size = new_size;
			closure.is_cols  = is_cols;
			if (is_cols) {
				rles->state.size_pts = sheet_col_get_default_size_pts (sheet);
				sheet_col_set_default_size_pixels (sheet, new_size);
				colrow_foreach (&sheet->cols, 0, gnm_sheet_get_last_col (sheet),
					&cb_set_colrow_size, &closure);
			} else {
				rles->state.size_pts = sheet_row_get_default_size_pts (sheet);
				sheet_row_set_default_size_pixels (sheet, new_size);
				colrow_foreach (&sheet->rows, 0, gnm_sheet_get_last_row (sheet),
					&cb_set_colrow_size, &closure);
			}

			/* force a re-render of cells with expanding formats */
			if (is_cols)
				sheet_foreach_cell_in_range (sheet, CELL_ITER_IGNORE_BLANK,
					0, 0, gnm_sheet_get_last_col (sheet), gnm_sheet_get_last_row (sheet),
					(CellIterFunc) &cb_clear_variable_width_content, NULL);

			/* Result is a magic 'default' record + >= 1 normal */
			return g_slist_prepend (res, g_slist_append (NULL, rles));
		}

		if (is_cols) {
			/* force a re-render of cells with expanding formats */
			sheet_foreach_cell_in_range (sheet, CELL_ITER_IGNORE_BLANK,
				index->first, 0, index->last, gnm_sheet_get_last_row (sheet),
				(CellIterFunc) &cb_clear_variable_width_content, NULL);

			/* In order to properly reposition cell comments in
			 * merged cells that cross the boundary we need to do
			 * everything.  Remove this when comments are handled
			 * properly */
			sheet->priv->reposition_objects.col = 0;
		}

		for (i = index->first ; i <= index->last ; ++i) {
			int tmp = new_size;
			if (tmp < 0) {
				int max = is_cols ? gnm_sheet_get_last_row (sheet)
					: gnm_sheet_get_last_col (sheet);
				if (from < 0)
					from = 0;
				if (to < 0 || to > max)
					to = max;
				if (from > max)
					from = to;
				/* Fall back to assigning the default if it is empty */
				tmp = (is_cols)
					? sheet_col_size_fit_pixels (sheet, i, from, to, FALSE)
					: sheet_row_size_fit_pixels (sheet, i, from, to, FALSE);
			}
			if (tmp > 0) {
				if (is_cols)
					sheet_col_set_size_pixels (sheet, i, tmp, new_size > 0);
				else
					sheet_row_set_size_pixels (sheet, i, tmp, new_size > 0);
			} else if (sheet_colrow_get (sheet, i, is_cols) != NULL) {
				if (is_cols)
					sheet_col_set_size_pixels (sheet, i,
						sheet_col_get_default_size_pixels (sheet), FALSE);
				else
					sheet_row_set_size_pixels (sheet, i,
						sheet_row_get_default_size_pixels (sheet), FALSE);
			}
		}
	}

	return res;
}

/*
 * NOTE : this is a low level routine it does not redraw or
 *        reposition objects
 *
 * NOTE : this does not delete states any longer since it may be used
 *        for several sheets.
 */

void
colrow_set_states (Sheet *sheet, gboolean is_cols,
		   int first, ColRowStateList *states)
{
	GSList *l;
	int i, max_outline, offset = first;
	ColRowCollection *infos;
	double scale;

	g_return_if_fail (IS_SHEET (sheet));

	infos = is_cols ? &(sheet->cols) : &(sheet->rows);
	max_outline = infos->max_outline_level;
	scale = colrow_compute_pixel_scale (sheet, is_cols);

	for (l = states; l != NULL; l = l->next) {
		ColRowRLEState const *rles = l->data;
		ColRowState const *state = &rles->state;

		if (max_outline < state->outline_level)
			max_outline = state->outline_level;

		for (i = offset; i < offset + rles->length; i++) {
			if (state->is_default) {
				ColRowSegment *segment = COLROW_GET_SEGMENT(infos, i);
				if (segment != NULL) {
					int const sub = COLROW_SUB_INDEX (i);
					ColRowInfo *cri = segment->info[sub];
					if (cri != NULL) {
						segment->info[sub] = NULL;
						colrow_free (cri);
					}
				}
			} else {
				ColRowInfo *cri = sheet_colrow_fetch (sheet, i, is_cols);
				cri->hard_size = state->hard_size;
				cri->size_pts = state->size_pts;
				colrow_compute_pixels_from_pts (cri, sheet, is_cols, scale);
				colrow_set_outline (cri, state->outline_level,
					state->is_collapsed);
			}
		}
		offset += rles->length;
	}

	/* Notify sheet of pending update */
	sheet->priv->recompute_visibility = TRUE;
	if (is_cols) {
		sheet_flag_recompute_spans (sheet);

		/* In order to properly reposition cell
		 * comments in merged cells that cross the
		 * boundary we need to do everything.  Revert
		 * this when comments are handled properly */
#if 0
		if (sheet->priv->reposition_objects.col > first)
			sheet->priv->reposition_objects.col = first;
#else
		sheet->priv->reposition_objects.col = 0;
#endif
	} else {
		if (sheet->priv->reposition_objects.row > first)
			sheet->priv->reposition_objects.row = first;
	}
	sheet_colrow_gutter (sheet, is_cols, max_outline);
}

void
colrow_restore_state_group (Sheet *sheet, gboolean is_cols,
			    ColRowIndexList *selection,
			    ColRowStateGroup *state_groups)
{
	ColRowStateGroup *ptr = state_groups;

	/* Cycle to end, we have to traverse the selections
	 * in parallel with the state_groups
	 */
	selection = g_list_last (selection);
	for (; selection != NULL && ptr != NULL ; ptr = ptr->next) {
		ColRowIndex const *index = selection->data;
		ColRowStateList *list = ptr->data;
		ColRowRLEState const *rles = list->data;

		/* MAGIC : the -1 was set above to flag this */
		if (rles->length == -1) {
			if (is_cols)
				sheet_col_set_default_size_pts (sheet, rles->state.size_pts);
			else
				sheet_row_set_default_size_pts (sheet, rles->state.size_pts);

			/* we are guaranteed to have at least 1 more record */
			ptr = ptr->next;
		}

		colrow_set_states (sheet, is_cols, index->first, ptr->data);
		/* force a re-render of cells with expanding formats */
		if (is_cols)
			sheet_foreach_cell_in_range (sheet, CELL_ITER_IGNORE_BLANK,
				index->first, 0, index->last, gnm_sheet_get_last_row (sheet),
				(CellIterFunc) &cb_clear_variable_width_content, NULL);
		selection = selection->prev;
	}
}

/**
 * rows_height_update:
 * @sheet:  The sheet,
 * @range:  The range whose rows should be resized.
 * @shrink: If set to FALSE, rows will never shrink!
 *
 * Use this function having changed the font size to auto
 * resize the row heights to make the text fit nicely.
 **/
void
rows_height_update (Sheet *sheet, GnmRange const * range, gboolean shrink)
{
	/* FIXME : this needs to check font sizes and contents rather than
	 * just contents.  Empty cells will cause resize also */
	colrow_autofit (sheet, range, FALSE, FALSE,
			FALSE, !shrink,
			NULL, NULL);
}

/* ------------------------------------------------------------------------- */

struct cb_autofit {
	Sheet *sheet;
	const GnmRange *range;
	gboolean ignore_strings;
	gboolean min_current;
	gboolean min_default;
};

static gboolean
cb_autofit_col (GnmColRowIter const *iter, gpointer data_)
{
	struct cb_autofit *data = data_;
	int size, min, max;

	if (iter->cri->hard_size)
		return FALSE;

	size = sheet_col_size_fit_pixels (data->sheet, iter->pos,
		 data->range->start.row, data->range->end.row,
		 data->ignore_strings);
	/* FIXME: better idea than this?  */
	max = 50 * sheet_col_get_default_size_pixels (data->sheet);
	size = MIN (size, max);

	min = 0;
	if (data->min_current)
		min = MAX (min, iter->cri->size_pixels);
	if (data->min_default)
		min = MAX (min, sheet_col_get_default_size_pixels (data->sheet));

	if (size > min)
		sheet_col_set_size_pixels (data->sheet, iter->pos, size, FALSE);

	return FALSE;
}

static gboolean
cb_autofit_row (GnmColRowIter const *iter, gpointer data_)
{
	struct cb_autofit *data = data_;
	int size, min, max;

	if (iter->cri->hard_size)
		return FALSE;

	size = sheet_row_size_fit_pixels (data->sheet, iter->pos,
		 data->range->start.col, data->range->end.col,
		 data->ignore_strings);
	max = 20 * sheet_row_get_default_size_pixels (data->sheet);
	size = MIN (size, max);

	min = 0;
	if (data->min_current)
		min = MAX (min, iter->cri->size_pixels);
	if (data->min_default)
		min = MAX (min, sheet_row_get_default_size_pixels (data->sheet));

	if (size > min)
		sheet_row_set_size_pixels (data->sheet, iter->pos, size, FALSE);

	return FALSE;
}

/*
 * colrow_autofit:
 * @sheet: the sheet to change
 * @range: the range to consider
 * @is_cols: TRUE for columns, FALSE for rows.
 * @ignore_strings: Don't consider cells with string values.
 * @min_current: Don't shrink below current size.
 * @min_default: Don't shrink below default size.
 * @indices: indices appropriate for colrow_restore_state_group.
 * @sizes: old sizes appropriate for colrow_restore_state_group.
 *
 * This function autofits columns or rows in @range as specified by
 * @is_cols.  Only cells in @range are considered for the sizing
 * and the size can be bounded below by current size and/or default
 * size.
 */
void
colrow_autofit (Sheet *sheet, const GnmRange *range, gboolean is_cols,
		gboolean ignore_strings,
		gboolean min_current, gboolean min_default,
		ColRowIndexList **indices,
		ColRowStateGroup **sizes)
{
	struct cb_autofit data;
	int a, b;
	ColRowCollection *crs;
	ColRowHandler handler;

	data.sheet = sheet;
	data.range = range;
	data.ignore_strings = ignore_strings;
	data.min_current = min_current;
	data.min_default = min_default;

	if (is_cols) {
		a = range->start.col;
		b = range->end.col;
		crs = &sheet->cols;
		handler = cb_autofit_col;
	} else {
		a = range->start.row;
		b = range->end.row;
		crs = &sheet->rows;
		handler = cb_autofit_row;
	}

	if (indices)
		*indices = colrow_get_index_list (a, b, NULL);
	if (sizes)
		*sizes = g_slist_prepend (NULL, colrow_get_states (sheet, is_cols, a, b));

	/* We potentially do a lot of recalcs as part of this, so make sure
	   stuff that caches sub-computations see the whole thing instead
	   of clearing between cells.  */
	gnm_app_recalc_start ();
	colrow_foreach (crs, a, b, handler, &data);
	gnm_app_recalc_finish ();
}

void
colrow_autofit_col (Sheet *sheet, GnmRange *r)
{
	colrow_autofit (sheet, r, TRUE, TRUE,
			TRUE, FALSE, NULL, NULL);
	sheet_foreach_cell_in_range (sheet, CELL_ITER_IGNORE_BLANK,
				     r->start.col, 0,
				     r->end.col, gnm_sheet_get_last_row (sheet),
				     (CellIterFunc) &cb_clear_variable_width_content,
				     NULL);
}

void
colrow_autofit_row (Sheet *sheet, GnmRange *r)
{
	colrow_autofit (sheet, r, FALSE, FALSE,
			TRUE, FALSE, NULL, NULL);
}

/*****************************************************************************/

typedef struct
{
	gboolean is_cols, visible;
	ColRowVisList *elements;
} ColRowVisiblity;

static gint
colrow_index_cmp (ColRowIndex const *a, ColRowIndex const *b)
{
	/* We can be very simplistic here because the ranges never overlap */
	return b->first - a->first;
}

static void
colrow_visibility (Sheet const *sheet, ColRowVisiblity * const dat,
		   int first, int last)
{
	int i;
	gboolean const visible = dat->visible;
	ColRowInfo * (*get) (Sheet const *sheet, int pos) = (dat->is_cols)
		? &sheet_col_get : &sheet_row_get;

	/* Find the end of a segment that will be toggled */
	for (i = last; i >= first; --i) {
		int j;
		ColRowIndex *res;
		ColRowInfo const *cri = (*get) (sheet, i);

		if (cri == NULL) {
			if (visible != 0)
				continue;
		} else if ((visible != 0) == (cri->visible != 0))
			continue;

		/* Find the begining */
		for (j = i; j >= first ; --j) {
			cri = (*get) (sheet, j);
			if (cri == NULL) {
				if (visible != 0)
					break;
			} else if ((visible != 0) == (cri->visible != 0))
				break;
			else if (cri->is_collapsed) {
				--j;
				break;
			}
		}
		res = g_new (ColRowIndex, 1);
		res->first = (j >= first) ? j+1 : first;
		res->last = i;
#if 0
		g_printerr ("%d %d\n", res->index, res->count);
#endif
		dat->elements = g_slist_insert_sorted (dat->elements, res,
					(GCompareFunc)colrow_index_cmp);

		if (visible && cri != NULL && cri->is_collapsed) {
			i = colrow_find_outline_bound (
				sheet, dat->is_cols, j,
				cri->outline_level+1, FALSE);
		} else
			i = j;
	}
}

/**
 * colrow_get_outline_toggle:
 * @sheet: #Sheet
 * @is_cols:
 * @visible:
 * @first:
 * @last:
 *
 * Returns: (transfer full):
 **/
ColRowVisList *
colrow_get_outline_toggle (Sheet const *sheet, gboolean is_cols, gboolean visible,
			   int first, int last)
{
	ColRowVisiblity closure;
	closure.is_cols = is_cols;
	closure.visible = visible;
	closure.elements = NULL;

	colrow_visibility (sheet, &closure, first, last);
	return closure.elements;
}

static void
cb_colrow_visibility (SheetView *sv, GnmRange const *r, gpointer closure)
{
	ColRowVisiblity * const dat = (ColRowVisiblity *)closure;
	int first, last;

	if (dat->is_cols) {
		first = r->start.col;
		last = r->end.col;
	} else {
		first = r->start.row;
		last = r->end.row;
	}
	colrow_visibility (sv_sheet (sv), dat, first, last);
}

/**
 * colrow_get_visiblity_toggle:
 * @sv: The sheet view whose selection we are interested in.
 * @is_cols: A flag indicating whether this it is a column or a row.
 * @visible: Should we unhide or hide the cols/rows.
 *
 * Searches the selection list and generates a list of index,count
 * pairs of row/col ranges that need to be hidden or unhiden.
 *
 * NOTE : leave sheet non-const until we have a const version of
 *        sv_selection_apply.
 *
 * Returns: (transfer full): the list.
 */
ColRowVisList *
colrow_get_visiblity_toggle (SheetView *sv, gboolean is_cols,
			     gboolean visible)
{
	ColRowVisiblity closure;
	closure.is_cols = is_cols;
	closure.visible = visible;
	closure.elements = NULL;

	sv_selection_apply (sv, &cb_colrow_visibility, FALSE, &closure);

	return closure.elements;
}

/*
 * colrow_set_visibility_list :
 *
 * This is the high level command that is wrapped by undo and redo.
 * It should not be called by other commands.
 */
void
colrow_set_visibility_list (Sheet *sheet, gboolean is_cols,
			    gboolean visible, ColRowVisList *list)
{
	ColRowVisList *ptr;
	ColRowIndex *info;

	for (ptr = list; ptr != NULL ; ptr = ptr->next) {
		info = ptr->data;
		colrow_set_visibility (sheet, is_cols, visible,
				       info->first, info->last);
	}

	if (visible)
		sheet_colrow_optimize (sheet);

	if (is_cols)
		sheet_queue_respan (sheet, 0, gnm_sheet_get_last_row (sheet));
	if (list != NULL)
		sheet_redraw_all (sheet, TRUE);
}

/**
 * colrow_set_outline :
 * @cri: the col/row to tweak
 * @outline_level:
 * @is_collapsed:
 *
 * Adjust the outline state of a col/row
 */
void
colrow_set_outline (ColRowInfo *cri, int outline_level, gboolean is_collapsed)
{
	g_return_if_fail (outline_level >= 0);

	cri->is_collapsed = (is_collapsed != 0);  /* be anal */
	cri->outline_level = outline_level;
}

/**
 * colrow_find_outline_bound :
 *
 * find the next/prev col/row at the designated depth starting from the
 * supplied @index.
 */
int
colrow_find_outline_bound (Sheet const *sheet, gboolean is_cols,
			   int index, int depth, gboolean inc)
{
	ColRowInfo * (*get) (Sheet const *sheet, int pos) = is_cols
		? &sheet_col_get : &sheet_row_get;
	int const max = colrow_max (is_cols, sheet);
	int const step = inc ? 1 : -1;

	while (1) {
		ColRowInfo const *cri;
		int const next = index + step;

		if (next < 0 || next >= max)
			return index;
		cri = (*get) (sheet, next);
		if (cri == NULL || cri->outline_level < depth)
			return index;
		index = next;
	}

	return index;
}

/**
 * colrow_set_visibility:
 * @sheet: the sheet
 * @is_cols: Are we dealing with rows or columns.
 * @visible: Make things visible or invisible.
 * @first: The index of the first row/col (inclusive)
 * @last: The index of the last row/col (inclusive)
 *
 * Change the visibility of the selected range of contiguous cols/rows.
 * NOTE : only changes the collapsed state for the LAST+1 element.
 */
void
colrow_set_visibility (Sheet *sheet, gboolean is_cols,
		       gboolean visible, int first, int last)
{
	int i, step, prev_outline   = 0;
	gboolean changed = FALSE;
	GnmRange * const bound   = &sheet->priv->unhidden_region;
	gboolean const fwd = is_cols ? sheet->outline_symbols_right : sheet->outline_symbols_below;

	g_return_if_fail (IS_SHEET (sheet));
	g_return_if_fail (first <= last);

	if (visible) { /* expand to include newly visible regions */
		if (is_cols) {
			if (bound->start.col > first)
				bound->start.col = first;
			if (bound->end.col < last)
				bound->end.col = last;
		} else {
			if (bound->start.row > first)
				bound->start.row = first;
			if (bound->end.row < last)
				bound->end.row = last;
		}
	} else { /* contract to exclude newly hidden regions */
		if (is_cols) {
			if (bound->start.col >= first && bound->start.col <= last)
				bound->start.col = last+1;
			if (bound->end.col <= last && bound->end.col >= first)
				bound->end.col = first-1;
		} else {
			if (bound->start.row >= first && bound->start.row <= last)
				bound->start.row = last+1;
			if (bound->end.row <= last && bound->end.row >= first)
				bound->end.row = first-1;
		}
	}

	if (fwd) {
		i = first;
		step = 1;
	} else {
		i = last;
		step = -1;
	}

	for (; fwd ? (i <= last) : (i >= first) ; i += step) {
		ColRowInfo * const cri = sheet_colrow_fetch (sheet, i, is_cols);

		if (changed && prev_outline > cri->outline_level && !visible)
			cri->is_collapsed = FALSE;

		changed = (visible == 0) != (cri->visible == 0);
		if (changed) {
			cri->visible = visible;
			prev_outline = cri->outline_level;
			sheet->priv->recompute_visibility = TRUE;

			if (is_cols) {
				sheet_flag_recompute_spans (sheet);

				/* In order to properly reposition cell
				 * comments in merged cells that cross the
				 * boundary we need to do everything.  Revert
				 * this when comments are handled properly */
#if 0
				if (sheet->priv->reposition_objects.col > i)
					sheet->priv->reposition_objects.col = i;
#else
				sheet->priv->reposition_objects.col = 0;
#endif
			} else {
				if (sheet->priv->reposition_objects.row > i)
					sheet->priv->reposition_objects.row = i;
			}
		}
	}

	if (changed && 0 <= i && i < colrow_max (is_cols, sheet)) {
		ColRowInfo * const cri = sheet_colrow_fetch (sheet, i, is_cols);
		if (prev_outline > cri->outline_level)
			cri->is_collapsed = !visible;
	}
}

/**
 * colrow_get_global_outline :
 * @sheet:
 * @is_cols:
 * @depth:
 * @show:
 * @hide:
 *
 * Collect the set of visiblity changes required to change the visiblity of
 * all outlined columns such tach those > @depth are visible.
 **/
void
colrow_get_global_outline (Sheet const *sheet, gboolean is_cols, int depth,
			   ColRowVisList **show, ColRowVisList **hide)
{
	ColRowInfo const *cri;
	ColRowIndex *prev = NULL;
	gboolean show_prev = FALSE;
	unsigned tmp, prev_outline = 0;
	int i, max = is_cols ? sheet->cols.max_used : sheet->rows.max_used;

	*show = *hide = NULL;
	for (i = 0; i <= max ; i++) {
		cri = sheet_colrow_get (sheet, i, is_cols);

		if (cri == NULL || cri->outline_level == 0) {
			prev_outline = 0;
			continue;
		}
		tmp = prev_outline;
		prev_outline = cri->outline_level;

		/* see what sort of changes are necessary and do simple run
		 * length encoding.  Do not be too efficent, we need to change
		 * the visiblity per outline level or the collapse state
		 * change in colrow_set_visibility is missed. */
		if (cri->outline_level < depth) {
			if (cri->visible)
				continue;
			if (show_prev && prev != NULL && prev->last == (i-1) &&
			    tmp == prev_outline) {
				prev->last = i;
				continue;
			}
			prev = g_new (ColRowIndex, 1);
			prev->first = prev->last = i;
			*show = g_slist_prepend (*show, prev);
			show_prev = TRUE;
		} else {
			if (!cri->visible)
				continue;
			if (!show_prev && prev != NULL && prev->last == (i-1) &&
			    tmp == prev_outline) {
				prev->last = i;
				continue;
			}
			prev = g_new (ColRowIndex, 1);
			prev->first = prev->last = i;
			*hide = g_slist_prepend (*hide, prev);
			show_prev = FALSE;
		}
	}

	*show = g_slist_reverse (*show);
	*hide = g_slist_reverse (*hide);
}

void
colrow_resize (ColRowCollection *infos, int size)
{
	int end_idx = COLROW_SEGMENT_INDEX (size);
	int i = infos->info->len - 1;

	while (i >= end_idx) {
		ColRowSegment *segment = g_ptr_array_index (infos->info, i);
		if (segment) {
			g_free (segment);
			g_ptr_array_index (infos->info, i) = NULL;
		}
		i--;
	}

	g_ptr_array_set_size (infos->info, end_idx);
}
