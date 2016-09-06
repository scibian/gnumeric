/* A Bison parser, made by GNU Bison 3.0.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2013 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.0.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */
#line 1 "parser.y" /* yacc.c:339  */

/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * Gnumeric Parser
 *
 * (C) 1998-2002 GNOME Foundation
 * Copyright (C) 2002-2009 Morten Welinder
 *
 * Authors:
 *    Miguel de Icaza (miguel@gnu.org)
 *    Jody Goldberg (jody@gnome.org)
 *    Morten Welinder (terra@diku.dk)
 *    Almer S. Tigelaar (almer@gnome.org)
 */
#include <gnumeric-config.h>
#include <glib/gi18n-lib.h>
#include "gnumeric.h"
#include "number-match.h"
#include "expr.h"
#include "expr-impl.h"
#include "expr-name.h"
#include "func.h"
#include "workbook.h"
#include "sheet.h"
#include "gnm-format.h"
#include "application.h"
#include "parse-util.h"
#include "gutils.h"
#include "style.h"
#include "value.h"
#include <goffice/goffice.h>

#include <string.h>
#include <errno.h>
#include <stdlib.h>

#define YYDEBUG 1

/* ------------------------------------------------------------------------- */
/* Allocation with disposal-on-error */

/*
 * If some dork enters "=1+2+2*(1+" we have already allocated space for
 * "1+2", "2", and "1" before the parser sees the syntax error and warps
 * us to the error production in the "line" non-terminal.
 *
 * To make sure we can clean up, we register every allocation.  On success,
 * nothing should be left (except the final expression which is unregistered),
 * but on failure we must free everything allocated.
 *
 * Note: there is some room left for optimisation here.  Talk to terra@diku.dk
 * before you set out to do it.
 */

static void
free_expr_list_list (GSList *list)
{
	GSList *l;
	for (l = list; l; l = l->next)
		gnm_expr_list_unref (l->data);
	g_slist_free (list);
}

typedef void (*ParseDeallocator) (void *);
static GPtrArray *deallocate_stack;

static void
deallocate_init (void)
{
	deallocate_stack = g_ptr_array_new ();
}

static void
deallocate_uninit (void)
{
	g_ptr_array_free (deallocate_stack, TRUE);
	deallocate_stack = NULL;
}

static void
deallocate_all (void)
{
	int i;

	for (i = 0; i < (int)deallocate_stack->len; i += 2) {
		ParseDeallocator freer = g_ptr_array_index (deallocate_stack, i + 1);
		freer (g_ptr_array_index (deallocate_stack, i));
	}

	g_ptr_array_set_size (deallocate_stack, 0);
}

static void
deallocate_assert_empty (void)
{
	if (deallocate_stack->len == 0)
		return;

	g_warning ("deallocate_stack not empty as expected.");
	deallocate_all ();
}

static void *
register_allocation (gpointer data, ParseDeallocator freer)
{
	/* It's handy to be able to register and unregister NULLs.  */
	if (data) {
		int len;
		/*
		 * There are really only a few different freers, so we
		 * could encode the freer in the lower bits of the data
		 * pointer.  Unfortunately, no-one can predict how high
		 * Miguel would jump when he found out.
		 */
		len = deallocate_stack->len;
		g_ptr_array_set_size (deallocate_stack, len + 2);
		g_ptr_array_index (deallocate_stack, len) = data;
		g_ptr_array_index (deallocate_stack, len + 1) = freer;
	}

	/* Returning the pointer here improved readability of the caller.  */
	return data;
}

#define register_expr_allocation(expr) \
  register_allocation ((gpointer)(expr), (ParseDeallocator)&gnm_expr_free)

#define register_expr_list_allocation(list) \
  register_allocation ((list), (ParseDeallocator)&gnm_expr_list_unref)

#define register_expr_list_list_allocation(list) \
  register_allocation ((list), (ParseDeallocator)&free_expr_list_list)

static void
unregister_allocation (void const *data)
{
	int i, pos;

	/* It's handy to be able to register and unregister NULLs.  */
	if (!data)
		return;

	pos = deallocate_stack->len - 2;
	if (pos >= 0 && data == g_ptr_array_index (deallocate_stack, pos)) {
		g_ptr_array_set_size (deallocate_stack, pos);
		return;
	}

	/*
	 * Bummer.  In certain error cases, it is possible that the parser
	 * will reduce after it has discovered a token that will lead to an
	 * error.  "2/16/1800 00:00" (without the quotes) is an example.
	 * The first "00" is registered before the second division is
	 * reduced.
	 *
	 * Another example is 564077 where we deallocate out of order.
	 *
	 * This isn't a big deal -- we will just look at the entries below
	 * the top.
	 */
	for (i = pos - 2; i >= 0; i -= 2) {
		if (data == g_ptr_array_index (deallocate_stack, i)) {
			g_ptr_array_remove_index (deallocate_stack, i);
			g_ptr_array_remove_index (deallocate_stack, i);
			return;
		}
	}

	g_warning ("Unbalanced allocation registration");
}

/* ------------------------------------------------------------------------- */

/* Bison/Yacc internals */
static int yylex (void);
static int yyerror (char const *s);

typedef struct {
	char const *ptr;	/* current position of the lexer */
	char const *start;	/* start of the expression */

	/* Location where the parsing is taking place */
	GnmParsePos const *pos;

	/* loaded from convs with locale specific mappings */
	gunichar decimal_point;
	gunichar arg_sep;
	gunichar array_col_sep;
	gunichar array_row_sep;
	/* if arg_sep conflicts with array_col_sep or array_row_sep */
	int in_array_sep_is;	/* token id */

	GnmExprParseFlags     flags;
	GnmConventions const *convs;

	/* dynamic state */
	int in_array; /* toggled in the lexer for '{' and '}' */
	GnmExprList *result;

	GnmParseError *error;
} ParserState;

/* The error returned from the */
static ParserState *state;

static void
report_err (ParserState *state, GError *err,
	    char const *last, int guesstimate_of_length)
{
	if (state->error != NULL) {
		state->error->err    	 = err;
		state->error->end_char   = last - state->start;
		state->error->begin_char = state->error->end_char - guesstimate_of_length;
		if (state->error->begin_char < 0)
			state->error->begin_char = 0;
	} else
		g_error_free (err);
}

static gboolean
is_signed (const GnmExpr *expr)
{
	if (GNM_EXPR_GET_OPER (expr) == GNM_EXPR_OP_UNARY_NEG)
		return TRUE;

	if (GNM_EXPR_GET_OPER (expr) == GNM_EXPR_OP_UNARY_PLUS)
		return TRUE;

	if (GNM_EXPR_GET_OPER (expr) == GNM_EXPR_OP_CONSTANT) {
		GnmValue const *v = expr->constant.value;
		return VALUE_IS_FLOAT (v) && value_get_as_float (v) < 0;
	}

	return FALSE;
}

/* Handle -cst for use in arrays.  Don't handle other types here.  */
static GnmExpr *
fold_negative_constant (GnmExpr *expr)
{
	if (expr && GNM_EXPR_GET_OPER (expr) == GNM_EXPR_OP_CONSTANT) {
		GnmValue *v = (GnmValue *)expr->constant.value;

		if (VALUE_IS_FLOAT (v)) {
			gnm_float f = value_get_as_float (v);
			expr->constant.value = value_new_float (0 - f);
			value_release (v);
			return expr;
		}
	}

	return NULL;
}

/* Handle +cst for use in arrays.  Don't handle other types here.  */
static GnmExpr *
fold_positive_constant (GnmExpr *expr)
{
	if (expr && GNM_EXPR_GET_OPER (expr) == GNM_EXPR_OP_CONSTANT) {
		const GnmValue *v = expr->constant.value;
		if (VALUE_IS_FLOAT (v))
			return expr;
	}

	return NULL;
}

static GnmExpr *
build_unary_op (GnmExprOp op, GnmExpr *expr)
{
	if (!expr) return NULL;

	unregister_allocation (expr);
	return register_expr_allocation (gnm_expr_new_unary (op, expr));
}

static GnmExpr *
build_binop (GnmExpr *l, GnmExprOp op, GnmExpr *r)
{
	if (!l || !r) return NULL;

	unregister_allocation (r);
	unregister_allocation (l);
	return register_expr_allocation (gnm_expr_new_binary (l, op, r));
}

static GnmExpr *
build_logical (GnmExpr *l, gboolean is_and, GnmExpr *r)
{
	static GnmFunc *and_func = NULL, *or_func = NULL;

	if (!l || !r) return NULL;

	if (and_func == NULL)
		and_func = gnm_func_lookup ("AND", NULL);
	if (or_func == NULL)
		or_func = gnm_func_lookup ("OR", NULL);

	unregister_allocation (r);
	unregister_allocation (l);
	return register_expr_allocation
		(gnm_expr_new_funcall2 (is_and ? and_func : or_func, l, r));
}

static GnmExpr *
build_not (GnmExpr *expr)
{
	static GnmFunc *not_func = NULL;

	if (!expr) return NULL;

	if (not_func == NULL)
		not_func = gnm_func_lookup ("NOT", NULL);
	unregister_allocation (expr);
	return register_expr_allocation
		(gnm_expr_new_funcall1 (not_func, expr));
}

static GnmExpr *
build_exp (GnmExpr *l, GnmExpr *r)
{
	if (is_signed (l)) {
		/* See bug 115941 */
		l = build_unary_op (GNM_EXPR_OP_PAREN, l);
	}

	if (GNM_EXPR_GET_OPER (l) == GNM_EXPR_OP_EXP) {
		/* Add ()s to x^y^z */
		l = build_unary_op (GNM_EXPR_OP_PAREN, l);
	}

	if (GNM_EXPR_GET_OPER (r) == GNM_EXPR_OP_EXP) {
		/* Add ()s to x^y^z */
		r = build_unary_op (GNM_EXPR_OP_PAREN, r);
	}

	return build_binop (l, GNM_EXPR_OP_EXP, r);
}

/*
 * Build an array expression.
 *
 * Returns NULL on failure.  Caller must YYERROR in that case.
 */
static GnmExpr *
build_array (GSList *cols)
{
	GnmValue *array;
	int mx, y;

	if (!cols) {
		report_err (state, g_error_new (1, PERR_INVALID_EMPTY,
			_("An array must have at least 1 element")),
			state->ptr, 0);
		return NULL;
	}

	mx = g_list_length (cols->data);
	array = value_new_array_empty (mx, g_slist_length (cols));

	y = 0;
	while (cols) {
		GSList *row = cols->data;
		int x = 0;
		while (row && x < mx) {
			GnmExpr const *expr = row->data;
			GnmValue const *v = expr->constant.value;

			g_assert (expr && GNM_EXPR_GET_OPER (expr) == GNM_EXPR_OP_CONSTANT);

			value_array_set (array, x, y, value_dup (v));

			x++;
			row = row->next;
		}
		if (x < mx || row) {
			/* parser_error = PARSE_ERR_SYNTAX; */
			report_err (state, g_error_new (1, PERR_ASYMETRIC_ARRAY,
				_("Arrays must be rectangular")),
				state->ptr, 0);
			value_release (array);
			return NULL;
		}
		y++;
		cols = cols->next;
	}

	return register_expr_allocation (gnm_expr_new_constant (array));
}

/*
 * Build a range constructor.
 *
 * Returns NULL on failure.  Caller must YYERROR in that case.
 */
static GnmExpr *
build_range_ctor (GnmExpr *l, GnmExpr *r, GnmExpr *validate)
{
	if (!l || !r) return NULL;

	if (validate != NULL) {
		if (GNM_EXPR_GET_OPER (validate) != GNM_EXPR_OP_CELLREF ||
		    validate->cellref.ref.sheet != NULL) {
			report_err (state, g_error_new (1, PERR_UNEXPECTED_TOKEN,
				_("Constructed ranges use simple references")),
				state->ptr, 0);
			return NULL;
		    }
	}

	unregister_allocation (r);
	unregister_allocation (l);
	return register_expr_allocation (gnm_expr_new_range_ctor (l, r));
}

/*
 * Build an intersection expression.
 *
 * Returns NULL on failure.  Caller must YYERROR in that case.
 */
static GnmExpr *
build_intersect (GnmExpr *l, GnmExpr *r)
{
	if (!l || !r) return NULL;

	if (gnm_expr_is_rangeref (l) && gnm_expr_is_rangeref (r))
		return build_binop (l, GNM_EXPR_OP_INTERSECT, r);
	report_err (state, g_error_new (1, PERR_SET_CONTENT_MUST_BE_RANGE,
		_("All entries in the set must be references")),
		state->ptr, 0);
	return NULL;
}

/*
 * Build a set expression.
 *
 * Returns NULL on failure.  Caller must YYERROR in that case.
 */
static GnmExpr *
build_set (GnmExprList *list)
{
	/* verify that every thing is a ref */
	GnmExprList *ptr;
	for (ptr = list; ptr != NULL ; ptr = ptr->next) {
		GnmExpr const *expr = ptr->data;
		if (!expr || !gnm_expr_is_rangeref (expr)) {
			report_err (state, g_error_new (1, PERR_SET_CONTENT_MUST_BE_RANGE,
				_("All entries in the set must be references")),
				state->ptr, 0);
			return NULL;
		}
	}

	unregister_allocation (list);
	return register_expr_allocation (gnm_expr_new_set (list));
}

/**
 * parse_string_as_value :
 *
 * Try to parse the entered text as a basic value (empty, bool, int,
 * gnm_float, err) if this succeeds, we store this as a GnmValue otherwise, we
 * return a string.
 */
static GnmExpr *
parse_string_as_value (GnmExpr *str)
{
	GnmValue *v = format_match_simple (value_peek_string (str->constant.value));

	if (v != NULL) {
		unregister_allocation (str);
		gnm_expr_free (str);
		return register_expr_allocation (gnm_expr_new_constant (v));
	}
	return str;
}

/**
 * parser_simple_val_or_name :
 * @str : An expression with oper constant, whose value is a string.
 *
 * Check to see if a string is a simple value or failing that a named
 * expression, if it is not create a placeholder name for it.
 */
static GnmExpr *
parser_simple_val_or_name (GnmExpr *str_expr)
{
	GnmExpr const *res;
	char const *str = value_peek_string (str_expr->constant.value);
	GnmValue *v = format_match_simple (str);

	/* if it is not a simple value see if it is a name */
	if (v == NULL) {
		GnmNamedExpr *nexpr = expr_name_lookup (state->pos, str);
		if (nexpr == NULL) {
			if (state->flags & GNM_EXPR_PARSE_UNKNOWN_NAMES_ARE_INVALID) {
				report_err (state, g_error_new (1, PERR_UNKNOWN_NAME,
								_("Name '%s' does not exist"),
								str),
					    state->ptr, 0);
				res = NULL;
			} else if (state->flags & GNM_EXPR_PARSE_UNKNOWN_NAMES_ARE_STRINGS) {
				res = gnm_expr_new_constant (value_new_string (str));
			} else if (state->convs->input.name_validate (str)) {
				GnmParsePos pp = *state->pos;
				pp.sheet = NULL;
				/* Create a place holder */
				nexpr = expr_name_add (&pp, str, NULL, NULL, TRUE, NULL);
				res = gnm_expr_new_name (nexpr, NULL, NULL);
			} else {
				report_err (state, g_error_new (1, PERR_UNKNOWN_NAME,
								_("'%s' cannot be used as a name"),
								str),
					    state->ptr, 0);
				res = NULL;
			}
		} else
			res = gnm_expr_new_name (nexpr, NULL, NULL);
	} else
		res = gnm_expr_new_constant (v);

	unregister_allocation (str_expr);
	gnm_expr_free (str_expr);
	return register_expr_allocation (res);
}

static Sheet *
parser_sheet_by_name (Workbook *wb, GnmExpr *name_expr)
{
	char const *name = value_peek_string (name_expr->constant.value);
	Sheet *sheet = NULL;

	if (wb == NULL)
		return NULL;

	sheet = workbook_sheet_by_name (wb, name);

	/* Applix has absolute and relative sheet references */
	if (sheet == NULL && *name == '$' &&
	    state->convs->allow_absolute_sheet_references)
		sheet = workbook_sheet_by_name (wb, name + 1);

	if (sheet == NULL)
		/* TODO : length is broken in the context of quoted names or
		 * names with escaped character */
		/* -1 is a kludge.  We know that this routine is only called
		 * when the last token was SHEET_SEP */
		report_err (state, g_error_new (1, PERR_UNKNOWN_SHEET,
			_("Unknown sheet '%s'"), name),
			state->ptr-1, strlen (name));

	return sheet;
}

/* Make byacc happier */
static int yyparse (void);


#line 625 "parser.c" /* yacc.c:339  */

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif


/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    STRING = 258,
    QUOTED_STRING = 259,
    CONSTANT = 260,
    RANGEREF = 261,
    tok_GTE = 262,
    tok_LTE = 263,
    tok_NE = 264,
    tok_AND = 265,
    tok_OR = 266,
    tok_NOT = 267,
    INTERSECT = 268,
    ARG_SEP = 269,
    ARRAY_COL_SEP = 270,
    ARRAY_ROW_SEP = 271,
    SHEET_SEP = 272,
    INVALID_TOKEN = 273,
    tok_WORKBOOKREF = 274,
    tok_RIGHT_EXP = 275,
    tok_LEFT_EXP = 276,
    tok_NEG = 277,
    tok_PLUS = 278,
    RANGE_INTERSECT = 279,
    RANGE_SEP = 280
  };
#endif
/* Tokens.  */
#define STRING 258
#define QUOTED_STRING 259
#define CONSTANT 260
#define RANGEREF 261
#define tok_GTE 262
#define tok_LTE 263
#define tok_NE 264
#define tok_AND 265
#define tok_OR 266
#define tok_NOT 267
#define INTERSECT 268
#define ARG_SEP 269
#define ARRAY_COL_SEP 270
#define ARRAY_ROW_SEP 271
#define SHEET_SEP 272
#define INVALID_TOKEN 273
#define tok_WORKBOOKREF 274
#define tok_RIGHT_EXP 275
#define tok_LEFT_EXP 276
#define tok_NEG 277
#define tok_PLUS 278
#define RANGE_INTERSECT 279
#define RANGE_SEP 280

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE YYSTYPE;
union YYSTYPE
{
#line 560 "parser.y" /* yacc.c:355  */

	GnmExpr		*expr;
	GnmValue	*value;
	GnmCellRef	*cell;
	GnmExprList	*list;
	Sheet		*sheet;
	Workbook	*wb;

#line 721 "parser.c" /* yacc.c:355  */
};
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);



/* Copy the second part of user declarations.  */

#line 736 "parser.c" /* yacc.c:358  */

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif


#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  4
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   221

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  42
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  14
/* YYNRULES -- Number of rules.  */
#define YYNRULES  62
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  102

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   280

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,    30,    23,     2,
      36,    37,    26,    25,    33,    24,     2,    27,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      20,    22,    21,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    40,     2,    41,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    38,     2,    39,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    28,    29,    31,    32,    34,
      35
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   590,   590,   596,   604,   610,   613,   614,   615,   619,
     620,   621,   622,   623,   624,   625,   626,   627,   628,   629,
     630,   631,   632,   633,   634,   635,   640,   644,   648,   649,
     651,   669,   676,   677,   695,   716,   733,   734,   737,   738,
     741,   742,   770,   792,   801,   813,   814,   818,   822,   826,
     832,   837,   848,   858,   861,   862,   867,   872,   876,   877,
     882,   890,   895
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "STRING", "QUOTED_STRING", "CONSTANT",
  "RANGEREF", "tok_GTE", "tok_LTE", "tok_NE", "tok_AND", "tok_OR",
  "tok_NOT", "INTERSECT", "ARG_SEP", "ARRAY_COL_SEP", "ARRAY_ROW_SEP",
  "SHEET_SEP", "INVALID_TOKEN", "tok_WORKBOOKREF", "'<'", "'>'", "'='",
  "'&'", "'-'", "'+'", "'*'", "'/'", "tok_RIGHT_EXP", "tok_LEFT_EXP",
  "'%'", "tok_NEG", "tok_PLUS", "','", "RANGE_INTERSECT", "RANGE_SEP",
  "'('", "')'", "'{'", "'}'", "'['", "']'", "$accept", "line", "opt_exp",
  "exp", "function", "string_opt_quote", "opt_sheet_sep", "workbookref",
  "sheetref", "cellref", "arg_list", "array_exp", "array_row",
  "array_rows", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
      60,    62,    61,    38,    45,    43,    42,    47,   275,   276,
      37,   277,   278,    44,   279,   280,    40,    41,   123,   125,
      91,    93
};
# endif

#define YYPACT_NINF -19

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-19)))

#define YYTABLE_NINF -38

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      62,   -19,     7,   108,   -19,   -11,    20,   -19,   -18,   108,
      30,   108,   108,    85,    15,     0,   129,    25,    53,    19,
      32,   -19,    85,    18,     2,   -19,   -19,     2,     2,    85,
     157,    34,   -19,   -19,   -19,    68,    70,   -19,    61,    63,
      38,   -19,    37,   108,   108,   108,   108,   108,   -19,   108,
     108,   108,   108,   108,   108,   108,   108,   108,   108,   -19,
     108,    66,   -19,    65,    75,   -19,    43,    49,   -19,   -19,
     -19,    85,   -19,   -19,   -19,    15,    15,   -19,   -19,     4,
       4,     4,    59,    59,     4,     4,     4,   178,   187,   187,
     165,   165,   165,    73,   -19,   -19,   -19,   -19,   -19,   -19,
     -19,   -19
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     3,     0,     0,     1,     8,     7,     6,    45,     0,
      39,     0,     0,    53,    58,     0,     2,    32,     0,     0,
       0,     9,    53,     0,    28,    38,    40,    26,    27,    53,
      50,     0,    36,    37,    54,     0,     0,    57,    59,    61,
       0,    42,     0,     0,     0,     0,     0,     0,     4,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    29,
       0,     0,    43,    34,     0,    33,     0,     0,    49,    47,
      52,    53,    30,    55,    56,    58,    58,    31,    41,    20,
      22,    21,    23,    24,    18,    19,    17,    16,    11,    10,
      12,    13,    14,    15,    25,    48,    46,    44,    35,    51,
      60,    62
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -19,   -19,   -19,    -1,     3,   -14,   -19,   -19,   -19,   -19,
     -13,   -19,    21,    29
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
      -1,     2,     3,    30,    17,    18,    26,    19,    20,    21,
      31,    38,    39,    40
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int8 yytable[] =
{
      37,    42,    16,    32,    33,    64,   -36,     4,    24,    66,
      27,    28,    46,    47,    46,    47,    70,    23,    32,    33,
      34,    67,    63,    33,    68,    22,    69,    52,    53,    54,
      55,    56,    57,    58,    59,    65,    60,   -37,    60,    35,
      36,    41,    79,    80,    81,    82,    83,    25,    84,    85,
      86,    87,    88,    89,    90,    91,    92,    93,    99,    94,
      61,    37,    37,     1,    96,    -5,    -5,    -5,    -5,    67,
      62,    72,    95,    73,    -5,    74,    75,    77,    78,    76,
      98,    -5,   -36,    46,    47,    22,    -5,    -5,     5,     6,
       7,     8,    97,    60,     0,     0,   100,     9,    -5,    29,
      -5,     0,    -5,    59,    10,   101,     0,    60,     0,    11,
      12,     5,     6,     7,     8,     0,     0,     0,     0,     0,
       9,    13,     0,    14,     0,    15,     0,    10,     0,     0,
       0,     0,    11,    12,     0,     0,    43,    44,    45,    46,
      47,     0,     0,    48,    13,     0,    14,     0,    15,    49,
      50,    51,    52,    53,    54,    55,    56,    57,    58,    59,
       0,     0,     0,    60,    43,    44,    45,    46,    47,     0,
       0,    71,     0,     0,     0,    46,    47,    49,    50,    51,
      52,    53,    54,    55,    56,    57,    58,    59,    46,    47,
       0,    60,     0,    57,    58,    59,     0,    46,    47,    60,
       0,     0,    53,    54,    55,    56,    57,    58,    59,     0,
       0,     0,    60,    55,    56,    57,    58,    59,     0,     0,
       0,    60
};

static const yytype_int8 yycheck[] =
{
      14,    15,     3,     3,     4,    19,    17,     0,     9,    22,
      11,    12,    10,    11,    10,    11,    29,    35,     3,     4,
       5,     3,     3,     4,     6,    36,    23,    23,    24,    25,
      26,    27,    28,    29,    30,     3,    34,    17,    34,    24,
      25,    41,    43,    44,    45,    46,    47,    17,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    58,    71,    60,
      35,    75,    76,     1,    61,     3,     4,     5,     6,     3,
      17,    37,     6,     5,    12,     5,    15,    39,    41,    16,
      37,    19,    17,    10,    11,    36,    24,    25,     3,     4,
       5,     6,    17,    34,    -1,    -1,    75,    12,    36,    14,
      38,    -1,    40,    30,    19,    76,    -1,    34,    -1,    24,
      25,     3,     4,     5,     6,    -1,    -1,    -1,    -1,    -1,
      12,    36,    -1,    38,    -1,    40,    -1,    19,    -1,    -1,
      -1,    -1,    24,    25,    -1,    -1,     7,     8,     9,    10,
      11,    -1,    -1,    14,    36,    -1,    38,    -1,    40,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      -1,    -1,    -1,    34,     7,     8,     9,    10,    11,    -1,
      -1,    14,    -1,    -1,    -1,    10,    11,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    10,    11,
      -1,    34,    -1,    28,    29,    30,    -1,    10,    11,    34,
      -1,    -1,    24,    25,    26,    27,    28,    29,    30,    -1,
      -1,    -1,    34,    26,    27,    28,    29,    30,    -1,    -1,
      -1,    34
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     1,    43,    44,     0,     3,     4,     5,     6,    12,
      19,    24,    25,    36,    38,    40,    45,    46,    47,    49,
      50,    51,    36,    35,    45,    17,    48,    45,    45,    14,
      45,    52,     3,     4,     5,    24,    25,    47,    53,    54,
      55,    41,    47,     7,     8,     9,    10,    11,    14,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      34,    35,    17,     3,    47,     3,    52,     3,     6,    46,
      52,    14,    37,     5,     5,    15,    16,    39,    41,    45,
      45,    45,    45,    45,    45,    45,    45,    45,    45,    45,
      45,    45,    45,    45,    45,     6,    46,    17,    37,    52,
      54,    55
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    42,    43,    43,    44,    44,    45,    45,    45,    45,
      45,    45,    45,    45,    45,    45,    45,    45,    45,    45,
      45,    45,    45,    45,    45,    45,    45,    45,    45,    45,
      45,    45,    45,    45,    45,    46,    47,    47,    48,    48,
      49,    49,    49,    50,    50,    51,    51,    51,    51,    51,
      52,    52,    52,    52,    53,    53,    53,    53,    54,    54,
      54,    55,    55
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     2,     1,     3,     0,     1,     1,     1,     1,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     2,     2,     2,     2,
       3,     3,     1,     2,     2,     4,     1,     1,     1,     0,
       2,     3,     2,     2,     3,     1,     3,     3,     3,     3,
       1,     3,     2,     0,     1,     2,     2,     1,     0,     1,
       3,     1,     3
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;                                                  \
    }                                                           \
while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, int yyrule)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                                              );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
yystrlen (const char *yystr)
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            /* Fall through.  */
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
{
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        YYSTYPE *yyvs1 = yyvs;
        yytype_int16 *yyss1 = yyss;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * sizeof (*yyssp),
                    &yyvs1, yysize * sizeof (*yyvsp),
                    &yystacksize);

        yyss = yyss1;
        yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yytype_int16 *yyss1 = yyss;
        union yyalloc *yyptr =
          (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
                  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 590 "parser.y" /* yacc.c:1646  */
    {
		unregister_allocation ((yyvsp[0].expr));
		unregister_allocation ((yyvsp[-1].list));
		state->result = gnm_expr_list_prepend ((yyvsp[-1].list), (yyvsp[0].expr));
	}
#line 1914 "parser.c" /* yacc.c:1646  */
    break;

  case 3:
#line 596 "parser.y" /* yacc.c:1646  */
    {
		if (state->result != NULL) {
			gnm_expr_list_unref (state->result);
			state->result = NULL;
		}
	}
#line 1925 "parser.c" /* yacc.c:1646  */
    break;

  case 4:
#line 604 "parser.y" /* yacc.c:1646  */
    {
	       unregister_allocation ((yyvsp[-1].expr));
	       unregister_allocation ((yyvsp[-2].list));
	       (yyval.list) = gnm_expr_list_prepend ((yyvsp[-2].list), (yyvsp[-1].expr));
	       register_expr_list_allocation ((yyval.list));
	}
#line 1936 "parser.c" /* yacc.c:1646  */
    break;

  case 5:
#line 610 "parser.y" /* yacc.c:1646  */
    { (yyval.list) = NULL; register_expr_list_allocation ((yyval.list)); }
#line 1942 "parser.c" /* yacc.c:1646  */
    break;

  case 6:
#line 613 "parser.y" /* yacc.c:1646  */
    { (yyval.expr) = (yyvsp[0].expr); }
#line 1948 "parser.c" /* yacc.c:1646  */
    break;

  case 7:
#line 614 "parser.y" /* yacc.c:1646  */
    { (yyval.expr) = (yyvsp[0].expr); }
#line 1954 "parser.c" /* yacc.c:1646  */
    break;

  case 8:
#line 615 "parser.y" /* yacc.c:1646  */
    {
		(yyval.expr) = parser_simple_val_or_name ((yyvsp[0].expr));
		if ((yyval.expr) == NULL) { YYERROR; }
	}
#line 1963 "parser.c" /* yacc.c:1646  */
    break;

  case 9:
#line 619 "parser.y" /* yacc.c:1646  */
    { (yyval.expr) = (yyvsp[0].expr); }
#line 1969 "parser.c" /* yacc.c:1646  */
    break;

  case 10:
#line 620 "parser.y" /* yacc.c:1646  */
    { (yyval.expr) = build_binop ((yyvsp[-2].expr), GNM_EXPR_OP_ADD,	(yyvsp[0].expr)); }
#line 1975 "parser.c" /* yacc.c:1646  */
    break;

  case 11:
#line 621 "parser.y" /* yacc.c:1646  */
    { (yyval.expr) = build_binop ((yyvsp[-2].expr), GNM_EXPR_OP_SUB,	(yyvsp[0].expr)); }
#line 1981 "parser.c" /* yacc.c:1646  */
    break;

  case 12:
#line 622 "parser.y" /* yacc.c:1646  */
    { (yyval.expr) = build_binop ((yyvsp[-2].expr), GNM_EXPR_OP_MULT,	(yyvsp[0].expr)); }
#line 1987 "parser.c" /* yacc.c:1646  */
    break;

  case 13:
#line 623 "parser.y" /* yacc.c:1646  */
    { (yyval.expr) = build_binop ((yyvsp[-2].expr), GNM_EXPR_OP_DIV,	(yyvsp[0].expr)); }
#line 1993 "parser.c" /* yacc.c:1646  */
    break;

  case 14:
#line 624 "parser.y" /* yacc.c:1646  */
    { (yyval.expr) = build_exp ((yyvsp[-2].expr), (yyvsp[0].expr)); }
#line 1999 "parser.c" /* yacc.c:1646  */
    break;

  case 15:
#line 625 "parser.y" /* yacc.c:1646  */
    { (yyval.expr) = build_exp ((yyvsp[-2].expr), (yyvsp[0].expr)); }
#line 2005 "parser.c" /* yacc.c:1646  */
    break;

  case 16:
#line 626 "parser.y" /* yacc.c:1646  */
    { (yyval.expr) = build_binop ((yyvsp[-2].expr), GNM_EXPR_OP_CAT,	(yyvsp[0].expr)); }
#line 2011 "parser.c" /* yacc.c:1646  */
    break;

  case 17:
#line 627 "parser.y" /* yacc.c:1646  */
    { (yyval.expr) = build_binop ((yyvsp[-2].expr), GNM_EXPR_OP_EQUAL,	(yyvsp[0].expr)); }
#line 2017 "parser.c" /* yacc.c:1646  */
    break;

  case 18:
#line 628 "parser.y" /* yacc.c:1646  */
    { (yyval.expr) = build_binop ((yyvsp[-2].expr), GNM_EXPR_OP_LT,		(yyvsp[0].expr)); }
#line 2023 "parser.c" /* yacc.c:1646  */
    break;

  case 19:
#line 629 "parser.y" /* yacc.c:1646  */
    { (yyval.expr) = build_binop ((yyvsp[-2].expr), GNM_EXPR_OP_GT,		(yyvsp[0].expr)); }
#line 2029 "parser.c" /* yacc.c:1646  */
    break;

  case 20:
#line 630 "parser.y" /* yacc.c:1646  */
    { (yyval.expr) = build_binop ((yyvsp[-2].expr), GNM_EXPR_OP_GTE,	(yyvsp[0].expr)); }
#line 2035 "parser.c" /* yacc.c:1646  */
    break;

  case 21:
#line 631 "parser.y" /* yacc.c:1646  */
    { (yyval.expr) = build_binop ((yyvsp[-2].expr), GNM_EXPR_OP_NOT_EQUAL,	(yyvsp[0].expr)); }
#line 2041 "parser.c" /* yacc.c:1646  */
    break;

  case 22:
#line 632 "parser.y" /* yacc.c:1646  */
    { (yyval.expr) = build_binop ((yyvsp[-2].expr), GNM_EXPR_OP_LTE,	(yyvsp[0].expr)); }
#line 2047 "parser.c" /* yacc.c:1646  */
    break;

  case 23:
#line 633 "parser.y" /* yacc.c:1646  */
    { (yyval.expr) = build_logical ((yyvsp[-2].expr), TRUE,	(yyvsp[0].expr)); }
#line 2053 "parser.c" /* yacc.c:1646  */
    break;

  case 24:
#line 634 "parser.y" /* yacc.c:1646  */
    { (yyval.expr) = build_logical ((yyvsp[-2].expr), FALSE, (yyvsp[0].expr)); }
#line 2059 "parser.c" /* yacc.c:1646  */
    break;

  case 25:
#line 635 "parser.y" /* yacc.c:1646  */
    {
		(yyval.expr) = build_intersect ((yyvsp[-2].expr), (yyvsp[0].expr));
		if ((yyval.expr) == NULL) { YYERROR; }
	}
#line 2068 "parser.c" /* yacc.c:1646  */
    break;

  case 26:
#line 640 "parser.y" /* yacc.c:1646  */
    {
		GnmExpr *tmp = fold_negative_constant ((yyvsp[0].expr));
		(yyval.expr) = tmp ? tmp : build_unary_op (GNM_EXPR_OP_UNARY_NEG, (yyvsp[0].expr));
	}
#line 2077 "parser.c" /* yacc.c:1646  */
    break;

  case 27:
#line 644 "parser.y" /* yacc.c:1646  */
    {
		/* Don't fold here.  */
		(yyval.expr) = build_unary_op (GNM_EXPR_OP_UNARY_PLUS, (yyvsp[0].expr));
	}
#line 2086 "parser.c" /* yacc.c:1646  */
    break;

  case 28:
#line 648 "parser.y" /* yacc.c:1646  */
    { (yyval.expr) = build_not ((yyvsp[0].expr)); }
#line 2092 "parser.c" /* yacc.c:1646  */
    break;

  case 29:
#line 649 "parser.y" /* yacc.c:1646  */
    { (yyval.expr) = build_unary_op (GNM_EXPR_OP_PERCENTAGE, (yyvsp[-1].expr)); }
#line 2098 "parser.c" /* yacc.c:1646  */
    break;

  case 30:
#line 651 "parser.y" /* yacc.c:1646  */
    {
		if ((yyvsp[-1].list) == NULL) {
			report_err (state, g_error_new (1, PERR_INVALID_EMPTY,
				_("() is an invalid expression")),
				state->ptr-2, 2);
			YYERROR;
		} else {
			if ((yyvsp[-1].list)->next == NULL) {
				unregister_allocation ((yyvsp[-1].list));
				(yyval.expr) = register_expr_allocation (gnm_expr_new_unary (GNM_EXPR_OP_PAREN, (yyvsp[-1].list)->data));
				/* NOTE : free list not content */
				gnm_expr_list_free ((yyvsp[-1].list));
			} else {
				(yyval.expr) = build_set ((yyvsp[-1].list));
				if ((yyval.expr) == NULL) { YYERROR; }
			}
		}
	}
#line 2121 "parser.c" /* yacc.c:1646  */
    break;

  case 31:
#line 669 "parser.y" /* yacc.c:1646  */
    {
		unregister_allocation ((yyvsp[-1].list));
		(yyval.expr) = build_array ((yyvsp[-1].list));
		free_expr_list_list ((yyvsp[-1].list));
		if ((yyval.expr) == NULL) { YYERROR; }
	}
#line 2132 "parser.c" /* yacc.c:1646  */
    break;

  case 33:
#line 677 "parser.y" /* yacc.c:1646  */
    {
		GnmNamedExpr *nexpr = NULL;
		char const *name = value_peek_string ((yyvsp[0].expr)->constant.value);
		GnmParsePos pos = *state->pos;

		pos.sheet = (yyvsp[-1].sheet);
		nexpr = expr_name_lookup (&pos, name);
		if (nexpr == NULL) {
			report_err (state, g_error_new (1, PERR_UNKNOWN_NAME,
				_("Name '%s' does not exist in sheet '%s'"),
						name, pos.sheet->name_quoted),
				state->ptr, strlen (name));
			YYERROR;
		} else {
			unregister_allocation ((yyvsp[0].expr)); gnm_expr_free ((yyvsp[0].expr));
			(yyval.expr) = register_expr_allocation (gnm_expr_new_name (nexpr, (yyvsp[-1].sheet), NULL));
		}
	}
#line 2155 "parser.c" /* yacc.c:1646  */
    break;

  case 34:
#line 695 "parser.y" /* yacc.c:1646  */
    {
		GnmNamedExpr *nexpr = NULL;
		char const *name = value_peek_string ((yyvsp[0].expr)->constant.value);
		GnmParsePos pos = *state->pos;

		pos.sheet = NULL;
		pos.wb = (yyvsp[-1].wb);
		nexpr = expr_name_lookup (&pos, name);
		if (nexpr != NULL) {
			unregister_allocation ((yyvsp[0].expr)); gnm_expr_free ((yyvsp[0].expr));
			(yyval.expr) = register_expr_allocation (gnm_expr_new_name (nexpr, NULL, (yyvsp[-1].wb)));
		} else {
			report_err (state, g_error_new (1, PERR_UNKNOWN_NAME,
				_("Name '%s' does not exist in workbook"),
							name),
				state->ptr, strlen (name));
			YYERROR;
		}
	}
#line 2179 "parser.c" /* yacc.c:1646  */
    break;

  case 35:
#line 716 "parser.y" /* yacc.c:1646  */
    {
		char const *name = value_peek_string ((yyvsp[-3].expr)->constant.value);
		GnmExpr const *f_call = (*state->convs->input.func) (
			state->convs, state->pos->wb, name, (yyvsp[-1].list));

		(yyval.expr) = NULL;
		if (f_call) {
			/* We're done with the function name.  */
			unregister_allocation ((yyvsp[-3].expr)); gnm_expr_free ((yyvsp[-3].expr));
			unregister_allocation ((yyvsp[-1].list));
			(yyval.expr) = register_expr_allocation (f_call);
		} else {
			YYERROR;
		}
	}
#line 2199 "parser.c" /* yacc.c:1646  */
    break;

  case 41:
#line 742 "parser.y" /* yacc.c:1646  */
    {
		char const *wb_name = value_peek_string ((yyvsp[-1].expr)->constant.value);
		Workbook *ref_wb = state->pos
			? (state->pos->wb
			   ? state->pos->wb
			   : (state->pos->sheet
			      ? state->pos->sheet->workbook
			      : NULL))
			: NULL;
		Workbook *wb =
			state->convs->input.external_wb (state->convs,
							 ref_wb,
							 wb_name);

		if (wb != NULL) {
			unregister_allocation ((yyvsp[-1].expr)); gnm_expr_free ((yyvsp[-1].expr));
			(yyval.wb) = wb;
		} else {
			/* kludge to produce better error messages
			 * we know that the last token read will be the ']'
			 * so subtract 1.
			 */
			report_err (state, g_error_new (1, PERR_UNKNOWN_WORKBOOK,
				_("Unknown workbook '%s'"), wb_name),
				state->ptr - 1, strlen (wb_name));
			YYERROR;
		}
	}
#line 2232 "parser.c" /* yacc.c:1646  */
    break;

  case 42:
#line 770 "parser.y" /* yacc.c:1646  */
    {
		/* Special syntax for global names shadowed by sheet names.  */
		Workbook *wb = state->pos
			? (state->pos->wb
			   ? state->pos->wb
			   : (state->pos->sheet
			      ? state->pos->sheet->workbook
			      : NULL))
			: NULL;
		(yyval.wb) = wb;
		if (wb == NULL) {
			report_err (state, g_error_new (1, PERR_UNKNOWN_WORKBOOK,
				_("Unknown workbook")),
				state->ptr - 1, 1);
			YYERROR;
		}
	}
#line 2254 "parser.c" /* yacc.c:1646  */
    break;

  case 43:
#line 792 "parser.y" /* yacc.c:1646  */
    {
		Sheet *sheet = parser_sheet_by_name (state->pos->wb, (yyvsp[-1].expr));
		if (sheet != NULL) {
			unregister_allocation ((yyvsp[-1].expr)); gnm_expr_free ((yyvsp[-1].expr));
			(yyval.sheet) = sheet;
		} else {
			YYERROR;
		}
	}
#line 2268 "parser.c" /* yacc.c:1646  */
    break;

  case 44:
#line 801 "parser.y" /* yacc.c:1646  */
    {
		Workbook *wb = (yyvsp[-2].wb);
		Sheet *sheet = parser_sheet_by_name (wb, (yyvsp[-1].expr));
		if (sheet != NULL) {
			unregister_allocation ((yyvsp[-1].expr)); gnm_expr_free ((yyvsp[-1].expr));
			(yyval.sheet) = sheet;
		} else {
			YYERROR;
		}
        }
#line 2283 "parser.c" /* yacc.c:1646  */
    break;

  case 45:
#line 813 "parser.y" /* yacc.c:1646  */
    { (yyval.expr) = (yyvsp[0].expr); }
#line 2289 "parser.c" /* yacc.c:1646  */
    break;

  case 46:
#line 814 "parser.y" /* yacc.c:1646  */
    {
		(yyval.expr) = build_range_ctor ((yyvsp[-2].expr), (yyvsp[0].expr), NULL);
		if ((yyval.expr) == NULL) { YYERROR; }
	}
#line 2298 "parser.c" /* yacc.c:1646  */
    break;

  case 47:
#line 818 "parser.y" /* yacc.c:1646  */
    {
		(yyval.expr) = build_range_ctor ((yyvsp[-2].expr), (yyvsp[0].expr), (yyvsp[-2].expr));
		if ((yyval.expr) == NULL) { YYERROR; }
	}
#line 2307 "parser.c" /* yacc.c:1646  */
    break;

  case 48:
#line 822 "parser.y" /* yacc.c:1646  */
    {
		(yyval.expr) = build_range_ctor ((yyvsp[-2].expr), (yyvsp[0].expr), (yyvsp[0].expr));
		if ((yyval.expr) == NULL) { YYERROR; }
	}
#line 2316 "parser.c" /* yacc.c:1646  */
    break;

  case 49:
#line 826 "parser.y" /* yacc.c:1646  */
    {
		(yyval.expr) = build_range_ctor ((yyvsp[-2].expr), (yyvsp[0].expr), NULL);
		if ((yyval.expr) == NULL) { YYERROR; }
	}
#line 2325 "parser.c" /* yacc.c:1646  */
    break;

  case 50:
#line 832 "parser.y" /* yacc.c:1646  */
    {
		unregister_allocation ((yyvsp[0].expr));
		(yyval.list) = gnm_expr_list_prepend (NULL, (yyvsp[0].expr));
		register_expr_list_allocation ((yyval.list));
        }
#line 2335 "parser.c" /* yacc.c:1646  */
    break;

  case 51:
#line 837 "parser.y" /* yacc.c:1646  */
    {
		GSList *tmp = (yyvsp[0].list);
		unregister_allocation ((yyvsp[0].list));
		unregister_allocation ((yyvsp[-2].expr));

		if (tmp == NULL)
			tmp = gnm_expr_list_prepend (NULL, gnm_expr_new_constant (value_new_empty ()));

		(yyval.list) = gnm_expr_list_prepend (tmp, (yyvsp[-2].expr));
		register_expr_list_allocation ((yyval.list));
	}
#line 2351 "parser.c" /* yacc.c:1646  */
    break;

  case 52:
#line 848 "parser.y" /* yacc.c:1646  */
    {
		GSList *tmp = (yyvsp[0].list);
		unregister_allocation ((yyvsp[0].list));

		if (tmp == NULL)
			tmp = gnm_expr_list_prepend (NULL, gnm_expr_new_constant (value_new_empty ()));

		(yyval.list) = gnm_expr_list_prepend (tmp, gnm_expr_new_constant (value_new_empty ()));
		register_expr_list_allocation ((yyval.list));
	}
#line 2366 "parser.c" /* yacc.c:1646  */
    break;

  case 53:
#line 858 "parser.y" /* yacc.c:1646  */
    { (yyval.list) = NULL; }
#line 2372 "parser.c" /* yacc.c:1646  */
    break;

  case 54:
#line 861 "parser.y" /* yacc.c:1646  */
    { (yyval.expr) = (yyvsp[0].expr); }
#line 2378 "parser.c" /* yacc.c:1646  */
    break;

  case 55:
#line 862 "parser.y" /* yacc.c:1646  */
    {
		GnmExpr *tmp = fold_negative_constant ((yyvsp[0].expr));
		if (!tmp) { YYERROR; }
		(yyval.expr) = tmp;
	 }
#line 2388 "parser.c" /* yacc.c:1646  */
    break;

  case 56:
#line 867 "parser.y" /* yacc.c:1646  */
    {
		GnmExpr *tmp = fold_positive_constant ((yyvsp[0].expr));
		if (!tmp) { YYERROR; }
		(yyval.expr) = tmp;
	 }
#line 2398 "parser.c" /* yacc.c:1646  */
    break;

  case 57:
#line 872 "parser.y" /* yacc.c:1646  */
    { (yyval.expr) = parse_string_as_value ((yyvsp[0].expr)); }
#line 2404 "parser.c" /* yacc.c:1646  */
    break;

  case 58:
#line 876 "parser.y" /* yacc.c:1646  */
    { (yyval.list) = NULL; }
#line 2410 "parser.c" /* yacc.c:1646  */
    break;

  case 59:
#line 877 "parser.y" /* yacc.c:1646  */
    {
		unregister_allocation ((yyvsp[0].expr));
		(yyval.list) = g_slist_prepend (NULL, (yyvsp[0].expr));
		register_expr_list_allocation ((yyval.list));
        }
#line 2420 "parser.c" /* yacc.c:1646  */
    break;

  case 60:
#line 882 "parser.y" /* yacc.c:1646  */
    {
		unregister_allocation ((yyvsp[0].list));
		unregister_allocation ((yyvsp[-2].expr));
		(yyval.list) = g_slist_prepend ((yyvsp[0].list), (yyvsp[-2].expr));
		register_expr_list_allocation ((yyval.list));
	}
#line 2431 "parser.c" /* yacc.c:1646  */
    break;

  case 61:
#line 890 "parser.y" /* yacc.c:1646  */
    {
		unregister_allocation ((yyvsp[0].list));
		(yyval.list) = g_slist_prepend (NULL, (yyvsp[0].list));
		register_expr_list_list_allocation ((yyval.list));
        }
#line 2441 "parser.c" /* yacc.c:1646  */
    break;

  case 62:
#line 895 "parser.y" /* yacc.c:1646  */
    {
		unregister_allocation ((yyvsp[0].list));
		unregister_allocation ((yyvsp[-2].list));
		(yyval.list) = g_slist_prepend ((yyvsp[0].list), (yyvsp[-2].list));
		register_expr_list_list_allocation ((yyval.list));
	}
#line 2452 "parser.c" /* yacc.c:1646  */
    break;


#line 2456 "parser.c" /* yacc.c:1646  */
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 903 "parser.y" /* yacc.c:1906  */


static char const *
find_matching_close (char const *str, char const **res)
{
	while (*str) {
		if (*str == '(') {
			char const *tmp = str;
			str = find_matching_close (str + 1, res);
			if (*str != ')' && *res == NULL) {
				*res = tmp;
				return str;
			}
			if (*str == 0)
				return str;
		} else if (*str == ')')
			return str;
		else if (*str == '\'' || *str == '\"') {
			GString *dummy = g_string_new (NULL);
			char const *end = go_strunescape (dummy, str);
			g_string_free (dummy, TRUE);
			if (end == NULL)
				return str + strlen (str);
			str = end;
			continue; /* skip incrementing str */
		}
		str = g_utf8_next_char (str);
	}

	return str;
}

static inline int
eat_space (ParserState *state, int res)
{
	/* help the user by ignoring pointless spaces after an
	 * arg_sep.  We know they are going to be errors and
	 * the spaces can not be operators in this context */
	while (*state->ptr == ' ')
		state->ptr++;
	return res;
}

/*
 * Do we want to ignore space before a given character?
 */
static gboolean
ignore_space_before (gunichar c)
{
	switch (c) {
	case '*': case '/': case '+': case '-': case '%': case '^': case '&':
	case '>': case '<': case '=':
	case ')':
	case '#':
	case '"': case '\'':  /* Refers to opening quote only.  */
	case UNICODE_LOGICAL_NOT_C:
	case UNICODE_LOGICAL_AND_C:
	case UNICODE_LOGICAL_OR_C:
	case UNICODE_MINUS_SIGN_C:
	case UNICODE_DIVISION_SLASH_C:
	case UNICODE_NOT_EQUAL_TO_C:
	case UNICODE_LESS_THAN_OR_EQUAL_TO_C:
	case UNICODE_GREATER_THAN_OR_EQUAL_TO_C:
	case 0:
		return TRUE;
	default:
		return FALSE;
	}
}

/*
 * Do we want to ignore space after a given character?
 */
static gboolean
ignore_space_after (gunichar c)
{
	switch (c) {
	case '*': case '/': case '+': case '-': case '%': case '^': case '&':
	case '>': case '<': case '=':
	case '(':
	case '"': case '\'':  /* Refers to closing quote only [not actually hit].  */
	case UNICODE_LOGICAL_NOT_C:
	case UNICODE_LOGICAL_AND_C:
	case UNICODE_LOGICAL_OR_C:
	case UNICODE_MINUS_SIGN_C:
	case UNICODE_DIVISION_SLASH_C:
	case UNICODE_NOT_EQUAL_TO_C:
	case UNICODE_LESS_THAN_OR_EQUAL_TO_C:
	case UNICODE_GREATER_THAN_OR_EQUAL_TO_C:
	case 0:
		return TRUE;
	default:
		return FALSE;
	}
}

static gboolean
open_paren (const char *p)
{
	while (g_unichar_isspace (g_utf8_get_char (p)))
		p = g_utf8_next_char (p);
	return *p == '(';
}

static int
yylex (void)
{
	gunichar c, tmp;
	char const *start, *end;
	GnmRangeRef ref;
	gboolean is_number = FALSE;
	gboolean is_space = FALSE;
	gboolean error_token = FALSE;

	/*
	 * Some special logic to handle space as intersection char.
	 * Any number of white space characters are treated as one
	 * intersecton.
	 *
	 * Also, if we are not using space for that, drop spaces.
	 */
        while (g_unichar_isspace (g_utf8_get_char (state->ptr))) {
                state->ptr = g_utf8_next_char (state->ptr);
		is_space = TRUE;
	}
	if (is_space && state->convs->intersection_char == ' ' &&
	    !ignore_space_before (g_utf8_get_char (state->ptr)))
		return RANGE_INTERSECT;

	start = state->ptr;
	c = g_utf8_get_char (start);
	if (c == 0)
		return 0;
	state->ptr = g_utf8_next_char (state->ptr);

	if (c == state->convs->intersection_char)
		return RANGE_INTERSECT;

	if (c == '&' && state->convs->decode_ampersands) {
		if (!strncmp (state->ptr, "amp;", 4)) {
			state->ptr += 4;
			return '&';
		}

		if (!strncmp (state->ptr, "lt;", 3)) {
			state->ptr += 3;
			if (*state->ptr == '='){
				state->ptr++;
				return tok_LTE;
			}
			if (!strncmp (state->ptr, "&gt;", 4)) {
				state->ptr += 4;
				return tok_NE;
			}
			return '<';
		}
		if (!strncmp (state->ptr, "gt;", 3)) {
			state->ptr += 3;
			if (*state->ptr == '='){
				state->ptr++;
				return tok_GTE;
			}
			return '>';
		}
		if (!strncmp (state->ptr, "apos;", 5) ||
		    !strncmp (state->ptr, "quot;", 5)) {
			char const *quotes_end;
			char const *p;
			char *string, *s;
			GnmValue *v;

			if (*state->ptr == 'q') {
				quotes_end = "&quot;";
				c = '\"';
			} else {
				quotes_end = "&apos;";
				c = '\'';
			}

			state->ptr += 5;
			p = state->ptr;
			double_quote_loop :
				state->ptr = strstr (state->ptr, quotes_end);
			if (!*state->ptr) {
				report_err (state, g_error_new (1, PERR_MISSING_CLOSING_QUOTE,
								_("Could not find matching closing quote")),
					    p, 1);
				return INVALID_TOKEN;
			}
			if (!strncmp (state->ptr + 6, quotes_end, 6)) {
				state->ptr += 2 * 6;
				goto double_quote_loop;
			}

			s = string = (char *) g_alloca (1 + state->ptr - p);
			while (p != state->ptr) {
				if (*p == '&') {
					if (!strncmp (p, "&amp;", 5)) {
						p += 5;
						*s++ = '&';
						continue;
					} else if (!strncmp (p, "&lt;", 4)) {
						p += 4;
						*s++ = '<';
						continue;
					} else if (!strncmp (p, "&gt;", 4)) {
						p += 4;
						*s++ = '>';
						continue;
					} else if (!strncmp (p, quotes_end, 6)) {
						p += 12; /* two in a row is the escape mechanism */
						*s++ = c;
						continue;
					} else if (!strncmp (p, "&quot;", 6)) {
						p += 6;
						*s++ = '\"';
						continue;
					} else if (!strncmp (p, "&apos;", 6)) {
						p += 6;
						*s++ = '\'';
						continue;
					}
				}
				*s++ = *p++;
			}

			*s = 0;
			state->ptr += 6;

			v = value_new_string (string);
			yylval.expr = register_expr_allocation (gnm_expr_new_constant (v));
			return QUOTED_STRING;
		}
	}

	if (c == ':' && state->convs->range_sep_colon)
		return eat_space (state, RANGE_SEP);

	if (c == state->convs->sheet_name_sep)
		return eat_space (state, SHEET_SEP);

	if (c == '.' && *state->ptr == '.' && state->convs->range_sep_dotdot) {
		state->ptr++;
		return RANGE_SEP;
	}

	if (c == '#' && state->convs->accept_hash_logicals) {
		if (!strncmp (state->ptr, "NOT#", 4)) {
			state->ptr += 4;
			return eat_space (state, tok_NOT);
		}
		if (!strncmp (state->ptr, "AND#", 4)) {
			state->ptr += 4;
			return eat_space (state, tok_AND);
		}
		if (!strncmp (state->ptr, "OR#", 3)) {
			state->ptr += 3;
			return eat_space (state, tok_OR);
		}
	}

	if (c == state->arg_sep)
		return eat_space (state, state->in_array ? state->in_array_sep_is : ARG_SEP);
	if (c == state->array_col_sep)
		return eat_space (state, ARRAY_COL_SEP);
	if (c == state->array_row_sep)
		return eat_space (state, ARRAY_ROW_SEP);

	end = state->convs->input.range_ref (&ref, start,
					     state->pos, state->convs);
	/*
	 * In order to parse "LOG10(1024)" in sheets with more than ~8500
	 * columns we do not consider anything a rangeref if it is followed
	 * by an opening parenthesis.
	 */
	if (start != end && !open_paren (end)) {
		state->ptr = end;
		if (invalid_sheet == ref.a.sheet) {
		        yylval.expr = register_expr_allocation
		                (gnm_expr_new_constant
				 (value_new_error_REF (NULL)));
			return CONSTANT;
		}
		if (state->flags & GNM_EXPR_PARSE_FORCE_ABSOLUTE_REFERENCES) {
			if (ref.a.col_relative) {
				ref.a.col += state->pos->eval.col;
				ref.a.col_relative = FALSE;
			}
			if (ref.b.col_relative) {
				ref.b.col += state->pos->eval.col;
				ref.b.col_relative = FALSE;
			}
			if (ref.a.row_relative) {
				ref.a.row += state->pos->eval.row;
				ref.a.row_relative = FALSE;
			}
			if (ref.b.row_relative) {
				ref.b.row += state->pos->eval.row;
				ref.b.row_relative = FALSE;
			}
		} else if (state->flags & GNM_EXPR_PARSE_FORCE_RELATIVE_REFERENCES) {
			if (!ref.a.col_relative) {
				ref.a.col -= state->pos->eval.col;
				ref.a.col_relative = TRUE;
			}
			if (!ref.b.col_relative) {
				ref.b.col -= state->pos->eval.col;
				ref.b.col_relative = TRUE;
			}
			if (!ref.a.row_relative) {
				ref.a.row -= state->pos->eval.row;
				ref.a.row_relative = TRUE;
			}
			if (!ref.b.row_relative) {
				ref.b.row -= state->pos->eval.row;
				ref.b.row_relative = TRUE;
			}
		}

		if (ref.a.sheet == NULL && (state->flags & GNM_EXPR_PARSE_FORCE_EXPLICIT_SHEET_REFERENCES)) {
			ref.a.sheet = state->pos->sheet;
			if (ref.a.sheet == NULL) {
				report_err (state, g_error_new (1, PERR_SHEET_IS_REQUIRED,
					_("Sheet name is required")),
					state->ptr, 0);
				return INVALID_TOKEN;
			}
		}

		if ((ref.b.sheet == NULL || ref.b.sheet == ref.a.sheet) &&
		    ref.a.col		== ref.b.col &&
		    ref.a.col_relative	== ref.b.col_relative &&
		    ref.a.row		== ref.b.row &&
		    ref.a.row_relative	== ref.b.row_relative) {
			yylval.expr = register_expr_allocation (gnm_expr_new_cellref (&ref.a));
			return RANGEREF;
		}
		yylval.expr = register_expr_allocation (gnm_expr_new_constant (
			 value_new_cellrange_unsafe (&ref.a, &ref.b)));
		return RANGEREF;
	}

	/* Do NOT handle negative numbers here.  That has to be done in the
	 * parser otherwise we mishandle A1-1 when it looks like
	 * rangeref CONSTANT  */
	if (c == state->decimal_point) {
		/* Could be a number or a stand alone  */
		if (!g_unichar_isdigit (g_utf8_get_char (state->ptr)))
			return c;
		is_number = TRUE;
	}  else if (g_unichar_isdigit (c)) {
		/* find the end of the first portion of the number */
		do {
			c = g_utf8_get_char (state->ptr);
			state->ptr = g_utf8_next_char (state->ptr);
		} while (g_unichar_isdigit (c));
		is_number = TRUE;
	}

	if (is_number) {
		GnmValue *v = NULL;

		if (c == state->decimal_point || c == 'e' || c == 'E') {
			/* This is a floating point number */
			char *end;
			gnm_float d;

			errno = 0;
			d = gnm_utf8_strto (start, &end);
			if (start == end) {
				g_warning ("%s is not a double, but was expected to be one", start);
			}  else if (errno != ERANGE) {
				v = value_new_float (d);
				state->ptr = end;
			} else if (c != 'e' && c != 'E') {
				report_err (state, g_error_new (1, PERR_OUT_OF_RANGE,
					_("The number is out of range")),
					state->ptr, end - start);
				return INVALID_TOKEN;
			} else {
				/* For an exponent it's hard to highlight the
				 * right region w/o it turning into an ugly
				 * hack, for now the cursor is put at the end.
				 */
				report_err (state, g_error_new (1, PERR_OUT_OF_RANGE,
					_("The number is out of range")),
					state->ptr, 0);
				return INVALID_TOKEN;
			}
		} else {
			char *end;
			long l;

			l = gnm_utf8_strtol (start, &end);
			if (start == end) {
				g_warning ("%s is not an integer, but was expected to be one", start);
			} else if (errno != ERANGE && l >= INT_MIN && l <= INT_MAX) {
				v = value_new_int (l);
				state->ptr = end;
			} else {
				gnm_float d;

				errno = 0;
				d = gnm_utf8_strto (start, &end);
				if (errno != ERANGE) {
					v = value_new_float (d);
					state->ptr = end;
				} else {
					report_err (state, g_error_new (1, PERR_OUT_OF_RANGE,
						_("The number is out of range")),
						state->ptr, end - start);
					return INVALID_TOKEN;
				}
			}
		}

		/* Very odd string,  Could be a bound problem.  Trigger an error */
		if (v == NULL)
			return c;

		yylval.expr = register_expr_allocation (gnm_expr_new_constant (v));
		return CONSTANT;
	}

	switch (c) {
	case '#':
		if (state->ptr[0] != '"') {
			while ((tmp = g_utf8_get_char (state->ptr)) != 0 &&
			       !g_unichar_isspace (tmp)) {
				state->ptr = g_utf8_next_char (state->ptr);
				if (tmp == '!' || tmp == '?' ||
				((state->ptr - start) == 4 && 0 == strncmp (start, "#N/A", 4))) {
					GOString *name = go_string_new_nocopy (g_strndup (start, state->ptr - start));
					yylval.expr = register_expr_allocation
						(gnm_expr_new_constant (
							value_new_error_str (NULL, name)));
					go_string_unref (name);
					return CONSTANT;
				}
			}

			report_err (state, g_error_new
				    (1, PERR_UNEXPECTED_TOKEN,
				     _("Improperly formatted error token")),
				    state->ptr, state->ptr - start);

			return INVALID_TOKEN;
		}
		error_token = TRUE;
		start++;
		/* Fall through */
	case '\'':
	case '"': {
		GString *s = g_string_new (NULL);
		char const *end = state->convs->input.string (start, s, state->convs);

		if (end == NULL) {
			size_t len = strlen (start);
			g_string_free (s, TRUE);
  			report_err (state,
				    g_error_new (1, PERR_MISSING_CLOSING_QUOTE,
						 _("Could not find matching closing quote")),
				    start + len, len);
			return INVALID_TOKEN;
		}

		state->ptr = (char *)end;

		if (error_token) {
			GnmValue *v = value_new_error (NULL, s->str);
			yylval.expr = register_expr_allocation (gnm_expr_new_constant (v));
			g_string_free (s, TRUE);
			return eat_space (state, CONSTANT);
		} else {
			GnmValue *v = value_new_string_nocopy (g_string_free (s, FALSE));
			yylval.expr = register_expr_allocation (gnm_expr_new_constant (v));
			return eat_space (state, QUOTED_STRING);
		}
	}

	case '[': {
		const char *p = state->ptr;
		GString *s = g_string_new (NULL);
		Workbook *ref_wb = state->pos
			? (state->pos->wb
			   ? state->pos->wb
			   : (state->pos->sheet
			      ? state->pos->sheet->workbook
			      : NULL))
			: NULL;

		while (g_unichar_isspace (g_utf8_get_char (p)))
			p = g_utf8_next_char (p);

		if (p[0] == '"' || p[0] == '\'') {
			p = go_strunescape (s, p);
		} else {
			gunichar uc;
			while (1) {
				uc = g_utf8_get_char (p);
				if (!uc || uc == ']' || g_unichar_isspace (uc))
					break;
				p = g_utf8_next_char (p);
				g_string_append_unichar (s, uc);
			}
		}

		while (p && g_unichar_isspace (g_utf8_get_char (p)))
			p = g_utf8_next_char (p);

		if (s->len == 0 || !p || p[0] != ']') {
			g_string_free (s, TRUE);
			break;
		}

		yylval.wb = state->convs->input.external_wb (state->convs,
							     ref_wb,
							     s->str);
		g_string_free (s, TRUE);
		if (!yylval.wb)
			break;

		state->ptr = p + 1;
		return tok_WORKBOOKREF;
	}
	}

	if ((end = state->convs->input.name (start, state->convs))) {
		state->ptr = end;
		yylval.expr = register_expr_allocation (gnm_expr_new_constant (
			value_new_string_nocopy (g_strndup (start, state->ptr - start))));
		return STRING;
	}

	switch (c) {
	case '<':
		if (*state->ptr == '='){
			state->ptr++;
			return eat_space (state, tok_LTE);
		}
		if (*state->ptr == '>'){
			state->ptr++;
			return eat_space (state, tok_NE);
		}
		return eat_space (state, c);

	case '>':
		if (*state->ptr == '='){
			state->ptr++;
			return eat_space (state, tok_GTE);
		}
		return eat_space (state, c);

	case '\n': return 0;

	case '{' :
		state->in_array++;
		return c;
	case '}' :
		state->in_array--;
		return c;

	case '^':
		return state->convs->exp_is_left_associative
			? tok_LEFT_EXP
			: tok_RIGHT_EXP;

	case UNICODE_LOGICAL_NOT_C: return tok_NOT;
	case UNICODE_MINUS_SIGN_C: return '-';
	case UNICODE_DIVISION_SLASH_C: return '/';
	case UNICODE_LOGICAL_AND_C: return tok_AND;
	case UNICODE_LOGICAL_OR_C: return tok_OR;
	case UNICODE_NOT_EQUAL_TO_C: return eat_space (state, tok_NE);
	case UNICODE_LESS_THAN_OR_EQUAL_TO_C: return eat_space (state, tok_LTE);
	case UNICODE_GREATER_THAN_OR_EQUAL_TO_C: return eat_space (state, tok_GTE);
	}

	if (ignore_space_after (c))
		return eat_space (state, c);
	else
		return c;
}

int
yyerror (char const *s)
{
#if 0
	g_printerr ("Error: %s\n", s);
#endif
	return 0;
}

static void
setup_state (ParserState *pstate, const char *str,
	     GnmParsePos const *pp,
	     GnmExprParseFlags flags,
	     GnmConventions const *convs,
	     GnmParseError *error)
{
	pstate->start = pstate->ptr = str;
	pstate->pos   = pp;

	pstate->flags		= flags;
	pstate->convs                                    =
		(NULL != convs) ? convs : ((NULL != pp->sheet) ? pp->sheet->convs : gnm_conventions_default);


	pstate->decimal_point = pstate->convs->decimal_sep_dot
		? '.'
		: g_utf8_get_char (go_locale_get_decimal ()->str); /* FIXME: one char handled.  */

	if (pstate->convs->arg_sep != 0)
		pstate->arg_sep = pstate->convs->arg_sep;
	else
		pstate->arg_sep = go_locale_get_arg_sep ();
	if (pstate->convs->array_col_sep != 0)
		pstate->array_col_sep = pstate->convs->array_col_sep;
	else
		pstate->array_col_sep = go_locale_get_col_sep ();
	if (pstate->convs->array_row_sep != 0)
		pstate->array_row_sep = pstate->convs->array_row_sep;
	else
		pstate->array_row_sep = go_locale_get_row_sep ();

	/* Some locales/conventions have ARG_SEP == ARRAY_ROW_SEP
	 * 	eg {1\2\3;4\5\6} for XL style with ',' as a decimal
	 * some have ARG_SEP == ARRAY_COL_SEPARATOR
	 * 	eg {1,2,3;4,5,6} for XL style with '.' as a decimal
	 * 	or {1;2;3|4;5;6} for OOo/
	 * keep track of whether we are in an array to allow the lexer to
	 * dis-ambiguate. */
	if (pstate->arg_sep == pstate->array_col_sep)
		pstate->in_array_sep_is = ARRAY_COL_SEP;
	else if (pstate->arg_sep == pstate->array_row_sep)
		pstate->in_array_sep_is = ARRAY_ROW_SEP;
	else
		pstate->in_array_sep_is = ARG_SEP;
	pstate->in_array = 0;

	pstate->result = NULL;
	pstate->error = error;

	state = pstate;
}

/**
 * gnm_expr_parse_str:
 *
 * @str   : The string to parse.
 * @pp	  : #GnmParsePos
 * @flags : See parse-utils for descriptions
 * @convs : optionally NULL #GnmConventions
 * @error : optionally NULL ptr to store details of error.
 *
 * Parse a string. if @error is non-null it will be assumed that the
 * caller has passed a pointer to a GnmParseError struct AND that it will
 * take responsibility for freeing that struct and its contents.
 * with parse_error_free.
 * If @convs is NULL use the conventions from @pp.
 **/
GnmExprTop const *
gnm_expr_parse_str (char const *str, GnmParsePos const *pp,
		    GnmExprParseFlags flags,
		    GnmConventions const *convs,
		    GnmParseError *error)
{
	GnmExpr const *expr;
	ParserState pstate;

	g_return_val_if_fail (str != NULL, NULL);
	g_return_val_if_fail (pp != NULL, NULL);
	g_return_val_if_fail (state == NULL, NULL);

	if (deallocate_stack == NULL)
		deallocate_init ();

	setup_state (&pstate, str, pp, flags, convs, error);
	yyparse ();
	state = NULL;

	if (pstate.result != NULL) {
		deallocate_assert_empty ();

#if 0
		/* If this happens, something is very wrong */
		if (pstate.error != NULL && pstate.error->message != NULL) {
			g_warning ("An error occurred and the GnmExpr is non-null! This should not happen");
			g_warning ("Error message is %s (%d, %d)", pstate.error->message, pstate.error->begin_char,
					pstate.error->end_char);
		}
#endif

		/* Do we have multiple expressions */
		if (pstate.result->next != NULL) {
			if (flags & GNM_EXPR_PARSE_PERMIT_MULTIPLE_EXPRESSIONS)
				expr = gnm_expr_new_set (g_slist_reverse (pstate.result));
			else {
				gnm_expr_list_unref (pstate.result);
				report_err (&pstate, g_error_new (1, PERR_MULTIPLE_EXPRESSIONS,
					_("Multiple expressions are not supported in this context")),
					pstate.start,
					(pstate.ptr - pstate.start));
				expr = NULL;
			}
		} else {
			/* Free the list, do not unref the content */
			expr = pstate.result->data;
			gnm_expr_list_free (pstate.result);
		}
	} else {
		/* If there is no error message, attempt to be more detailed */
		if (pstate.error != NULL &&
		    (pstate.error->err == NULL || pstate.error->err->message == NULL)) {
			char const *last_token = pstate.ptr;

			if (*last_token == '\0') {
				char const *str = pstate.start;
				char const *res = NULL;
				char const *last = find_matching_close (str, &res);

				if (*last)
					report_err (&pstate, g_error_new (1, PERR_MISSING_PAREN_OPEN,
						_("Could not find matching opening parenthesis")),
						last, 1);
				else if (res != NULL)
					report_err (&pstate, g_error_new (1, PERR_MISSING_PAREN_CLOSE,
						_("Could not find matching closing parenthesis")),
						res, 1);
				else
					report_err (&pstate, g_error_new (1, PERR_INVALID_EXPRESSION,
						_("Invalid expression")),
						pstate.ptr, pstate.ptr - pstate.start);
			} else
				report_err (&pstate, g_error_new (1, PERR_UNEXPECTED_TOKEN,
					_("Unexpected token %c"), *last_token),
					last_token, 1);
		}

		deallocate_all ();

		expr = NULL;
	}

	deallocate_uninit ();

	return gnm_expr_top_new (expr);
}

GnmLexerItem *
gnm_expr_lex_all (char const *str, GnmParsePos const *pp,
		  GnmExprParseFlags flags,
		  GnmConventions const *convs)
{
	GnmLexerItem *res = NULL;
	int n = 0, alloc = 0;
	ParserState pstate;
	GnmParseError *error = NULL;

	g_return_val_if_fail (str != NULL, NULL);
	g_return_val_if_fail (pp != NULL, NULL);

	if (deallocate_stack == NULL)
		deallocate_init ();

	setup_state (&pstate, str, pp, flags, convs, error);

	while (1) {
		int len;

		if (alloc <= n) {
			alloc = alloc * 2 + 20;
			res = g_renew (GnmLexerItem, res, alloc);
		}

		res[n].start = pstate.ptr - pstate.start;
		res[n].token = yylex ();
		res[n].end = pstate.ptr - pstate.start;

		if (res[n].token == 0)
			break;

		len = res[n].end - res[n].start;
		/* Kill spaces that got eaten, but not a space operator */
		while (len > 1 && str[res[n].start] == ' ') {
			res[n].start++;
			len--;
		}
		while (len > 1 && str[res[n].end - 1] == ' ') {
			res[n].end--;
			len--;
		}

		n++;
	}

	deallocate_all ();

	state = NULL;

	return res;
}
