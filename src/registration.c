#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void ocaode_initmod_desolve(void *);
extern void ocaode_rhs_dde(void *);
extern void ocaode_rhs_desolve(void *);

/* .Call calls */
extern SEXP ocaode_contents(SEXP);
extern SEXP ocaode_create(SEXP);
extern SEXP ocaode_initial_conditions(SEXP, SEXP);
extern SEXP ocaode_metadata(SEXP);
extern SEXP ocaode_rhs_r(SEXP, SEXP, SEXP);
extern SEXP ocaode_set_initial(SEXP, SEXP, SEXP, SEXP);
extern SEXP ocaode_set_user(SEXP, SEXP);

static const R_CMethodDef CEntries[] = {
    {"ocaode_initmod_desolve", (DL_FUNC) &ocaode_initmod_desolve, 1},
    {"ocaode_rhs_dde",         (DL_FUNC) &ocaode_rhs_dde,         1},
    {"ocaode_rhs_desolve",     (DL_FUNC) &ocaode_rhs_desolve,     1},
    {NULL, NULL, 0}
};

static const R_CallMethodDef CallEntries[] = {
    {"ocaode_contents",           (DL_FUNC) &ocaode_contents,           1},
    {"ocaode_create",             (DL_FUNC) &ocaode_create,             1},
    {"ocaode_initial_conditions", (DL_FUNC) &ocaode_initial_conditions, 2},
    {"ocaode_metadata",           (DL_FUNC) &ocaode_metadata,           1},
    {"ocaode_rhs_r",              (DL_FUNC) &ocaode_rhs_r,              3},
    {"ocaode_set_initial",        (DL_FUNC) &ocaode_set_initial,        4},
    {"ocaode_set_user",           (DL_FUNC) &ocaode_set_user,           2},
    {NULL, NULL, 0}
};

void R_init_OCA1(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
