// Automatically generated by odin 1.5.11 - do not edit
#include <float.h>
#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>
#include <stdbool.h>
#include <R_ext/Rdynload.h>
#ifndef CINTERPOLTE_CINTERPOLATE_H_
#define CINTERPOLTE_CINTERPOLATE_H_

// Allow use from C++
#ifdef __cplusplus
extern "C" {
#endif

// There are only three functions in the interface; allocation,
// evaluation and freeing.

// Allocate an interpolation object.
//
//   type: The mode of interpolation. Must be one of "constant",
//       "linear" or "spline" (an R error is thrown if a different
//       value is given).
//
//   n: The number of `x` points to interpolate over
//
//   ny: the number of `y` points per `x` point.  This is 1 in the
//       case of zimple interpolation as used by Rs `interpolate()`
//
//   x: an array of `x` values of length `n`
//
//   y: an array of `ny` sets of `y` values.  This is in R's matrix
//       order (i.e., the first `n` values are the first series to
//       interpolate over).
//
//   fail_on_extrapolate: if true, when an extrapolation occurs throw
//       an error; if false return NA_REAL
//
//   auto_free: automatically clean up the interpolation object on
//       return to R. This uses `R_alloc` for allocations rather than
//       `Calloc` so freeing will always happen (even on error
//       elsewhere in the code). However, this prevents returning back
//       a pointer to R that will last longer than the call into C
//       code.
//
// The return value is an opaque pointer that can be passed through to
// `cinterpolate_eval` and `cinterpolate_free`
void *cinterpolate_alloc(const char *type, size_t n, size_t ny,
                         double *x, double *y, bool fail_on_extrapolate,
                         bool auto_free);

// Evaluate the interpolated function at a new `x` point.
//
//   x: A new, single, `x` point to interpolate `y` values to
//
//   obj: The interpolation object, as returned by `cinterpolate_alloc`
//
//   y: An array of length `ny` to store the interpolated values
//
// The return value is 0 if the interpolation is successful (with x
// lying within the range of values that the interpolation function
// supports), -1 otherwise
int cinterpolate_eval(double x, void *obj, double *y);

// Clean up all allocated memory
//
//   obj: The interpolation object, as returned by `cinterpolate_alloc`
void cinterpolate_free(void *obj);

#ifdef __cplusplus
}
#endif

#endif
typedef struct ocaode_internal {
  double *BF;
  double *birthrisk;
  double *BM;
  int dim_BF;
  int dim_birthrisk;
  int dim_BM;
  int dim_N;
  int dim_N_1;
  int dim_N_12;
  int dim_N_123;
  int dim_N_2;
  int dim_N_3;
  int dim_N_4;
  int dim_native;
  int dim_omegaF;
  int dim_omegaM;
  int dim_popdatF;
  int dim_popdatF_1;
  int dim_popdatF_2;
  int dim_popdatM;
  int dim_popdatM_1;
  int dim_popdatM_2;
  int dim_popinitF;
  int dim_popinitF_1;
  int dim_popinitF_12;
  int dim_popinitF_2;
  int dim_popinitF_3;
  int dim_popinitM;
  int dim_popinitM_1;
  int dim_popinitM_12;
  int dim_popinitM_2;
  int dim_popinitM_3;
  int dim_r;
  int dim_ttp;
  double *initial_N;
  void *interpolate_bzf;
  void *interpolate_bzm;
  void *interpolate_omegaF;
  void *interpolate_omegaM;
  int lttp;
  int nage;
  double *native;
  int nnat;
  int nrisk;
  double *omegaF;
  double *omegaM;
  double *popdatF;
  double *popdatM;
  double *popinitF;
  double *popinitM;
  double *r;
  double *ttp;
} ocaode_internal;
ocaode_internal* ocaode_get_internal(SEXP internal_p, int closed_error);
static void ocaode_finalise(SEXP internal_p);
SEXP ocaode_create(SEXP user);
void ocaode_initmod_desolve(void(* odeparms) (int *, double *));
SEXP ocaode_contents(SEXP internal_p);
SEXP ocaode_set_user(SEXP internal_p, SEXP user);
SEXP ocaode_set_initial(SEXP internal_p, SEXP t_ptr, SEXP state_ptr, SEXP ocaode_use_dde_ptr);
SEXP ocaode_metadata(SEXP internal_p);
SEXP ocaode_initial_conditions(SEXP internal_p, SEXP t_ptr);
void ocaode_rhs(ocaode_internal* internal, double t, double * state, double * dstatedt, double * output);
void ocaode_rhs_dde(size_t neq, double t, double * state, double * dstatedt, void * internal);
void ocaode_rhs_desolve(int * neq, double * t, double * state, double * dstatedt, double * output, int * np);
SEXP ocaode_rhs_r(SEXP internal_p, SEXP t, SEXP state);
double user_get_scalar_double(SEXP user, const char *name,
                              double default_value, double min, double max);
int user_get_scalar_int(SEXP user, const char *name,
                        int default_value, double min, double max);
void user_check_values_double(double * value, size_t len,
                                  double min, double max, const char *name);
void user_check_values_int(int * value, size_t len,
                               double min, double max, const char *name);
void user_check_values(SEXP value, double min, double max,
                           const char *name);
SEXP user_list_element(SEXP list, const char *name);
void odin_set_dim(SEXP target, int rank, ...);
void* user_get_array_dim(SEXP user, bool is_integer, void * previous,
                         const char *name, int rank,
                         double min, double max, int *dest_dim);
void* user_get_array(SEXP user, bool is_integer, void * previous,
                     const char *name, double min, double max,
                     int rank, ...);
SEXP user_get_array_check(SEXP el, bool is_integer, const char *name,
                          double min, double max);
SEXP user_get_array_check_rank(SEXP user, const char *name, int rank,
                               bool required);
void interpolate_check_y(size_t nx, size_t ny, size_t i, const char *name_arg, const char *name_target);
double scalar_real(SEXP x, const char * name);
ocaode_internal* ocaode_get_internal(SEXP internal_p, int closed_error) {
  ocaode_internal *internal = NULL;
  if (TYPEOF(internal_p) != EXTPTRSXP) {
    Rf_error("Expected an external pointer");
  }
  internal = (ocaode_internal*) R_ExternalPtrAddr(internal_p);
  if (!internal && closed_error) {
    Rf_error("Pointer has been invalidated");
  }
  return internal;
}
void ocaode_finalise(SEXP internal_p) {
  ocaode_internal *internal = ocaode_get_internal(internal_p, 0);
  if (internal_p) {
    cinterpolate_free(internal->interpolate_bzf);
    cinterpolate_free(internal->interpolate_bzm);
    cinterpolate_free(internal->interpolate_omegaF);
    cinterpolate_free(internal->interpolate_omegaM);
    internal->interpolate_bzf = NULL;
    internal->interpolate_bzm = NULL;
    internal->interpolate_omegaF = NULL;
    internal->interpolate_omegaM = NULL;
    R_Free(internal->BF);
    R_Free(internal->birthrisk);
    R_Free(internal->BM);
    R_Free(internal->initial_N);
    R_Free(internal->native);
    R_Free(internal->omegaF);
    R_Free(internal->omegaM);
    R_Free(internal->popdatF);
    R_Free(internal->popdatM);
    R_Free(internal->popinitF);
    R_Free(internal->popinitM);
    R_Free(internal->r);
    R_Free(internal->ttp);
    R_Free(internal);
    R_ClearExternalPtr(internal_p);
  }
}
SEXP ocaode_create(SEXP user) {
  ocaode_internal *internal = (ocaode_internal*) R_Calloc(1, ocaode_internal);
  internal->BF = NULL;
  internal->birthrisk = NULL;
  internal->BM = NULL;
  internal->initial_N = NULL;
  internal->interpolate_omegaF = NULL;
  internal->interpolate_omegaM = NULL;
  internal->native = NULL;
  internal->omegaF = NULL;
  internal->omegaM = NULL;
  internal->popdatF = NULL;
  internal->popdatM = NULL;
  internal->popinitF = NULL;
  internal->popinitM = NULL;
  internal->r = NULL;
  internal->ttp = NULL;
  internal->BF = NULL;
  internal->birthrisk = NULL;
  internal->BM = NULL;
  internal->nage = NA_INTEGER;
  internal->nnat = NA_INTEGER;
  internal->nrisk = NA_INTEGER;
  internal->popdatF = NULL;
  internal->popdatM = NULL;
  internal->popinitF = NULL;
  internal->popinitM = NULL;
  internal->r = NULL;
  internal->ttp = NULL;
  SEXP ptr = PROTECT(R_MakeExternalPtr(internal, R_NilValue, R_NilValue));
  R_RegisterCFinalizer(ptr, ocaode_finalise);
  UNPROTECT(1);
  return ptr;
}
static ocaode_internal *ocaode_internal_ds;
void ocaode_initmod_desolve(void(* odeparms) (int *, double *)) {
  static DL_FUNC get_desolve_gparms = NULL;
  if (get_desolve_gparms == NULL) {
    get_desolve_gparms =
      R_GetCCallable("deSolve", "get_deSolve_gparms");
  }
  ocaode_internal_ds = ocaode_get_internal(get_desolve_gparms(), 1);
}
SEXP ocaode_contents(SEXP internal_p) {
  ocaode_internal *internal = ocaode_get_internal(internal_p, 1);
  SEXP contents = PROTECT(allocVector(VECSXP, 52));
  SEXP BF = PROTECT(allocVector(REALSXP, internal->dim_BF));
  memcpy(REAL(BF), internal->BF, internal->dim_BF * sizeof(double));
  SET_VECTOR_ELT(contents, 0, BF);
  SEXP birthrisk = PROTECT(allocVector(REALSXP, internal->dim_birthrisk));
  memcpy(REAL(birthrisk), internal->birthrisk, internal->dim_birthrisk * sizeof(double));
  SET_VECTOR_ELT(contents, 1, birthrisk);
  SEXP BM = PROTECT(allocVector(REALSXP, internal->dim_BM));
  memcpy(REAL(BM), internal->BM, internal->dim_BM * sizeof(double));
  SET_VECTOR_ELT(contents, 2, BM);
  SET_VECTOR_ELT(contents, 3, ScalarInteger(internal->dim_BF));
  SET_VECTOR_ELT(contents, 4, ScalarInteger(internal->dim_birthrisk));
  SET_VECTOR_ELT(contents, 5, ScalarInteger(internal->dim_BM));
  SET_VECTOR_ELT(contents, 6, ScalarInteger(internal->dim_N));
  SET_VECTOR_ELT(contents, 7, ScalarInteger(internal->dim_N_1));
  SET_VECTOR_ELT(contents, 8, ScalarInteger(internal->dim_N_12));
  SET_VECTOR_ELT(contents, 9, ScalarInteger(internal->dim_N_123));
  SET_VECTOR_ELT(contents, 10, ScalarInteger(internal->dim_N_2));
  SET_VECTOR_ELT(contents, 11, ScalarInteger(internal->dim_N_3));
  SET_VECTOR_ELT(contents, 12, ScalarInteger(internal->dim_N_4));
  SET_VECTOR_ELT(contents, 13, ScalarInteger(internal->dim_native));
  SET_VECTOR_ELT(contents, 14, ScalarInteger(internal->dim_omegaF));
  SET_VECTOR_ELT(contents, 15, ScalarInteger(internal->dim_omegaM));
  SET_VECTOR_ELT(contents, 16, ScalarInteger(internal->dim_popdatF));
  SET_VECTOR_ELT(contents, 17, ScalarInteger(internal->dim_popdatF_1));
  SET_VECTOR_ELT(contents, 18, ScalarInteger(internal->dim_popdatF_2));
  SET_VECTOR_ELT(contents, 19, ScalarInteger(internal->dim_popdatM));
  SET_VECTOR_ELT(contents, 20, ScalarInteger(internal->dim_popdatM_1));
  SET_VECTOR_ELT(contents, 21, ScalarInteger(internal->dim_popdatM_2));
  SET_VECTOR_ELT(contents, 22, ScalarInteger(internal->dim_popinitF));
  SET_VECTOR_ELT(contents, 23, ScalarInteger(internal->dim_popinitF_1));
  SET_VECTOR_ELT(contents, 24, ScalarInteger(internal->dim_popinitF_12));
  SET_VECTOR_ELT(contents, 25, ScalarInteger(internal->dim_popinitF_2));
  SET_VECTOR_ELT(contents, 26, ScalarInteger(internal->dim_popinitF_3));
  SET_VECTOR_ELT(contents, 27, ScalarInteger(internal->dim_popinitM));
  SET_VECTOR_ELT(contents, 28, ScalarInteger(internal->dim_popinitM_1));
  SET_VECTOR_ELT(contents, 29, ScalarInteger(internal->dim_popinitM_12));
  SET_VECTOR_ELT(contents, 30, ScalarInteger(internal->dim_popinitM_2));
  SET_VECTOR_ELT(contents, 31, ScalarInteger(internal->dim_popinitM_3));
  SET_VECTOR_ELT(contents, 32, ScalarInteger(internal->dim_r));
  SET_VECTOR_ELT(contents, 33, ScalarInteger(internal->dim_ttp));
  SEXP initial_N = PROTECT(allocVector(REALSXP, internal->dim_N));
  memcpy(REAL(initial_N), internal->initial_N, internal->dim_N * sizeof(double));
  odin_set_dim(initial_N, 4, internal->dim_N_1, internal->dim_N_2, internal->dim_N_3, internal->dim_N_4);
  SET_VECTOR_ELT(contents, 34, initial_N);
  SET_VECTOR_ELT(contents, 39, ScalarInteger(internal->lttp));
  SET_VECTOR_ELT(contents, 40, ScalarInteger(internal->nage));
  SEXP native = PROTECT(allocVector(REALSXP, internal->dim_native));
  memcpy(REAL(native), internal->native, internal->dim_native * sizeof(double));
  SET_VECTOR_ELT(contents, 41, native);
  SET_VECTOR_ELT(contents, 42, ScalarInteger(internal->nnat));
  SET_VECTOR_ELT(contents, 43, ScalarInteger(internal->nrisk));
  SEXP omegaF = PROTECT(allocVector(REALSXP, internal->dim_omegaF));
  memcpy(REAL(omegaF), internal->omegaF, internal->dim_omegaF * sizeof(double));
  SET_VECTOR_ELT(contents, 44, omegaF);
  SEXP omegaM = PROTECT(allocVector(REALSXP, internal->dim_omegaM));
  memcpy(REAL(omegaM), internal->omegaM, internal->dim_omegaM * sizeof(double));
  SET_VECTOR_ELT(contents, 45, omegaM);
  SEXP popdatF = PROTECT(allocVector(REALSXP, internal->dim_popdatF));
  memcpy(REAL(popdatF), internal->popdatF, internal->dim_popdatF * sizeof(double));
  odin_set_dim(popdatF, 2, internal->dim_popdatF_1, internal->dim_popdatF_2);
  SET_VECTOR_ELT(contents, 46, popdatF);
  SEXP popdatM = PROTECT(allocVector(REALSXP, internal->dim_popdatM));
  memcpy(REAL(popdatM), internal->popdatM, internal->dim_popdatM * sizeof(double));
  odin_set_dim(popdatM, 2, internal->dim_popdatM_1, internal->dim_popdatM_2);
  SET_VECTOR_ELT(contents, 47, popdatM);
  SEXP popinitF = PROTECT(allocVector(REALSXP, internal->dim_popinitF));
  memcpy(REAL(popinitF), internal->popinitF, internal->dim_popinitF * sizeof(double));
  odin_set_dim(popinitF, 3, internal->dim_popinitF_1, internal->dim_popinitF_2, internal->dim_popinitF_3);
  SET_VECTOR_ELT(contents, 48, popinitF);
  SEXP popinitM = PROTECT(allocVector(REALSXP, internal->dim_popinitM));
  memcpy(REAL(popinitM), internal->popinitM, internal->dim_popinitM * sizeof(double));
  odin_set_dim(popinitM, 3, internal->dim_popinitM_1, internal->dim_popinitM_2, internal->dim_popinitM_3);
  SET_VECTOR_ELT(contents, 49, popinitM);
  SEXP r = PROTECT(allocVector(REALSXP, internal->dim_r));
  memcpy(REAL(r), internal->r, internal->dim_r * sizeof(double));
  SET_VECTOR_ELT(contents, 50, r);
  SEXP ttp = PROTECT(allocVector(REALSXP, internal->dim_ttp));
  memcpy(REAL(ttp), internal->ttp, internal->dim_ttp * sizeof(double));
  SET_VECTOR_ELT(contents, 51, ttp);
  SEXP nms = PROTECT(allocVector(STRSXP, 52));
  SET_STRING_ELT(nms, 0, mkChar("BF"));
  SET_STRING_ELT(nms, 1, mkChar("birthrisk"));
  SET_STRING_ELT(nms, 2, mkChar("BM"));
  SET_STRING_ELT(nms, 3, mkChar("dim_BF"));
  SET_STRING_ELT(nms, 4, mkChar("dim_birthrisk"));
  SET_STRING_ELT(nms, 5, mkChar("dim_BM"));
  SET_STRING_ELT(nms, 6, mkChar("dim_N"));
  SET_STRING_ELT(nms, 7, mkChar("dim_N_1"));
  SET_STRING_ELT(nms, 8, mkChar("dim_N_12"));
  SET_STRING_ELT(nms, 9, mkChar("dim_N_123"));
  SET_STRING_ELT(nms, 10, mkChar("dim_N_2"));
  SET_STRING_ELT(nms, 11, mkChar("dim_N_3"));
  SET_STRING_ELT(nms, 12, mkChar("dim_N_4"));
  SET_STRING_ELT(nms, 13, mkChar("dim_native"));
  SET_STRING_ELT(nms, 14, mkChar("dim_omegaF"));
  SET_STRING_ELT(nms, 15, mkChar("dim_omegaM"));
  SET_STRING_ELT(nms, 16, mkChar("dim_popdatF"));
  SET_STRING_ELT(nms, 17, mkChar("dim_popdatF_1"));
  SET_STRING_ELT(nms, 18, mkChar("dim_popdatF_2"));
  SET_STRING_ELT(nms, 19, mkChar("dim_popdatM"));
  SET_STRING_ELT(nms, 20, mkChar("dim_popdatM_1"));
  SET_STRING_ELT(nms, 21, mkChar("dim_popdatM_2"));
  SET_STRING_ELT(nms, 22, mkChar("dim_popinitF"));
  SET_STRING_ELT(nms, 23, mkChar("dim_popinitF_1"));
  SET_STRING_ELT(nms, 24, mkChar("dim_popinitF_12"));
  SET_STRING_ELT(nms, 25, mkChar("dim_popinitF_2"));
  SET_STRING_ELT(nms, 26, mkChar("dim_popinitF_3"));
  SET_STRING_ELT(nms, 27, mkChar("dim_popinitM"));
  SET_STRING_ELT(nms, 28, mkChar("dim_popinitM_1"));
  SET_STRING_ELT(nms, 29, mkChar("dim_popinitM_12"));
  SET_STRING_ELT(nms, 30, mkChar("dim_popinitM_2"));
  SET_STRING_ELT(nms, 31, mkChar("dim_popinitM_3"));
  SET_STRING_ELT(nms, 32, mkChar("dim_r"));
  SET_STRING_ELT(nms, 33, mkChar("dim_ttp"));
  SET_STRING_ELT(nms, 34, mkChar("initial_N"));
  SET_STRING_ELT(nms, 35, mkChar("interpolate_bzf"));
  SET_STRING_ELT(nms, 36, mkChar("interpolate_bzm"));
  SET_STRING_ELT(nms, 37, mkChar("interpolate_omegaF"));
  SET_STRING_ELT(nms, 38, mkChar("interpolate_omegaM"));
  SET_STRING_ELT(nms, 39, mkChar("lttp"));
  SET_STRING_ELT(nms, 40, mkChar("nage"));
  SET_STRING_ELT(nms, 41, mkChar("native"));
  SET_STRING_ELT(nms, 42, mkChar("nnat"));
  SET_STRING_ELT(nms, 43, mkChar("nrisk"));
  SET_STRING_ELT(nms, 44, mkChar("omegaF"));
  SET_STRING_ELT(nms, 45, mkChar("omegaM"));
  SET_STRING_ELT(nms, 46, mkChar("popdatF"));
  SET_STRING_ELT(nms, 47, mkChar("popdatM"));
  SET_STRING_ELT(nms, 48, mkChar("popinitF"));
  SET_STRING_ELT(nms, 49, mkChar("popinitM"));
  SET_STRING_ELT(nms, 50, mkChar("r"));
  SET_STRING_ELT(nms, 51, mkChar("ttp"));
  setAttrib(contents, R_NamesSymbol, nms);
  UNPROTECT(15);
  return contents;
}
SEXP ocaode_set_user(SEXP internal_p, SEXP user) {
  ocaode_internal *internal = ocaode_get_internal(internal_p, 1);
  internal->nage = user_get_scalar_int(user, "nage", internal->nage, NA_REAL, NA_REAL);
  internal->nnat = user_get_scalar_int(user, "nnat", internal->nnat, NA_REAL, NA_REAL);
  internal->nrisk = user_get_scalar_int(user, "nrisk", internal->nrisk, NA_REAL, NA_REAL);
  internal->ttp = (double*) user_get_array_dim(user, false, internal->ttp, "ttp", 1, NA_REAL, NA_REAL, &internal->dim_ttp);
  internal->dim_birthrisk = internal->nrisk;
  internal->dim_N_1 = internal->nage;
  internal->dim_N_2 = 2;
  internal->dim_N_3 = internal->nnat;
  internal->dim_N_4 = internal->nrisk;
  internal->dim_native = internal->nnat;
  internal->dim_omegaF = internal->nage;
  internal->dim_omegaM = internal->nage;
  internal->dim_popinitF_1 = internal->nage;
  internal->dim_popinitF_2 = internal->nnat;
  internal->dim_popinitF_3 = internal->nrisk;
  internal->dim_popinitM_1 = internal->nage;
  internal->dim_popinitM_2 = internal->nnat;
  internal->dim_popinitM_3 = internal->nrisk;
  internal->dim_r = internal->nage;
  R_Free(internal->native);
  internal->native = (double*) R_Calloc(internal->dim_native, double);
  R_Free(internal->omegaF);
  internal->omegaF = (double*) R_Calloc(internal->dim_omegaF, double);
  R_Free(internal->omegaM);
  internal->omegaM = (double*) R_Calloc(internal->dim_omegaM, double);
  internal->birthrisk = (double*) user_get_array(user, false, internal->birthrisk, "birthrisk", NA_REAL, NA_REAL, 1, internal->dim_birthrisk);
  internal->dim_N = internal->dim_N_1 * internal->dim_N_2 * internal->dim_N_3 * internal->dim_N_4;
  internal->dim_N_12 = internal->dim_N_1 * internal->dim_N_2;
  internal->dim_N_123 = internal->dim_N_1 * internal->dim_N_2 * internal->dim_N_3;
  internal->dim_popinitF = internal->dim_popinitF_1 * internal->dim_popinitF_2 * internal->dim_popinitF_3;
  internal->dim_popinitF_12 = internal->dim_popinitF_1 * internal->dim_popinitF_2;
  internal->dim_popinitM = internal->dim_popinitM_1 * internal->dim_popinitM_2 * internal->dim_popinitM_3;
  internal->dim_popinitM_12 = internal->dim_popinitM_1 * internal->dim_popinitM_2;
  internal->lttp = internal->dim_ttp;
  internal->r = (double*) user_get_array(user, false, internal->r, "r", NA_REAL, NA_REAL, 1, internal->dim_r);
  R_Free(internal->initial_N);
  internal->initial_N = (double*) R_Calloc(internal->dim_N, double);
  internal->dim_BF = internal->lttp;
  internal->dim_BM = internal->lttp;
  internal->dim_popdatF_1 = internal->lttp;
  internal->dim_popdatF_2 = internal->nage;
  internal->dim_popdatM_1 = internal->lttp;
  internal->dim_popdatM_2 = internal->nage;
  {
     int i = 1;
     internal->native[i - 1] = 1;
  }
  for (int i = 2; i <= internal->nnat; ++i) {
    internal->native[i - 1] = 0;
  }
  internal->popinitF = (double*) user_get_array(user, false, internal->popinitF, "popinitF", NA_REAL, NA_REAL, 3, internal->dim_popinitF_1, internal->dim_popinitF_2, internal->dim_popinitF_3);
  internal->popinitM = (double*) user_get_array(user, false, internal->popinitM, "popinitM", NA_REAL, NA_REAL, 3, internal->dim_popinitM_1, internal->dim_popinitM_2, internal->dim_popinitM_3);
  internal->BF = (double*) user_get_array(user, false, internal->BF, "BF", NA_REAL, NA_REAL, 1, internal->dim_BF);
  internal->BM = (double*) user_get_array(user, false, internal->BM, "BM", NA_REAL, NA_REAL, 1, internal->dim_BM);
  internal->dim_popdatF = internal->dim_popdatF_1 * internal->dim_popdatF_2;
  internal->dim_popdatM = internal->dim_popdatM_1 * internal->dim_popdatM_2;
  for (int i = 1; i <= internal->nage; ++i) {
    int j = 1;
    for (int k = 1; k <= internal->nnat; ++k) {
      for (int l = 1; l <= internal->nrisk; ++l) {
        internal->initial_N[i - 1 + internal->dim_N_1 * (j - 1) + internal->dim_N_12 * (k - 1) + internal->dim_N_123 * (l - 1)] = internal->popinitM[internal->dim_popinitM_12 * (l - 1) + internal->dim_popinitM_1 * (k - 1) + i - 1];
      }
    }
  }
  for (int i = 1; i <= internal->nage; ++i) {
    int j = 2;
    for (int k = 1; k <= internal->nnat; ++k) {
      for (int l = 1; l <= internal->nrisk; ++l) {
        internal->initial_N[i - 1 + internal->dim_N_1 * (j - 1) + internal->dim_N_12 * (k - 1) + internal->dim_N_123 * (l - 1)] = internal->popinitF[internal->dim_popinitF_12 * (l - 1) + internal->dim_popinitF_1 * (k - 1) + i - 1];
      }
    }
  }
  interpolate_check_y(internal->dim_ttp, internal->dim_BF, 0, "BF", "bzf");
  cinterpolate_free(internal->interpolate_bzf);
  internal->interpolate_bzf = cinterpolate_alloc("linear", internal->dim_ttp, 1, internal->ttp, internal->BF, true, false);
  interpolate_check_y(internal->dim_ttp, internal->dim_BM, 0, "BM", "bzm");
  cinterpolate_free(internal->interpolate_bzm);
  internal->interpolate_bzm = cinterpolate_alloc("linear", internal->dim_ttp, 1, internal->ttp, internal->BM, true, false);
  internal->popdatF = (double*) user_get_array(user, false, internal->popdatF, "popdatF", NA_REAL, NA_REAL, 2, internal->dim_popdatF_1, internal->dim_popdatF_2);
  internal->popdatM = (double*) user_get_array(user, false, internal->popdatM, "popdatM", NA_REAL, NA_REAL, 2, internal->dim_popdatM_1, internal->dim_popdatM_2);
  interpolate_check_y(internal->dim_ttp, internal->dim_popdatF_1, 1, "popdatF", "omegaF");
  interpolate_check_y(internal->dim_omegaF, internal->dim_popdatF_2, 2, "popdatF", "omegaF");
  cinterpolate_free(internal->interpolate_omegaF);
  internal->interpolate_omegaF = cinterpolate_alloc("linear", internal->dim_ttp, internal->dim_omegaF, internal->ttp, internal->popdatF, true, false);
  interpolate_check_y(internal->dim_ttp, internal->dim_popdatM_1, 1, "popdatM", "omegaM");
  interpolate_check_y(internal->dim_omegaM, internal->dim_popdatM_2, 2, "popdatM", "omegaM");
  cinterpolate_free(internal->interpolate_omegaM);
  internal->interpolate_omegaM = cinterpolate_alloc("linear", internal->dim_ttp, internal->dim_omegaM, internal->ttp, internal->popdatM, true, false);
  return R_NilValue;
}
SEXP ocaode_set_initial(SEXP internal_p, SEXP t_ptr, SEXP state_ptr, SEXP ocaode_use_dde_ptr) {
  return R_NilValue;
}
SEXP ocaode_metadata(SEXP internal_p) {
  ocaode_internal *internal = ocaode_get_internal(internal_p, 1);
  SEXP ret = PROTECT(allocVector(VECSXP, 4));
  SEXP nms = PROTECT(allocVector(STRSXP, 4));
  SET_STRING_ELT(nms, 0, mkChar("variable_order"));
  SET_STRING_ELT(nms, 1, mkChar("output_order"));
  SET_STRING_ELT(nms, 2, mkChar("n_out"));
  SET_STRING_ELT(nms, 3, mkChar("interpolate_t"));
  setAttrib(ret, R_NamesSymbol, nms);
  SEXP variable_length = PROTECT(allocVector(VECSXP, 1));
  SEXP variable_names = PROTECT(allocVector(STRSXP, 1));
  setAttrib(variable_length, R_NamesSymbol, variable_names);
  SET_VECTOR_ELT(variable_length, 0, allocVector(INTSXP, 4));
  int * dim_N = INTEGER(VECTOR_ELT(variable_length, 0));
  dim_N[0] = internal->dim_N_1;
  dim_N[1] = internal->dim_N_2;
  dim_N[2] = internal->dim_N_3;
  dim_N[3] = internal->dim_N_4;
  SET_STRING_ELT(variable_names, 0, mkChar("N"));
  SET_VECTOR_ELT(ret, 0, variable_length);
  UNPROTECT(2);
  SET_VECTOR_ELT(ret, 1, R_NilValue);
  SET_VECTOR_ELT(ret, 2, ScalarInteger(0));
  SEXP interpolate_t = PROTECT(allocVector(VECSXP, 3));
  SEXP interpolate_t_nms = PROTECT(allocVector(STRSXP, 3));
  setAttrib(interpolate_t, R_NamesSymbol, interpolate_t_nms);
  SET_VECTOR_ELT(interpolate_t, 0, ScalarReal(internal->ttp[0]));
  SET_VECTOR_ELT(interpolate_t, 1, ScalarReal(internal->ttp[internal->dim_ttp - 1]));
  SET_STRING_ELT(interpolate_t_nms, 0, mkChar("min"));
  SET_STRING_ELT(interpolate_t_nms, 1, mkChar("max"));
  SET_VECTOR_ELT(ret, 3, interpolate_t);
  UNPROTECT(2);
  UNPROTECT(2);
  return ret;
}
SEXP ocaode_initial_conditions(SEXP internal_p, SEXP t_ptr) {
  ocaode_internal *internal = ocaode_get_internal(internal_p, 1);
  SEXP r_state = PROTECT(allocVector(REALSXP, internal->dim_N));
  double * state = REAL(r_state);
  memcpy(state + 0, internal->initial_N, internal->dim_N * sizeof(double));
  UNPROTECT(1);
  return r_state;
}
void ocaode_rhs(ocaode_internal* internal, double t, double * state, double * dstatedt, double * output) {
  double * N = state + 0;
  double bzf = 0.0;
  cinterpolate_eval(t, internal->interpolate_bzf, &bzf);
  double bzm = 0.0;
  cinterpolate_eval(t, internal->interpolate_bzm, &bzm);
  cinterpolate_eval(t, internal->interpolate_omegaF, internal->omegaF);
  cinterpolate_eval(t, internal->interpolate_omegaM, internal->omegaM);
  {
     int i = 1;
     int j = 1;
     for (int k = 1; k <= internal->nnat; ++k) {
       for (int l = 1; l <= internal->nrisk; ++l) {
         dstatedt[0 + i - 1 + internal->dim_N_1 * (j - 1) + internal->dim_N_12 * (k - 1) + internal->dim_N_123 * (l - 1)] = bzm * internal->native[k - 1] * internal->birthrisk[l - 1] - internal->omegaM[0] * N[internal->dim_N_123 * (l - 1) + internal->dim_N_12 * (k - 1) + internal->dim_N_1 * 0 + 0];
       }
     }
  }
  for (int i = 2; i <= internal->nage; ++i) {
    int j = 1;
    for (int k = 1; k <= internal->nnat; ++k) {
      for (int l = 1; l <= internal->nrisk; ++l) {
        dstatedt[0 + i - 1 + internal->dim_N_1 * (j - 1) + internal->dim_N_12 * (k - 1) + internal->dim_N_123 * (l - 1)] = internal->r[i - 1 - 1] * N[internal->dim_N_123 * (l - 1) + internal->dim_N_12 * (k - 1) + internal->dim_N_1 * 0 + i - 1 - 1] - internal->omegaM[i - 1] * N[internal->dim_N_123 * (l - 1) + internal->dim_N_12 * (k - 1) + internal->dim_N_1 * 0 + i - 1];
      }
    }
  }
  {
     int i = 1;
     int j = 2;
     for (int k = 1; k <= internal->nnat; ++k) {
       for (int l = 1; l <= internal->nrisk; ++l) {
         dstatedt[0 + i - 1 + internal->dim_N_1 * (j - 1) + internal->dim_N_12 * (k - 1) + internal->dim_N_123 * (l - 1)] = bzf * internal->native[k - 1] * internal->birthrisk[l - 1] - internal->omegaF[0] * N[internal->dim_N_123 * (l - 1) + internal->dim_N_12 * (k - 1) + internal->dim_N_1 * 1 + 0];
       }
     }
  }
  for (int i = 2; i <= internal->nage; ++i) {
    int j = 2;
    for (int k = 1; k <= internal->nnat; ++k) {
      for (int l = 1; l <= internal->nrisk; ++l) {
        dstatedt[0 + i - 1 + internal->dim_N_1 * (j - 1) + internal->dim_N_12 * (k - 1) + internal->dim_N_123 * (l - 1)] = internal->r[i - 1 - 1] * N[internal->dim_N_123 * (l - 1) + internal->dim_N_12 * (k - 1) + internal->dim_N_1 * 1 + i - 1 - 1] - internal->omegaF[i - 1] * N[internal->dim_N_123 * (l - 1) + internal->dim_N_12 * (k - 1) + internal->dim_N_1 * 1 + i - 1];
      }
    }
  }
}
void ocaode_rhs_dde(size_t neq, double t, double * state, double * dstatedt, void * internal) {
  ocaode_rhs((ocaode_internal*)internal, t, state, dstatedt, NULL);
}
void ocaode_rhs_desolve(int * neq, double * t, double * state, double * dstatedt, double * output, int * np) {
  ocaode_rhs(ocaode_internal_ds, *t, state, dstatedt, output);
}
SEXP ocaode_rhs_r(SEXP internal_p, SEXP t, SEXP state) {
  SEXP dstatedt = PROTECT(allocVector(REALSXP, LENGTH(state)));
  ocaode_internal *internal = ocaode_get_internal(internal_p, 1);
  double *output = NULL;
  ocaode_rhs(internal, scalar_real(t, "t"), REAL(state), REAL(dstatedt), output);
  UNPROTECT(1);
  return dstatedt;
}
double user_get_scalar_double(SEXP user, const char *name,
                              double default_value, double min, double max) {
  double ret = default_value;
  SEXP el = user_list_element(user, name);
  if (el != R_NilValue) {
    if (length(el) != 1) {
      Rf_error("Expected a scalar numeric for '%s'", name);
    }
    if (TYPEOF(el) == REALSXP) {
      ret = REAL(el)[0];
    } else if (TYPEOF(el) == INTSXP) {
      ret = INTEGER(el)[0];
    } else {
      Rf_error("Expected a numeric value for '%s'", name);
    }
  }
  if (ISNA(ret)) {
    Rf_error("Expected a value for '%s'", name);
  }
  user_check_values_double(&ret, 1, min, max, name);
  return ret;
}
int user_get_scalar_int(SEXP user, const char *name,
                        int default_value, double min, double max) {
  int ret = default_value;
  SEXP el = user_list_element(user, name);
  if (el != R_NilValue) {
    if (length(el) != 1) {
      Rf_error("Expected scalar integer for '%s'", name);
    }
    if (TYPEOF(el) == REALSXP) {
      double tmp = REAL(el)[0];
      if (fabs(tmp - round(tmp)) > 2e-8) {
        Rf_error("Expected '%s' to be integer-like", name);
      }
    }
    ret = INTEGER(coerceVector(el, INTSXP))[0];
  }
  if (ret == NA_INTEGER) {
    Rf_error("Expected a value for '%s'", name);
  }
  user_check_values_int(&ret, 1, min, max, name);
  return ret;
}
void user_check_values_double(double * value, size_t len,
                                  double min, double max, const char *name) {
  for (size_t i = 0; i < len; ++i) {
    if (ISNA(value[i])) {
      Rf_error("'%s' must not contain any NA values", name);
    }
  }
  if (min != NA_REAL) {
    for (size_t i = 0; i < len; ++i) {
      if (value[i] < min) {
        Rf_error("Expected '%s' to be at least %g", name, min);
      }
    }
  }
  if (max != NA_REAL) {
    for (size_t i = 0; i < len; ++i) {
      if (value[i] > max) {
        Rf_error("Expected '%s' to be at most %g", name, max);
      }
    }
  }
}
void user_check_values_int(int * value, size_t len,
                               double min, double max, const char *name) {
  for (size_t i = 0; i < len; ++i) {
    if (ISNA(value[i])) {
      Rf_error("'%s' must not contain any NA values", name);
    }
  }
  if (min != NA_REAL) {
    for (size_t i = 0; i < len; ++i) {
      if (value[i] < min) {
        Rf_error("Expected '%s' to be at least %g", name, min);
      }
    }
  }
  if (max != NA_REAL) {
    for (size_t i = 0; i < len; ++i) {
      if (value[i] > max) {
        Rf_error("Expected '%s' to be at most %g", name, max);
      }
    }
  }
}
void user_check_values(SEXP value, double min, double max,
                           const char *name) {
  size_t len = (size_t)length(value);
  if (TYPEOF(value) == INTSXP) {
    user_check_values_int(INTEGER(value), len, min, max, name);
  } else {
    user_check_values_double(REAL(value), len, min, max, name);
  }
}
SEXP user_list_element(SEXP list, const char *name) {
  SEXP ret = R_NilValue, names = getAttrib(list, R_NamesSymbol);
  for (int i = 0; i < length(list); ++i) {
    if (strcmp(CHAR(STRING_ELT(names, i)), name) == 0) {
      ret = VECTOR_ELT(list, i);
      break;
    }
  }
  return ret;
}
void odin_set_dim(SEXP target, int rank, ...) {
  SEXP r_dim = PROTECT(allocVector(INTSXP, rank));
  int *dim = INTEGER(r_dim);

  va_list ap;
  va_start(ap, rank);
  for (size_t i = 0; i < (size_t)rank; ++i) {
    dim[i] = va_arg(ap, int);
  }
  va_end(ap);

  setAttrib(target, R_DimSymbol, r_dim);
  UNPROTECT(1);
}
void* user_get_array_dim(SEXP user, bool is_integer, void * previous,
                         const char *name, int rank,
                         double min, double max, int *dest_dim) {
  SEXP el = user_get_array_check_rank(user, name, rank, previous == NULL);
  if (el == R_NilValue) {
    return previous;
  }

  dest_dim[0] = LENGTH(el);
  if (rank > 1) {
    SEXP r_dim = PROTECT(coerceVector(getAttrib(el, R_DimSymbol), INTSXP));
    int *dim = INTEGER(r_dim);

    for (size_t i = 0; i < (size_t) rank; ++i) {
      dest_dim[i + 1] = dim[i];
    }

    UNPROTECT(1);
  }

  el = PROTECT(user_get_array_check(el, is_integer, name, min, max));

  int len = LENGTH(el);
  void *dest = NULL;
  if (is_integer) {
    dest = R_Calloc(len, int);
    memcpy(dest, INTEGER(el), len * sizeof(int));
  } else {
    dest = R_Calloc(len, double);
    memcpy(dest, REAL(el), len * sizeof(double));
  }
  R_Free(previous);

  UNPROTECT(1);

  return dest;
}
void* user_get_array(SEXP user, bool is_integer, void * previous,
                     const char *name, double min, double max,
                     int rank, ...) {
  SEXP el = user_get_array_check_rank(user, name, rank, previous == NULL);
  if (el == R_NilValue) {
    return previous;
  }

  SEXP r_dim;
  int *dim;

  size_t len = LENGTH(el);
  if (rank == 1) {
    r_dim = PROTECT(ScalarInteger(len));
  } else {
    r_dim = PROTECT(coerceVector(getAttrib(el, R_DimSymbol), INTSXP));
  }
  dim = INTEGER(r_dim);

  va_list ap;
  va_start(ap, rank);
  for (size_t i = 0; i < (size_t) rank; ++i) {
    int dim_expected = va_arg(ap, int);
    if (dim[i] != dim_expected) {
      va_end(ap); // avoid a leak
      if (rank == 1) {
        Rf_error("Expected length %d value for '%s'", dim_expected, name);
      } else {
        Rf_error("Incorrect size of dimension %d of '%s' (expected %d)",
                 (int)i + 1, name, dim_expected);
      }
    }
  }
  va_end(ap);
  UNPROTECT(1);

  el = PROTECT(user_get_array_check(el, is_integer, name, min, max));

  void *dest = NULL;
  if (is_integer) {
    dest = R_Calloc(len, int);
    memcpy(dest, INTEGER(el), len * sizeof(int));
  } else {
    dest = R_Calloc(len, double);
    memcpy(dest, REAL(el), len * sizeof(double));
  }
  R_Free(previous);

  UNPROTECT(1);

  return dest;
}
SEXP user_get_array_check(SEXP el, bool is_integer, const char *name,
                          double min, double max) {
  size_t len = (size_t) length(el);
  if (is_integer) {
    if (TYPEOF(el) == INTSXP) {
      user_check_values_int(INTEGER(el), len, min, max, name);
    } else if (TYPEOF(el) == REALSXP) {
      el = PROTECT(coerceVector(el, INTSXP));
      user_check_values_int(INTEGER(el), len, min, max, name);
      UNPROTECT(1);
    } else {
      Rf_error("Expected a integer value for '%s'", name);
    }
  } else {
    if (TYPEOF(el) == INTSXP) {
      el = PROTECT(coerceVector(el, REALSXP));
      user_check_values_double(REAL(el), len, min, max, name);
      UNPROTECT(1);
    } else if (TYPEOF(el) == REALSXP) {
      user_check_values_double(REAL(el), len, min, max, name);
    } else {
      Rf_error("Expected a numeric value for '%s'", name);
    }
  }
  return el;
}
SEXP user_get_array_check_rank(SEXP user, const char *name, int rank,
                               bool required) {
  SEXP el = user_list_element(user, name);
  if (el == R_NilValue) {
    if (required) {
      Rf_error("Expected a value for '%s'", name);
    }
  } else {
    if (rank == 1) {
      if (isArray(el)) {
        Rf_error("Expected a numeric vector for '%s'", name);
      }
    } else {
      SEXP r_dim = getAttrib(el, R_DimSymbol);
      if (r_dim == R_NilValue || LENGTH(r_dim) != rank) {
        if (rank == 2) {
          Rf_error("Expected a numeric matrix for '%s'", name);
        } else {
          Rf_error("Expected a numeric array of rank %d for '%s'", rank, name);
        }
      }
    }
  }
  return el;
}
void interpolate_check_y(size_t nx, size_t ny, size_t i, const char *name_arg, const char *name_target) {
  if (nx != ny) {
    if (i == 0) {
      // vector case
      Rf_error("Expected %s to have length %d (for '%s')",
               name_arg, (int)nx, name_target);
    } else {
      // array case
      Rf_error("Expected dimension %d of %s to have size %d (for '%s')",
               (int)i, name_arg, (int)nx, name_target);
    }
  }
}
double scalar_real(SEXP x, const char * name) {
  if (Rf_length(x) != 1) {
    Rf_error("Expected a scalar for '%s'", name);
  }
  double ret = 0.0;
  if (TYPEOF(x) == INTSXP) {
    ret = INTEGER(x)[0];
  } else if (TYPEOF(x) == REALSXP) {
    ret = REAL(x)[0];
  } else {
    Rf_error("Expected a numeric value for '%s'", name);
  }
  return ret;
}
// This construction is to help odin
#ifndef CINTERPOLTE_CINTERPOLATE_H_
#endif


void * cinterpolate_alloc(const char *type, size_t n, size_t ny,
                          double *x, double *y, bool fail_on_extrapolate,
                          bool auto_clean) {
  typedef void* interpolate_alloc_t(const char *, size_t, size_t,
                                    double*, double*, bool, bool);
  static interpolate_alloc_t *fun;
  if (fun == NULL) {
    fun = (interpolate_alloc_t*)
      R_GetCCallable("cinterpolate", "interpolate_alloc");
  }
  return fun(type, n, ny, x, y, fail_on_extrapolate, auto_clean);
}


int cinterpolate_eval(double x, void *obj, double *y) {
  typedef int interpolate_eval_t(double, void*, double*);
  static interpolate_eval_t *fun;
  if (fun == NULL) {
    fun = (interpolate_eval_t*)
      R_GetCCallable("cinterpolate", "interpolate_eval");
  }
  return fun(x, obj, y);
}


void cinterpolate_free(void *obj) {
  typedef int interpolate_free_t(void*);
  static interpolate_free_t *fun;
  if (fun == NULL) {
    fun = (interpolate_free_t*)
      R_GetCCallable("cinterpolate", "interpolate_free");
  }
  fun(obj);
}
