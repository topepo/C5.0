#include <R.h>
#include <R_ext/Rdynload.h>
#include <Rinternals.h>
#include <string.h>

#include "rdata.h"
#include "redefine.h"
#include "rsample.h"
#include "rulebasedmodels.h"
#include "strbuf.h"
#include "trace.h"

extern void c50main(void);
extern void sample(double *outputv);
extern void FreeCases(void);

static void c50(char **namesv, char **datav, char **costv, int *subset,
                int *rules, int *utility, int *trials, int *winnow,
                double *sample, int *seed, int *noGlobalPruning, double *CF,
                int *minCases, int *fuzzyThreshold, int *earlyStopping,
                char **treev, char **rulesv, char **outputv) {
  int val; /* Used by setjmp/longjmp for implementing rbm_exit */

  // Announce ourselves for testing
  // Rprintf("c50 called\n");

  // Initialize the globals to the values that the c50
  // program would have at the start of execution
  initglobals();

  // Set globals based on the arguments.  This is analogous
  // to parsing the command line in the c50 program.
  setglobals(*subset, *rules, *utility, *trials, *winnow, *sample, *seed,
             *noGlobalPruning, *CF, *minCases, *fuzzyThreshold, *earlyStopping,
             *costv);

  // Handles the strbufv data structure
  rbm_removeall();

  // Deallocates memory allocated by NewCase.
  // Not necessary since it's also called at the end of this function,
  // but it doesn't hurt, and I'm feeling paranoid.
  FreeCases();

  // XXX Should this be controlled via an option?
  // Rprintf("Calling setOf\n");
  setOf();

  // Create a strbuf using *namesv as the buffer.
  // Note that this is a readonly strbuf since we can't
  // extend *namesv.
  STRBUF *sb_names = strbuf_create_full(*namesv, strlen(*namesv));

  // Register this strbuf using the name "undefined.names"
  if (rbm_register(sb_names, "undefined.names", 0) < 0) {
    error("undefined.names already exists");
  }

  // Create a strbuf using *datav and register it as "undefined.data"
  STRBUF *sb_datav = strbuf_create_full(*datav, strlen(*datav));
  // XXX why is sb_datav copied? was that part of my debugging?
  // XXX or is this the cause of the leak?
  if (rbm_register(strbuf_copy(sb_datav), "undefined.data", 0) < 0) {
    error("undefined data already exists");
  }

  // Create a strbuf using *costv and register it as "undefined.costs"
  if (strlen(*costv) > 0) {
    // Rprintf("registering cost matrix: %s", *costv);
    STRBUF *sb_costv = strbuf_create_full(*costv, strlen(*costv));
    // XXX should sb_costv be copied?
    if (rbm_register(sb_costv, "undefined.costs", 0) < 0) {
      error("undefined.cost already exists");
    }
  } else {
    // Rprintf("no cost matrix to register\n");
  }

  /*
   * We need to initialize rbm_buf before calling any code that
   * might call exit/rbm_exit.
   */
  if ((val = setjmp(rbm_buf)) == 0) {

    // Real work is done here
    // Rprintf("Calling c50main\n");
    c50main();

    // Rprintf("c50main finished\n");

    if (*rules == 0) {
      // Get the contents of the the tree file
      STRBUF *treebuf = rbm_lookup("undefined.tree");
      if (treebuf != NULL) {
        char *treeString = strbuf_getall(treebuf);
        char *treeObj = R_alloc(strlen(treeString) + 1, 1);
        strcpy(treeObj, treeString);

        // I think the previous value of *treev will be garbage collected
        *treev = treeObj;
      } else {
        // XXX Should *treev be assigned something in this case?
        // XXX Throw an error?
      }
    } else {
      // Get the contents of the the rules file
      STRBUF *rulesbuf = rbm_lookup("undefined.rules");
      if (rulesbuf != NULL) {
        char *rulesString = strbuf_getall(rulesbuf);
        char *rulesObj = R_alloc(strlen(rulesString) + 1, 1);
        strcpy(rulesObj, rulesString);

        // I think the previous value of *rulesv will be garbage collected
        *rulesv = rulesObj;
      } else {
        // XXX Should *rulesv be assigned something in this case?
        // XXX Throw an error?
      }
    }
  } else {
    Rprintf("c50 code called exit with value %d\n", val - JMP_OFFSET);
  }

  // Close file object "Of", and return its contents via argument outputv
  char *outputString = closeOf();
  char *output = R_alloc(strlen(outputString) + 1, 1);
  strcpy(output, outputString);
  *outputv = output;

  // Deallocates memory allocated by NewCase
  FreeCases();

  // We reinitialize the globals on exit out of general paranoia
  initglobals();
}

static void predictions(char **casev, char **namesv, char **treev,
                        char **rulesv, char **costv,
                        int *predv, /* XXX predictions are character */
                        double *confidencev, int *trials, char **outputv) {
  int val; /* Used by setjmp/longjmp for implementing rbm_exit */

  // Announce ourselves for testing
  // Rprintf("predictions called\n");

  // Initialize the globals
  initglobals();

  // Handles the strbufv data structure
  rbm_removeall();

  // XXX Should this be controlled via an option?
  // Rprintf("Calling setOf\n");
  setOf();

  STRBUF *sb_cases = strbuf_create_full(*casev, strlen(*casev));
  if (rbm_register(sb_cases, "undefined.cases", 0) < 0) {
    error("undefined.cases already exists");
  }

  STRBUF *sb_names = strbuf_create_full(*namesv, strlen(*namesv));
  if (rbm_register(sb_names, "undefined.names", 0) < 0) {
    error("undefined.names already exists");
  }

  if (strlen(*treev)) {
    STRBUF *sb_treev = strbuf_create_full(*treev, strlen(*treev));
    /* XXX should sb_treev be copied? */
    if (rbm_register(sb_treev, "undefined.tree", 0) < 0) {
      error("undefined.tree already exists");
    }
  } else if (strlen(*rulesv)) {
    STRBUF *sb_rulesv = strbuf_create_full(*rulesv, strlen(*rulesv));
    /* XXX should sb_rulesv be copied? */
    if (rbm_register(sb_rulesv, "undefined.rules", 0) < 0) {
      error("undefined.rules already exists");
    }
    setrules(1);
  } else {
    error("either a tree or rules must be provided");
  }

  // Create a strbuf using *costv and register it as "undefined.costs"
  if (strlen(*costv) > 0) {
    // Rprintf("registering cost matrix: %s", *costv);
    STRBUF *sb_costv = strbuf_create_full(*costv, strlen(*costv));
    // XXX should sb_costv be copied?
    if (rbm_register(sb_costv, "undefined.costs", 0) < 0) {
      error("undefined.cost already exists");
    }
  } else {
    // Rprintf("no cost matrix to register\n");
  }

  /*
   * We need to initialize rbm_buf before calling any code that
   * might call exit/rbm_exit.
   */
  if ((val = setjmp(rbm_buf)) == 0) {
    // Real work is done here
    // Rprintf("\n\nCalling rpredictmain\n");
    rpredictmain(trials, predv, confidencev);

    // Rprintf("predict finished\n\n");
  } else {
    // Rprintf("predict code called exit with value %d\n\n", val - JMP_OFFSET);
  }

  // Close file object "Of", and return its contents via argument outputv
  char *outputString = closeOf();
  char *output = R_alloc(strlen(outputString) + 1, 1);
  strcpy(output, outputString);
  *outputv = output;

  // We reinitialize the globals on exit out of general paranoia
  initglobals();
}

/*
 * .Call interface: data are passed as R vectors and loaded by rdata.c,
 * with no .names/.data/.costs text in between.  All argument checking
 * happens here, before any C5.0 globals are touched, so that R errors
 * cannot leave the C code half initialized.  The R side (R/data-spec.R)
 * builds these arguments; the checks below only guard against misuse.
 */

static SEXP list_elt(SEXP list, const char *name) {
  SEXP names = Rf_getAttrib(list, R_NamesSymbol);
  if (TYPEOF(list) != VECSXP || TYPEOF(names) != STRSXP) {
    Rf_error("internal error: the attribute spec must be a named list");
  }
  for (R_xlen_t i = 0; i < Rf_xlength(list); i++) {
    if (strcmp(CHAR(STRING_ELT(names, i)), name) == 0) {
      return VECTOR_ELT(list, i);
    }
  }
  Rf_error("internal error: attribute spec has no element '%s'", name);
  return R_NilValue; /* not reached */
}

static const char *string_copy(SEXP s) {
  const char *src = Rf_translateCharUTF8(s);
  char *dst = R_alloc(strlen(src) + 1, 1);
  strcpy(dst, src);
  return dst;
}

static const char **strings_copy(SEXP x) {
  int n = Rf_length(x);
  const char **out = (const char **)R_alloc(n + 1, sizeof(char *));
  for (int k = 0; k < n; k++) {
    if (STRING_ELT(x, k) == NA_STRING) {
      Rf_error("internal error: missing value in names or levels");
    }
    out[k] = string_copy(STRING_ELT(x, k));
  }
  return out;
}

static int is_flag(SEXP x) {
  return TYPEOF(x) == LGLSXP && Rf_length(x) == 1 &&
         LOGICAL(x)[0] != NA_LOGICAL;
}

static void unpack_spec(SEXP spec, RAttSpec *out) {
  SEXP names = list_elt(spec, "names");
  SEXP discrete = list_elt(spec, "discrete");
  SEXP ordered = list_elt(spec, "ordered");
  SEXP levels = list_elt(spec, "levels");
  SEXP class_levels = list_elt(spec, "class_levels");
  SEXP class_ordered = list_elt(spec, "class_ordered");
  SEXP target = list_elt(spec, "target");
  SEXP has_weights = list_elt(spec, "has_weights");

  int p = Rf_length(names);
  if (TYPEOF(names) != STRSXP || TYPEOF(discrete) != LGLSXP ||
      TYPEOF(ordered) != LGLSXP || TYPEOF(levels) != VECSXP ||
      Rf_length(discrete) != p || Rf_length(ordered) != p ||
      Rf_length(levels) != p || TYPEOF(class_levels) != STRSXP ||
      Rf_length(class_levels) < 1 || !is_flag(class_ordered) ||
      TYPEOF(target) != STRSXP || Rf_length(target) != 1 ||
      STRING_ELT(target, 0) == NA_STRING || !is_flag(has_weights)) {
    Rf_error("internal error: malformed attribute spec");
  }

  int *nlevels = (int *)R_alloc(p + 1, sizeof(int));
  const char ***lvls = (const char ***)R_alloc(p + 1, sizeof(char **));
  const char **att_names = strings_copy(names);

  for (int j = 0; j < p; j++) {
    nlevels[j] = 0;
    lvls[j] = NULL;
    if (LOGICAL(discrete)[j]) {
      SEXP lv = VECTOR_ELT(levels, j);
      if (TYPEOF(lv) != STRSXP) {
        Rf_error("internal error: levels for '%s' are not character",
                 att_names[j]);
      }
      nlevels[j] = Rf_length(lv);
      lvls[j] = strings_copy(lv);
    }
  }

  out->NPred = p;
  out->Names = att_names;
  out->Discrete = LOGICAL(discrete);
  out->Ordered = LOGICAL(ordered);
  out->NLevels = nlevels;
  out->Levels = lvls;
  out->NClass = Rf_length(class_levels);
  out->ClassLevels = strings_copy(class_levels);
  out->ClassOrdered = LOGICAL(class_ordered)[0];
  out->Target = string_copy(STRING_ELT(target, 0));
  out->HasWeights = LOGICAL(has_weights)[0];
}

static void unpack_cases(SEXP x, SEXP y, SEXP w, const RAttSpec *spec,
                         RCaseData *out) {
  if (TYPEOF(x) != VECSXP || Rf_length(x) != spec->NPred) {
    Rf_error("internal error: expected a list of %d columns", spec->NPred);
  }

  int n = -1;
  if (y != R_NilValue) {
    if (TYPEOF(y) != INTSXP) {
      Rf_error("internal error: the class must be integer codes");
    }
    n = Rf_length(y);
    const int *codes = INTEGER(y);
    for (int i = 0; i < n; i++) {
      if (codes[i] != NA_INTEGER && (codes[i] < 1 || codes[i] > spec->NClass)) {
        Rf_error("internal error: invalid class code %d", codes[i]);
      }
    }
  } else if (spec->NPred > 0) {
    n = Rf_length(VECTOR_ELT(x, 0));
  }
  if (n < 0) {
    Rf_error("internal error: cannot determine the number of cases");
  }

  const void **cols = (const void **)R_alloc(spec->NPred + 1, sizeof(void *));
  for (int j = 0; j < spec->NPred; j++) {
    SEXP col = VECTOR_ELT(x, j);
    if (Rf_length(col) != n) {
      Rf_error("internal error: column '%s' has %d values, expected %d",
               spec->Names[j], Rf_length(col), n);
    }
    if (spec->Discrete[j]) {
      if (TYPEOF(col) != INTSXP) {
        Rf_error("internal error: column '%s' must be integer codes",
                 spec->Names[j]);
      }
      const int *codes = INTEGER(col);
      for (int i = 0; i < n; i++) {
        if (codes[i] != NA_INTEGER &&
            (codes[i] < 1 || codes[i] > spec->NLevels[j])) {
          Rf_error("internal error: column '%s' has an invalid code %d",
                   spec->Names[j], codes[i]);
        }
      }
      cols[j] = codes;
    } else {
      if (TYPEOF(col) != REALSXP) {
        Rf_error("internal error: column '%s' must be double",
                 spec->Names[j]);
      }
      cols[j] = REAL(col);
    }
  }

  const double *wts = NULL;
  if (w != R_NilValue) {
    if (TYPEOF(w) != REALSXP || Rf_length(w) != n) {
      Rf_error("internal error: case weights must be %d doubles", n);
    }
    wts = REAL(w);
  }

  out->N = n;
  out->Cols = cols;
  if (y == R_NilValue) {
    out->Y = NULL;
  } else {
    out->Y = INTEGER(y);
  }
  out->W = wts;
}

static void unpack_costs(SEXP costs, const RAttSpec *spec, RCosts *out) {
  out->K = spec->NClass;
  out->M = NULL;
  if (costs == R_NilValue) {
    return;
  }
  if (TYPEOF(costs) != REALSXP ||
      Rf_length(costs) != spec->NClass * spec->NClass) {
    Rf_error("internal error: costs must be a %d x %d double matrix",
             spec->NClass, spec->NClass);
  }
  out->M = REAL(costs);
}

static SEXP make_string(const char *s) {
  return Rf_ScalarString(Rf_mkCharCE(s, CE_UTF8));
}

/* Copy a registered strbuf (model text) before the strbufs are removed */
static const char *copy_strbuf(const char *name) {
  STRBUF *sb = rbm_lookup(name);
  if (sb == NULL) {
    return "";
  }
  const char *src = strbuf_getall(sb);
  char *dst = R_alloc(strlen(src) + 1, 1);
  strcpy(dst, src);
  return dst;
}

SEXP C_c50_fit(SEXP spec, SEXP x, SEXP y, SEXP w, SEXP costs, SEXP subset,
               SEXP rules, SEXP utility, SEXP trials, SEXP winnow,
               SEXP sample, SEXP seed, SEXP noGlobalPruning, SEXP CF,
               SEXP minCases, SEXP fuzzyThreshold, SEXP earlyStopping,
               SEXP traceRows) {
  RAttSpec att_spec;
  RCaseData train;
  RCosts cost_matrix;
  int val; /* Used by setjmp/longjmp for implementing rbm_exit */
  const char *tree_string = "";
  const char *rules_string = "";

  if (RDATA_NA_INT != NA_INTEGER) {
    Rf_error("internal error: NA_INTEGER does not match RDATA_NA_INT");
  }

  unpack_spec(spec, &att_spec);
  if (y == R_NilValue) {
    Rf_error("internal error: the class is required to fit a model");
  }
  unpack_cases(x, y, w, &att_spec, &train);
  unpack_costs(costs, &att_spec, &cost_matrix);

  int c_subset = Rf_asLogical(subset);
  int c_rules = Rf_asLogical(rules);
  int c_utility = Rf_asInteger(utility);
  int c_trials = Rf_asInteger(trials);
  int c_winnow = Rf_asLogical(winnow);
  double c_sample = Rf_asReal(sample);
  int c_seed = Rf_asInteger(seed);
  int c_no_global = Rf_asInteger(noGlobalPruning);
  double c_cf = Rf_asReal(CF);
  int c_min_cases = Rf_asInteger(minCases);
  int c_fuzzy = Rf_asLogical(fuzzyThreshold);
  int c_early = Rf_asLogical(earlyStopping);

  initglobals();
  ResetTrace();

  int n_trace = 0;
  if (traceRows != R_NilValue) {
    n_trace = Rf_length(traceRows);
  }
  SEXP trace_wt = PROTECT(Rf_allocMatrix(REALSXP, n_trace, c_trials));
  SEXP trace_pred = PROTECT(Rf_allocMatrix(INTSXP, n_trace, c_trials));
  for (R_xlen_t j = 0; j < (R_xlen_t)n_trace * c_trials; j++) {
    REAL(trace_wt)[j] = NA_REAL;
    INTEGER(trace_pred)[j] = NA_INTEGER;
  }
  if (n_trace > 0) {
    NTrace = n_trace;
    TraceTrials = c_trials;
    TraceRow = (int *)R_alloc(n_trace, sizeof(int));
    TraceRec = (void **)R_alloc(n_trace, sizeof(void *));
    for (int k = 0; k < n_trace; k++) {
      TraceRow[k] = INTEGER(traceRows)[k] - 1;
      TraceRec[k] = NULL;
    }
    TraceWt = REAL(trace_wt);
    TracePred = INTEGER(trace_pred);
  }

  // The costs are passed as a matrix, so there is no cost text
  setglobals(c_subset, c_rules, c_utility, c_trials, c_winnow, c_sample,
             c_seed, c_no_global, c_cf, c_min_cases, c_fuzzy, c_early, "");

  rbm_removeall();
  FreeCases();
  setOf();

  /*
   * We need to initialize rbm_buf before calling any code that
   * might call exit/rbm_exit.
   */
  if ((val = setjmp(rbm_buf)) == 0) {
    RC50(&att_spec, &train, &cost_matrix);

    if (c_rules == 0) {
      tree_string = copy_strbuf("undefined.tree");
    } else {
      rules_string = copy_strbuf("undefined.rules");
    }
  } else {
    Rprintf("c50 code called exit with value %d\n", val - JMP_OFFSET);
  }

  const char *ostr = closeOf();
  char *output_string = R_alloc(strlen(ostr) + 1, 1);
  strcpy(output_string, ostr);

  FreeCases();
  rbm_removeall();
  initglobals();
  ResetTrace();

  SEXP out = PROTECT(Rf_allocVector(VECSXP, 5));
  SEXP out_names = PROTECT(Rf_allocVector(STRSXP, 5));
  SET_VECTOR_ELT(out, 0, make_string(tree_string));
  SET_VECTOR_ELT(out, 1, make_string(rules_string));
  SET_VECTOR_ELT(out, 2, make_string(output_string));
  SET_VECTOR_ELT(out, 3, trace_wt);
  SET_VECTOR_ELT(out, 4, trace_pred);
  SET_STRING_ELT(out_names, 0, Rf_mkChar("tree"));
  SET_STRING_ELT(out_names, 1, Rf_mkChar("rules"));
  SET_STRING_ELT(out_names, 2, Rf_mkChar("output"));
  SET_STRING_ELT(out_names, 3, Rf_mkChar("trace_weight"));
  SET_STRING_ELT(out_names, 4, Rf_mkChar("trace_pred"));
  Rf_setAttrib(out, R_NamesSymbol, out_names);
  UNPROTECT(4);
  return out;
}

SEXP C_c50_predict(SEXP spec, SEXP model, SEXP rules, SEXP costs, SEXP new_x,
                   SEXP trials) {
  RAttSpec att_spec;
  RCaseData cases;
  RCosts cost_matrix;
  int val; /* Used by setjmp/longjmp for implementing rbm_exit */

  if (TYPEOF(model) != STRSXP || Rf_length(model) != 1 ||
      STRING_ELT(model, 0) == NA_STRING) {
    Rf_error("internal error: the model must be a single string");
  }
  if (!is_flag(rules)) {
    Rf_error("internal error: `rules` must be TRUE or FALSE");
  }

  unpack_spec(spec, &att_spec);
  unpack_cases(new_x, R_NilValue, R_NilValue, &att_spec, &cases);
  unpack_costs(costs, &att_spec, &cost_matrix);
  int c_trials = Rf_asInteger(trials);
  int c_rules = LOGICAL(rules)[0];

  // The model is read through a strbuf; give it a private copy since
  // strbuf_create_full() does not copy its buffer.
  const char *mstr = string_copy(STRING_ELT(model, 0));
  if (strlen(mstr) == 0) {
    Rf_error("internal error: the model text is empty");
  }

  SEXP pred = PROTECT(Rf_allocVector(INTSXP, cases.N));
  SEXP conf = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t)cases.N *
                                                  att_spec.NClass));
  memset(INTEGER(pred), 0, sizeof(int) * cases.N);
  memset(REAL(conf), 0, sizeof(double) * cases.N * att_spec.NClass);

  initglobals();
  rbm_removeall();
  setOf();

  const char *model_name = "undefined.tree";
  if (c_rules) {
    model_name = "undefined.rules";
  }
  STRBUF *sb_model = strbuf_create_full((char *)mstr, strlen(mstr));
  if (rbm_register(sb_model, model_name, 0) < 0) {
    UNPROTECT(2);
    Rf_error("internal error: %s already exists", model_name);
  }
  setrules(c_rules);

  if ((val = setjmp(rbm_buf)) == 0) {
    RPredict(&att_spec, &cost_matrix, c_trials, &cases, INTEGER(pred),
             REAL(conf));
  }

  const char *ostr = closeOf();
  char *output_string = R_alloc(strlen(ostr) + 1, 1);
  strcpy(output_string, ostr);

  rbm_removeall();
  initglobals();

  SEXP out = PROTECT(Rf_allocVector(VECSXP, 3));
  SEXP out_names = PROTECT(Rf_allocVector(STRSXP, 3));
  SET_VECTOR_ELT(out, 0, pred);
  SET_VECTOR_ELT(out, 1, conf);
  SET_VECTOR_ELT(out, 2, make_string(output_string));
  SET_STRING_ELT(out_names, 0, Rf_mkChar("pred"));
  SET_STRING_ELT(out_names, 1, Rf_mkChar("confidence"));
  SET_STRING_ELT(out_names, 2, Rf_mkChar("output"));
  Rf_setAttrib(out, R_NamesSymbol, out_names);
  UNPROTECT(4);
  return out;
}

// Declare the type of each of the arguments to the c50 function
static R_NativePrimitiveArgType c50_t[] = {
    STRSXP,  // namesv
    STRSXP,  // datav
    STRSXP,  // costv
    LGLSXP,  // subset
    LGLSXP,  // rules
    INTSXP,  // utility
    INTSXP,  // trials
    LGLSXP,  // winnow
    REALSXP, // sample
    INTSXP,  // seed
    INTSXP,  // noGlobalPruning
    REALSXP, // CF
    INTSXP,  // minCases
    LGLSXP,  // fuzzyThreshold
    LGLSXP,  // early stopping
    STRSXP,  // treev
    STRSXP,  // rulesv
    STRSXP   // outputv
};

// Declare the type of each of the arguments to the predictions function
static R_NativePrimitiveArgType predictions_t[] = {
    STRSXP,  // casev
    STRSXP,  // namesv
    STRSXP,  // treev
    STRSXP,  // rulesv
    STRSXP,  // costv
    INTSXP,  // predv
    REALSXP, // confidencev
    INTSXP,  // trials
    STRSXP   // outputv
};

// Declare the c50 and predictions functions
static const R_CMethodDef cEntries[] = {
    {"C50", (DL_FUNC)&c50, 18, c50_t},
    {"predictions", (DL_FUNC)&predictions, 9, predictions_t},
    {NULL, NULL, 0}};

// Declare the .Call functions that take R vectors directly
static const R_CallMethodDef callEntries[] = {
    {"C_c50_fit", (DL_FUNC)&C_c50_fit, 18},
    {"C_c50_predict", (DL_FUNC)&C_c50_predict, 6},
    {NULL, NULL, 0}};

// Initialization function for this shared object
void R_init_C50(DllInfo *dll) {
  // Announce ourselves for testing
  // Rprintf("R_init_C50 called\n");

  // Register the functions "c50" and "predictions"
  R_registerRoutines(dll, cEntries, callEntries, NULL, NULL);

  // This should help prevent people from accidentally accessing
  // any of our global variables, or any functions that are not
  // intended to be called from R.  Only the functions "c50"
  // and "predictions" can be accessed, since they're the only ones
  // we registered.
  R_useDynamicSymbols(dll, FALSE);
}
