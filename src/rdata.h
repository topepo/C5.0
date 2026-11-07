#ifndef _RDATA_H_
#define _RDATA_H_

/*
 * Plain-C description of data handed over from R.  This header must not
 * include any R headers: rdata.c includes the C5.0 headers, whose
 * Free/Realloc/Calloc macros clash with R's.  top.c unpacks the SEXPs
 * into these structs.
 */

/* Missing value marker for integer codes; equal to R's NA_INTEGER */
#define RDATA_NA_INT (-2147483647 - 1)

typedef struct {
  int NPred;               /* number of predictors */
  const char **Names;      /* NPred predictor names, unescaped */
  const int *Discrete;     /* NPred flags: 1 = discrete, 0 = continuous */
  const int *Ordered;      /* NPred flags: 1 = ordered discrete */
  const int *NLevels;      /* NPred level counts (0 for continuous) */
  const char ***Levels;    /* NPred arrays of level labels (NULL if cont.) */
  int NClass;              /* number of classes */
  const char **ClassLevels; /* NClass class labels */
  int ClassOrdered;        /* 1 = the class is an ordered factor */
  const char *Target;      /* name of the class attribute */
  int HasWeights;          /* 1 = add a trailing "case weight" attribute */
} RAttSpec;

typedef struct {
  int N;                   /* number of cases */
  const void **Cols;       /* NPred columns: double* or int* (codes 1..k) */
  const int *Y;            /* class codes 1..NClass, or NULL (prediction) */
  const double *W;         /* case weights, or NULL */
} RCaseData;

typedef struct {
  int K;                   /* number of classes */
  const double *M;         /* K x K, column-major; rows = predicted class,
                              columns = true class.  NULL = no costs */
} RCosts;

void RC50(const RAttSpec *Spec, const RCaseData *Train, const RCosts *Costs);
void RPredict(const RAttSpec *Spec, const RCosts *Costs, int Trials,
              const RCaseData *New, int *Pred, double *Conf);

#endif
