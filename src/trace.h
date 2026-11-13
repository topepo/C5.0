#ifndef _TRACE_H_
#define _TRACE_H_

/*  Demo-only state for recording boosting case weights of selected
    training rows; see ConstructClassifiers() and C_c50_fit()  */

extern int NTrace;      /* number of traced rows (0 = tracing off) */
extern int TraceTrials; /* columns allocated in TraceWt/TracePred */
extern int *TraceRow;   /* 0-based R row indices of traced rows */
extern void **TraceRec; /* DataRec of each traced row, or 0 if not kept */
extern double *TraceWt; /* NTrace x TraceTrials weights, column-major */
extern int *TracePred;  /* NTrace x TraceTrials predicted class codes */

void ResetTrace(void);

#endif
