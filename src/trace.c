#include "trace.h"

int NTrace = 0;
int TraceTrials = 0;
int *TraceRow = 0;
void **TraceRec = 0;
double *TraceWt = 0;
int *TracePred = 0;

void ResetTrace(void) {
  NTrace = TraceTrials = 0;
  TraceRow = 0;
  TraceRec = 0;
  TraceWt = 0;
  TracePred = 0;
}
