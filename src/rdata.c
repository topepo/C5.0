/*************************************************************************/
/*                                                                       */
/*  Load attributes, cases, and costs directly from R vectors.          */
/*                                                                       */
/*  The original program reads .names, .data, and .costs files.         */
/*  RDefineAtts replaces GetNames(), RLoadCases replaces GetData(), and */
/*  RSetCosts replaces GetMCosts(), so that no text has to be formatted */
/*  in R and parsed again here.  Everything else (model building,       */
/*  prediction) is shared with the file based code through C50Start()/ */
/*  C50Build(), StartData()/KeepCase()/FinishData(), SetMCost()/        */
/*  FinishMCosts(), and PredictLoadModel()/PredictOneCase().            */
/*                                                                       */
/*  The attribute layout matches what makeNamesFile() writes, so that   */
/*  both paths build the same model:                                    */
/*                                                                       */
/*    1            the class (ClassAtt)                                 */
/*    2..NPred+1   the predictors, in column order                      */
/*    NPred+2      "case weight" (CWtAtt), only if weights are given    */
/*                                                                       */
/*************************************************************************/

#include "defns.h"
#include "extern.h"

#include "rdata.h"
#include "redefine.h"
#include "transform.h"

/*************************************************************************/
/*                                                                       */
/* Set up the attribute globals that GetNames() would set.   */
/*                                                                       */
/* Names and values are copied as given: there is no escaping,  */
/* whitespace collapsing, length limit, reserved name, or special */
/* meaning for values such as "?" or "N/A".     */
/*                                                                       */
/*************************************************************************/

static void RDefineAtts(const RAttSpec *Spec)
/*          -----------  */
{
  Attribute Att, AttCeiling;
  int j, k, NLev;

  ErrMsgs = AttExIn = 0;
  LineNo = 0;
  MaxClass = ClassAtt = LabelAtt = CWtAtt = 0;

  MaxAtt = 1 + Spec->NPred;
  if (Spec->HasWeights) {
    MaxAtt++;
  }
  AttCeiling = MaxAtt + 1;

  AttName = AllocZero(AttCeiling, String);
  MaxAttVal = AllocZero(AttCeiling, DiscrValue);
  AttValName = AllocZero(AttCeiling, String *);
  SpecialStatus = AllocZero(AttCeiling, char);
  AttDef = AllocZero(AttCeiling, Definition);
  if (MODE == m_build) {
    AttDefUses = AllocZero(AttCeiling, Attribute *);
  }

  /*  Class.  As in ExplicitAtt(), the class has no "N/A" value, so
      its values are numbered 1..NClass  */

  ClassAtt = 1;
  AttName[ClassAtt] = strdup(Spec->Target);
  AttValName[ClassAtt] = AllocZero(Spec->NClass + 2, String);
  ForEach(k, 0, Spec->NClass - 1) {
    AttValName[ClassAtt][k + 1] = strdup(Spec->ClassLevels[k]);
  }
  MaxAttVal[ClassAtt] = Spec->NClass;

  if (Spec->ClassOrdered && MaxAttVal[ClassAtt] > 3) {
    SpecialStatus[ClassAtt] = ORDERED;
  }
  if (MODE == m_build && MaxAttVal[ClassAtt] > MaxDiscrVal) {
    MaxDiscrVal = MaxAttVal[ClassAtt];
  }

  /*  Predictors.  Discrete values are numbered as in ExplicitAtt():
      0 = unknown, 1 = "N/A", 2.. = the levels in order  */

  ForEach(j, 0, Spec->NPred - 1) {
    Att = j + 2;
    AttName[Att] = strdup(Spec->Names[j]);

    if (!Spec->Discrete[j]) {
      continue;
    }

    NLev = Spec->NLevels[j];
    AttValName[Att] = AllocZero(NLev + 3, String);
    AttValName[Att][1] = strdup("N/A");
    ForEach(k, 0, NLev - 1) {
      AttValName[Att][k + 2] = strdup(Spec->Levels[j][k]);
    }
    MaxAttVal[Att] = NLev + 1;

    /*  As in ExplicitAtt(), ordering needs at least three values  */

    if (Spec->Ordered[j] && MaxAttVal[Att] > 3) {
      SpecialStatus[Att] = ORDERED;
    }

    if (MODE == m_build && MaxAttVal[Att] > MaxDiscrVal) {
      MaxDiscrVal = MaxAttVal[Att];
    }
  }

  /*  Case weights are never used as a predictor  */

  if (Spec->HasWeights) {
    CWtAtt = MaxAtt;
    AttName[CWtAtt] = strdup("case weight");
    SpecialStatus[CWtAtt] |= SKIP;
  }

  /*  The class names are the values of the class attribute  */

  MaxClass = MaxAttVal[ClassAtt];
  ClassName = AttValName[ClassAtt];
  ClassName[0] = "?";
}

/*************************************************************************/
/*                                                                       */
/* Mark a value as unknown, as GetDataRec() does for "?"   */
/*                                                                       */
/*************************************************************************/

static void SetUnknown(DataRec DVec, Attribute Att)
/*          ----------  */
{
  DVal(DVec, Att) = UNKNOWN;
  if (SomeMiss) {
    SomeMiss[Att] = bintrue;
  }
}

/*************************************************************************/
/*                                                                       */
/* Store a continuous value: NA and NaN are unknown, and other  */
/* non-finite values are reported by CheckValue()    */
/*                                                                       */
/*************************************************************************/

static void SetContinuous(DataRec DVec, Attribute Att, double Value)
/*          -------------  */
{
  if (isnan(Value)) {
    SetUnknown(DVec, Att);
    return;
  }

  CVal(DVec, Att) = Value;
  CheckValue(DVec, Att);
}

/*************************************************************************/
/*                                                                       */
/* Fill the attribute values of case i, as GetDataRec() does for a */
/* line of a .data or .cases file      */
/*                                                                       */
/*************************************************************************/

static void FillCase(DataRec DVec, const RAttSpec *Spec, const RCaseData *Data,
                     CaseNo i)
/*          --------  */
{
  int j, Code;
  Attribute Att;

  /*  Class; unknown when predicting  */

  if (Data->Y && Data->Y[i] != RDATA_NA_INT) {
    DVal(DVec, ClassAtt) = Data->Y[i];
  } else {
    SetUnknown(DVec, ClassAtt);
  }

  ForEach(j, 0, Spec->NPred - 1) {
    Att = j + 2;

    if (Spec->Discrete[j]) {
      Code = ((const int *)Data->Cols[j])[i];
      if (Code == RDATA_NA_INT) {
        SetUnknown(DVec, Att);
      } else {
        DVal(DVec, Att) = Code + 1;
      }
    } else {
      SetContinuous(DVec, Att, ((const double *)Data->Cols[j])[i]);
    }
  }

  if (CWtAtt) {
    if (Data->W) {
      SetContinuous(DVec, CWtAtt, Data->W[i]);
    } else {
      SetUnknown(DVec, CWtAtt);
    }
  }

  Class(DVec) = XDVal(DVec, ClassAtt);
}

/*************************************************************************/
/*                                                                       */
/* Load cases into Case[0..MaxCase], as GetData() does for a file  */
/*                                                                       */
/*************************************************************************/

static void RLoadCases(const RAttSpec *Spec, const RCaseData *Data,
                       Boolean Train, Boolean AllowUnknownClass)
/*          ----------  */
{
  CaseNo i;
  DataRec DVec;

  StartData(Train, Data->N);

  ForEach(i, 0, Data->N - 1) {
    Case[MaxCase] = DVec = NewCase(); /* for error reporting */
    FillCase(DVec, Spec, Data, i);
    KeepCase(DVec, Train, AllowUnknownClass);
  }

  FinishData();
}

/*************************************************************************/
/*                                                                       */
/* Set the misclassification costs, as GetMCosts() does for a file */
/*                                                                       */
/*************************************************************************/

static void RSetCosts(const RCosts *Costs)
/*          ---------  */
{
  ClassNo Pred, Real;

  if (!Costs || !Costs->M) {
    return;
  }

  ForEach(Pred, 1, MaxClass) {
    ForEach(Real, 1, MaxClass) {
      SetMCost(Pred, Real, (float)Costs->M[(Pred - 1) + (Real - 1) * Costs->K]);
    }
  }
}

/*************************************************************************/
/*                                                                       */
/* Hold-out cases for sampled fits (see LoadTestData in rc50.c)  */
/*                                                                       */
/*************************************************************************/

static const RAttSpec *TestSpec = Nil;
static const RCaseData *TestData = Nil;

static void RLoadTestCases(void)
/*          --------------  */
{
  RLoadCases(TestSpec, TestData, binfalse, binfalse);
}

/*************************************************************************/
/*                                                                       */
/* Build a model; the counterpart of c50main()    */
/*                                                                       */
/*************************************************************************/

void RC50(const RAttSpec *Spec, const RCaseData *Train, const RCosts *Costs)
/*   ----  */
{
  double StartTime;

  StartTime = C50Start();

  RDefineAtts(Spec);
  fprintf(Of, T_ClassVar, AttName[ClassAtt]);

  NotifyStage(READDATA);
  Progress(-1.0);

  /*  Allocate space for SomeMiss[] and SomeNA[] */

  SomeMiss = AllocZero(MaxAtt + 1, Boolean);
  SomeNA = AllocZero(MaxAtt + 1, Boolean);

  RLoadCases(Spec, Train, bintrue, binfalse);
  fprintf(Of, TX_ReadData(MaxCase + 1, MaxAtt, FileStem));

  /*  Costs need the loaded cases for the two-class reweighting  */

  if (Costs && Costs->M) {
    RSetCosts(Costs);
    FinishMCosts();
  }

  /*  A sampled fit evaluates on the cases that were not sampled,
      which are read again from the same data  */

  TestSpec = Spec;
  TestData = Train;
  LoadTestData = RLoadTestCases;

  C50Build(StartTime);

  LoadTestData = Nil;
  TestSpec = Nil;
  TestData = Nil;
}

/*************************************************************************/
/*                                                                       */
/* Predict new cases from the registered model text; the   */
/* counterpart of rpredictmain()      */
/*                                                                       */
/*************************************************************************/

void RPredict(const RAttSpec *Spec, const RCosts *Costs, int Trials,
              const RCaseData *New, int *Pred, double *Conf)
/*   --------  */
{
  CaseNo i;
  DataRec Dummy, DVec;

  MODE = m_predict;

  RDefineAtts(Spec);

  /*  Set the costs before reading the model.  The model header
      has costs="1" when costs were used, and ReadHeader() then
      looks for a .costs file; none is registered here.  */

  RSetCosts(Costs);

  PredictLoadModel(Trials);

  LineNo = 0;

  ForEach(i, 0, New->N - 1) {
    /*  Allocate the case as PredictGetDataRec() does  */

    Dummy = AllocZero(MaxAtt + 2, AttValue);
    DVec = &Dummy[1];

    FillCase(DVec, Spec, New, i);
    PredictOneCase(DVec, i, Pred, Conf);

    PredictFreeLastCase(DVec);
  }
}
