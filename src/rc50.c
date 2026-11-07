/*************************************************************************/
/*                                                                       */
/*  Copyright 2010 Rulequest Research Pty Ltd.                           */
/*                                                                       */
/*  This file is part of Cubist GPL Edition, a single-threaded version   */
/*  of Cubist release 2.07.                                              */
/*                                                                       */
/*  Cubist GPL Edition is free software: you can redistribute it and/or  */
/*  modify it under the terms of the GNU General Public License as       */
/*  published by the Free Software Foundation, either version 3 of the   */
/*  License, or (at your option) any later version.                      */
/*                                                                       */
/*  Cubist GPL Edition is distributed in the hope that it will be        */
/*  useful, but WITHOUT ANY WARRANTY; without even the implied warranty  */
/*  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     */
/*  GNU General Public License for more details.                         */
/*                                                                       */
/*  You should have received a copy of the GNU General Public License    */
/*  (gpl.txt) along with Cubist GPL Edition.  If not, see                */
/*                                                                       */
/*      <http://www.gnu.org/licenses/>.                                  */
/*                                                                       */
/*************************************************************************/

/*************************************************************************/
/*                                                                       */
/* Main routine, C5.0       */
/* ------------------       */
/*                                                                       */
/*************************************************************************/

#include "defns.h"
#include "extern.h"
#include <signal.h>

#include <sys/time.h>
#include <unistd.h>

#include "redefine.h"
#include "transform.h"

/*  When cases come from R vectors rather than a .data file (rdata.c),
    the hold-out cases of a sampled fit cannot be re-read from a file.
    RC50() then sets this hook to load them; it is Nil otherwise and
    is reset by initglobals().  */

void (*LoadTestData)(void) = Nil;

/*************************************************************************/
/*                                                                       */
/* The work of c50main() is split so that the R interface in  */
/* rdata.c can load attributes and cases directly and then share */
/* everything else:        */
/*                                                                       */
/*   C50Start - print the header and start the clock    */
/*   C50Build - costs, model construction, and evaluation   */
/*                                                                       */
/*************************************************************************/

double C50Start(void)
/*     --------  */
{
  /* The original C code set the seed here in main(). The c50
   function in top.c calls setglobals(), which sets the seed.
   If KRInit is created here, it will over-write the value
   specificed in R
   */

  // KRInit = time(0) & 07777;

  PrintHeader("");

  if (UTILITY && BOOST) {
    fprintf(Of, T_UBWarn);
  }

  return ExecTime();
}

void C50Build(double StartTime)
/*   --------  */
{
  FILE *F = Nil;
  Boolean HaveTest = binfalse;
  Attribute Att;

  /*  Check whether case weight attribute appears  */

  if (CWtAtt) {
    fprintf(Of, T_CWtAtt);
  }

  /*  Costs come from a .costs file, or were set directly by RC50()  */

  if (!NOCOSTS && (F = GetFile(".costs", "r"))) {
    GetMCosts(F);
  }
  if (MCost) {
    fprintf(Of, T_ReadCosts, FileStem);
  }

  /*  Note any attribute exclusions/inclusions  */

  if (AttExIn) {
    fprintf(Of, "%s", (AttExIn == -1 ? T_AttributesOut : T_AttributesIn));

    ForEach(Att, 1, MaxAtt) {
      if (Att != ClassAtt && Att != CWtAtt &&
          (StatBit(Att, SKIP) > 0) == (AttExIn == -1)) {
        fprintf(Of, "    %s\n", AttName[Att]);
      }
    }
  }

  /*  Build decision trees  */

  if (!BOOST) {
    TRIALS = 1;
  }

  InitialiseTreeData();
  if (RULES) {
    RuleSet = AllocZero(TRIALS + 1, CRuleSet);
  }

  if (WINNOW) {
    NotifyStage(WINNOWATTS);
    Progress(-MaxAtt);
    WinnowAtts();
  }

  if (XVAL) {
    CrossVal();
  } else {
    ConstructClassifiers();

    /*  Evaluation  */

    fprintf(Of, T_EvalTrain, MaxCase + 1);

    NotifyStage(EVALTRAIN);
    Progress(-TRIALS * (MaxCase + 1.0));

    Evaluate(CMINFO | USAGEINFO);

    F = Nil;
    if (SAMPLE && LoadTestData) {
      HaveTest = bintrue;
    } else if ((F = GetFile((SAMPLE ? ".data" : ".test"), "r"))) {
      HaveTest = bintrue;
    }

    if (HaveTest) {
      NotifyStage(READTEST);
      fprintf(Of, "\n");

      FreeData();
      if (F) {
        GetData(F, binfalse, binfalse);
      } else {
        LoadTestData();
      }

      fprintf(Of, T_EvalTest, MaxCase + 1);

      NotifyStage(EVALTEST);
      Progress(-TRIALS * (MaxCase + 1.0));

      Evaluate(CMINFO);
    }
  }

  fprintf(Of, T_Time, ExecTime() - StartTime);

#ifdef VerbOpt
  Cleanup();
#endif
}

int c50main(void)
/*  -------  */
{
  double StartTime;
  FILE *F;
  CaseNo SaveMaxCase;

  StartTime = C50Start();

  /*  Get information on training data  */

  if (!(F = GetFile(".names", "r")))
    Error(NOFILE, "", "");
  GetNames(F);

  if (ClassAtt) {
    fprintf(Of, T_ClassVar, AttName[ClassAtt]);
  }

  NotifyStage(READDATA);
  Progress(-1.0);

  /*  Allocate space for SomeMiss[] and SomeNA[] */

  SomeMiss = AllocZero(MaxAtt + 1, Boolean);
  SomeNA = AllocZero(MaxAtt + 1, Boolean);

  /*  Read data file  */

  if (!(F = GetFile(".data", "r")))
    Error(NOFILE, "", "");
  GetData(F, bintrue, binfalse);
  fprintf(Of, TX_ReadData(MaxCase + 1, MaxAtt, FileStem));

  if (XVAL && (F = GetFile(".test", "r"))) {
    SaveMaxCase = MaxCase;
    GetData(F, binfalse, binfalse);
    fprintf(Of, TX_ReadTest(MaxCase - SaveMaxCase, FileStem));
  }

  C50Build(StartTime);

  return 0;
}
