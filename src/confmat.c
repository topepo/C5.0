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
/*            */
/* Routine for printing confusion matrices and attribute usage  */
/* -----------------------------------------------------------  */
/*            */
/*************************************************************************/

#include "defns.h"
#include "extern.h"

#include "redefine.h"
#include "transform.h"

void PrintConfusionMatrix(CaseNo *ConfusionMat)
/*   --------------------  */
{
  int Row, Col, Entry, EntryWidth = 10000;

  /*  For more than 20 classes, use summary instead  */

  if (MaxClass > 20) {
    PrintErrorBreakdown(ConfusionMat);
    return;
  }

  /*  Find maximum entry width in chars  */

  ForEach(Row, 1, MaxClass) {
    ForEach(Col, 1, MaxClass) {
      EntryWidth = Max(EntryWidth, ConfusionMat[Row * (MaxClass + 1) + Col]);
    }
  }

  EntryWidth = floor(Log(EntryWidth + 100.0) / Log(10.0)) + 2;

  /*  Print the heading, then each row  */

  fprintf(Of, "\n\n\t");
  ForEach(Col, 1, MaxClass) {
    fprintf(Of, "%*s(%c)", EntryWidth - 3, " ", 'a' + Col - 1);
  }

  fprintf(Of, "    <-" T_classified_as "\n\t");
  ForEach(Col, 1, MaxClass) {
    fprintf(Of, "%*.*s", EntryWidth, EntryWidth - 2, "----------");
  }
  fprintf(Of, "\n");

  ForEach(Row, 1, MaxClass) {
    fprintf(Of, "\t");
    ForEach(Col, 1, MaxClass) {
      if ((Entry = ConfusionMat[Row * (MaxClass + 1) + Col])) {
        fprintf(Of, " %*d", EntryWidth - 1, Entry);
      } else {
        fprintf(Of, "%*s", EntryWidth, " ");
      }
    }
    fprintf(Of, "    (%c): " T_class " %s\n", 'a' + Row - 1, ClassName[Row]);
  }
}

void PrintErrorBreakdown(CaseNo *ConfusionMat)
/*   -------------------  */
{
  CaseNo *TruePos, *FalsePos, *FalseNeg, Entry;
  int Row, Col, EntryWidth = 100000, ClassWidth = 5;

  TruePos = AllocZero(MaxClass + 1, CaseNo);
  FalsePos = AllocZero(MaxClass + 1, CaseNo);
  FalseNeg = AllocZero(MaxClass + 1, CaseNo);

  ForEach(Row, 1, MaxClass) {
    ForEach(Col, 1, MaxClass) {
      Entry = ConfusionMat[Row * (MaxClass + 1) + Col];

      if (Col == Row) {
        TruePos[Row] += Entry;
      } else {
        FalseNeg[Row] += Entry;
        FalsePos[Col] += Entry;
      }
    }

    EntryWidth = Max(EntryWidth, TruePos[Row] + FalseNeg[Row]);
    ClassWidth = Max(ClassWidth, strlen(ClassName[Row]));
  }

  EntryWidth = floor(Log(EntryWidth + 100.0) / Log(10.0)) + 2;

  /*  Print heading (tricky spacing) */

  fprintf(Of, "\n\n\t  %-*s %*s %*s %*s\n\t  %*s %*s %*s %*s\n", ClassWidth,
          "Class", EntryWidth, "Cases", EntryWidth, "False", EntryWidth,
          "False", ClassWidth, "", EntryWidth, "", EntryWidth, "Pos",
          EntryWidth, "Neg");
  fprintf(Of, "\t  %-*s %*s %*s %*s\n", ClassWidth, "-----", EntryWidth,
          "-----", EntryWidth, "-----", EntryWidth, "-----");

  ForEach(Row, 1, MaxClass) {
    fprintf(Of, "\t  %-*s %*d %*d %*d\n", ClassWidth, ClassName[Row],
            EntryWidth, TruePos[Row] + FalseNeg[Row], EntryWidth, FalsePos[Row],
            EntryWidth, FalseNeg[Row]);
  }

  Free(TruePos);
  Free(FalsePos);
  Free(FalseNeg);
}

void PrintUsageInfo(CaseNo *Usage)
/*   --------------  */
{
  Attribute Att, Best;
  float Tests;
  Boolean First = bintrue;
  double varUsage;

  Tests = Max(1, MaxCase + 1);

  while (bintrue) {
    Best = 0;

    ForEach(Att, 1, MaxAtt) {
      if (Usage[Att] > Usage[Best])
        Best = Att;
    }

    /* MK edit; lower the bar for printing attribute usage
       so that there is more consistency between which predictors
      were used in the tree and shown in the table */
    if (!Best || Usage[Best] < 0.00001 * Tests)
      break;

    if (First) {
      fprintf(Of, T_Usage);
      First = binfalse;
    }

    /* MK edit; print a percentage with 2 decimal places for
       higher resolution on the numbers
       Also, I eliminated adding 0.5 to everything below. There
       is likely some really good reason to do this, but I think
       the above line with Tests = Max(1, MaxCase+1); should
       eliminate the chances of divide by zero. We'll see! */

    varUsage = (100 * Usage[Best]) / Tests;
    if (varUsage < 100) {
      if (varUsage >= 10) {
        fprintf(Of, "\t %3.2f%%\t%s\n", varUsage, AttName[Best]);
      } else {
        fprintf(Of, "\t  %3.2f%%\t%s\n", varUsage, AttName[Best]);
      }
    } else {
      fprintf(Of, "\t%3.2f%%\t%s\n", varUsage, AttName[Best]);
    }
    Usage[Best] = 0;
  }
}
