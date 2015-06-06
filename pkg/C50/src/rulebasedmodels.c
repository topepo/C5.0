#include <setjmp.h>

#include "defns.h"
#include "extern.h"
#include "rulebasedmodels.h"
#include "redefine.h"
#include "strbuf.h"

/* Global variables defined in update.d */
extern int Stage;
extern FILE *Uf;

/* Used to implement rbm_exit */
jmp_buf rbm_buf;

/* Don't want to include R.h, which has conflicts with cubist headers */
extern void Rprintf(const char *, ...);

/*
 * Reset all global variables to their initial value
 */
void initglobals(void)
/*   ---------- */
{
    VERBOSITY=0;	/* verbosity level (0 = none) */
    TRIALS=1;		/* number of trees to be grown */
    FOLDS=10;		/* crossvalidation folds */
    UTILITY=0;		/* rule utility bands */
    PRUNEM=0;
    SUBSET=0;		/* subset tests allowed */
    BOOST=0;		/* boosting invoked */
    EARLYSTOPPING=0;     /* let C5 check for effective boosting */
    PROBTHRESH=0;	/* to use soft thresholds */
    RULES=0;		/* rule-based classifiers */
    XVAL=0;		/* perform crossvalidation */
    NOCOSTS=0;		/* ignoring costs */
    WINNOW=0;		/* attribute winnowing */
    GLOBAL=1;		/* use global pruning for trees */

    /* This was set in C5's main(), but we do it here. 
     This value may be over-ridden by the R seed option */
    KRInit = time(0) & 07777;
    
    /* Added for sample.c */
    RULESUSED=0;    	/* list applicable rules */

    MINITEMS=2;		/* minimum cases each side of a cut */
    LEAFRATIO=0;	/* leaves per case for boosting */

    CF=0.25;		/* confidence limit for tree pruning */
    SAMPLE=0.0;		/* sample training proportion */

    LOCK=false;		/* sample locked */


/*************************************************************************/
/*									 */
/*		Attributes and data					 */
/*									 */
/*************************************************************************/

    ClassAtt=0;		/* attribute to use as class */
    LabelAtt=0;		/* attribute to use as case ID */
    CWtAtt=0;		/* attribute to use for case weight */

    AvCWt;		/* average case weight */

    ClassName=0;	/* class names */
    AttName=0;		/* att names */
    AttValName=0;	/* att value names */

    IgnoredVals=0;	/* values of labels and atts marked ignore */
    IValsSize=0;	/* size of above */
    IValsOffset=0;	/* index of first free char */

    MaxAtt;		/* max att number */
    MaxClass;		/* max class number */
    MaxDiscrVal=3;	/* max discrete values for any att */
    MaxLabel=0;		/* max characters in case label */
    LineNo=0;		/* input line number */
    ErrMsgs=0;		/* errors found */
    AttExIn=0;		/* attribute exclusions/inclusions */
    TSBase=0;		/* base day for time stamps */

    MaxAttVal=0;	/* number of values for each att */

    SpecialStatus=0;	/* special att treatment */

    AttDef=0;		/* definitions of implicit atts */
    AttDefUses=0;	/* list of attributes used by definition */

    SomeMiss=Nil;	/* att has missing values */
    SomeNA=Nil;		/* att has N/A values */
    Winnowed=0;		/* atts have been winnowed */

    ClassThresh=0;	/* thresholded class attribute */

    MaxCase=-1;		/* max data case number */

    Case=0;		/* data cases */

    SaveCase=0;

    FileStem="undefined";

/*************************************************************************/
/*									 */
/*		Trees							 */
/*									 */
/*************************************************************************/

    Raw=0;		/* unpruned trees */
    Pruned=0;		/* pruned trees */
    WTree=0;		/* winnow tree */

    Confidence;		/* set by classify() */
    SampleFrac=1;	/* fraction used when sampling */
    Vote=0;		/* total votes for classes */
    BVoteBlock=0;	/* boost voting block */
    MCost=0;		/* misclass cost [pred][real] */
    NCost=0;		/* normalised MCost used for rules */
    WeightMul=0;	/* prior adjustment factor */

    MostSpec=0;	/* most specific rule for each class */

    UnitWeights=1;	/* all weights are 1.0 */
    CostWeights=0;	/* reweight cases for costs */

    Trial;		/* trial number for boosting */
    MaxTree=0;		/* max tree grown */

    TrialPred=0;	/* predictions for each boost trial */

    ClassFreq=0;	/* ClassFreq[c] = # cases of class c */
    DFreq=0;		/* DFreq[a][c*x] = Freq[][] for attribute a */

    Gain=0;		/* Gain[a] = info gain by split on att a */
    Info=0;		/* Info[a] = max info from split on att a */
    EstMaxGR=0;	/* EstMaxGR[a] = est max GR from folit on a */
    ClassSum=0;	/* class weights during classification */

    Bar=0;		/* Bar[a]  = best threshold for contin att a */

    GlobalBaseInfo;	/* base information before split */
    Bell=0;		/* table of Bell numbers for subsets */

    Tested=0;		/* Tested[a] = att a already tested */

    Subset=0;		/* Subset[a][s] = subset s for att a */
    Subsets=0;		/* Subsets[a] = no. subsets for att a */

    GEnv;		/* environment block */

/*************************************************************************/
/*									 */
/*		Rules							 */
/*									 */
/*************************************************************************/

    Rule=0;		/* current rules */

    NRules;		/* number of rules */
    RuleSpace;		/* space currently allocated for rules */

/* Added for sample.c */
    RulesUsed=Nil; 	/* list of all rules used */

    RuleSet=0;		/* rulesets */

    Default=0;		/* default class associated with ruleset or
			   boosted classifier */

    Fires=Nil;		/* Fires[r][*] = cases covered by rule r */
    CBuffer=Nil;	/* buffer for compressing lists */

    CovBy=Nil;		/* entry numbers for Fires inverse */
    List=Nil;		/* temporary list of cases or rules */

    AttTestBits;	/* average bits to encode tested attribute */
    BranchBits=0;	/* ditto attribute value */
    AttValues=0;	/* number of attribute values in the data */
    PossibleCuts=0;	/* number of thresholds for an attribute */

    LogCaseNo=0;	/* LogCaseNo[i] = log2(i) */
    LogFact=0;		/* LogFact[i] = log2(i!) */

    UtilErr=0;		/* error by utility band */
    UtilBand=0;		/* last rule in each band */
    UtilCost=0;		/* cost ditto */


/*************************************************************************/
/*									 */
/*		Misc							 */
/*									 */
/*************************************************************************/

    KRInit=0;		/* KRandom initializer for SAMPLE */
    Now=0;		/* current stage */

    TRf=0;		/* file pointer for tree and rule i/o */
    Fn[0] = '\0';	/* file name */

    Of=0;		/* output file */
    MODE = m_build;

    modelfilesinit();
}

/*
 * Set global variables in preparation for creating a model
 */
void setglobals(int subset, int rules, int utility, int trials,int prunem, int winnow,
                double sample, int seed, int noGlobalPruning, double cf,
                int minCases, int fuzzyThreshold, int earlyStopping,
                char *costv)
{
    // I don't think there is a need for the NOCOSTS variable
    // in the C50 package, so the costv argument is ignored,
    // and the NOCOSTS global variable is always left at the
    // default value.

    SUBSET = subset != 0 ? true : false;                      /* Logical */
    RULES = rules != 0 ? true : false;                        /* Logical */
    UTILITY = utility;                                       /* Int */
    TRIALS = trials;                                         /* Int */
    PRUNEM = prunem;
    BOOST = trials > 1 ? true : false;                        /* Logical */   
    EARLYSTOPPING = earlyStopping != 0 ? true : false;        /* Logical */
    WINNOW = winnow != 0 ? true : false;                      /* Logical */
    SAMPLE = sample;                                         /* Real */
    KRInit = seed;                                           /* Int */
    GLOBAL = noGlobalPruning != 0 ? false : true;             /* Logical */
    CF = cf;                                                 /* Real */
    MINITEMS = minCases;                                     /* Int */
    PROBTHRESH = fuzzyThreshold != 0 ? true : false;          /* Logical */
}

void setrules (int val) {
    RULES = val;
}

void setOf()
{
    // XXX Experimental
    Of = rbm_fopen("rulebasedmodels.stdout", "w");
}

char *closeOf()
{
    if (Of) {
        rbm_fclose(Of);
        return strbuf_getall((STRBUF *) Of);
    } else {
        return "";
    }
}


/*
 * The jmp_buf needs to be initialized before calling this.
 * Also, this must be called further down the stack from the
 * code that called setjmp to initialize rbm_buf.
 * That's why we can't have a function that initialize the
 * jmp_buf, but must use a macro instead.
 */
void rbm_exit(int status)
{
    /* This doesn't return */
    longjmp(rbm_buf, status + JMP_OFFSET);
}
