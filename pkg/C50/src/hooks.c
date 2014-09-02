/*************************************************************************/
/*									 */
/*	Source code for use with See5/C5.0 Release 2.09			 */
/*	-----------------------------------------------			 */
/*		      Copyright RuleQuest Research 2012			 */
/*									 */
/*	This code is provided "as is" without warranty of any kind,	 */
/*	either express or implied.  All use is at your own risk.	 */
/*									 */
/*************************************************************************/

#include "defns.h"
#include "extern.h"

/* ReadName was identical to getnames.c/ReadName */

/* modified getnames.c/GetNames to recognize predict mode */

/* modified getnames.c/Explicitatt to recognize predict mode */

/* Which was identical to getnames.c/Which */

/* InChar was identical to getnames.c/InChar */

/* moved GetDataRec to getdata.c/PredictGetDataRec */

/* StoreIVal was identical to getdata.c/StoreIVal */

/* CheckValue was identical to getdata.c/CheckValue */

/* ImplicitAtt was identical to implicitatt.c/ImplicitAtt */

/* ReadDefinition was identical to implicitatt.c/ReadDefinition" */

/* Append was identical to implictatt.c/Append */

/* Expression was identical to implicitatt.c/Expression */

/* Conjunct was identical to implicitatt.c/Conjunct */

/* SExpression was identical to implicitatt.c/SExpression */

/* AExpression was identical to implicitatt.c/AExpression */

/* Term was identical to implicitatt.c/Term */

/* Factor was identical to implicitatt.c/Factor */

/* Primary was identical to implicitatt.c/Primary */

/* Atom was identical to implicitatt.c/Atom */

/* Find was identical to implicitatt.c/Find */

/* FindOne was identical to implicitatt.c/FindOne */

/* FindAttName was identical to implicitatt.c/FindAttName */

/* DefSyntaxError was identical to implicitatt.c/DefSyntaxError */

/* DefSemanticsError was identical to implicitatt.c/DefSemanticsError */

/* Dump was identical to implicitatt.c/Dump */

/* DumpOp was identical to implicitatt.c/DumpOp */

/* UPdateTStack was identical to implicitatt.c/UpdateTStack */

/*EvaluateDef was identical to implicitatt.c/EvaluateDef */

/* moved ReadFilePrefix to modelfiles.c/PredictReadFilePrefix */

/* moved ReadHeader to modelfiles.c/PredictReadHeader */

/* Integrated GetTree into  modelfiles.c/GetTree */

/* Instead of InTree, use modelfiles.c/InTree superset */

/* integrated GetRules into modelfiles.c/GetRules */

/* InRules was identical to modelfiles.c/InRules */

/* InRule was identical to modelfiles.c/InRule */

/* InCondition was identical to modelfiles.c/InCondition */

/* ConstructRuleTree was identical to ruletree.c/ConstructRuleTree */

/* SetTestIndex was identical to ruletree.c/SetTestIndex */

/* GrowRT was identical to ruletree.c/GrowRT */

/* DesiredOutcome was identical to ruletree.c/DesiredOutcome */

/* SelectTest was identical to ruletree.c/SelectTest */

/* ReadProp was identical to ruletree.c/ReadProp */

/* RemoveQuotes was identical to ruletree.c/RemoveQuotes */

/* MakeSubset was identical to ruletree.c/MakeSubset */

/* moved BinRecoverDiscreteName to modelfiles.c */

/* moved BinInTree to modelfiles.c */

/* moved BinInRules to modelfiles.c */

/* moved StreamIn to modelfiles.c */

/* Leaf was identical to trees.c/Leaf */

/* moved GetMCosts to mcost.c/PredictGetMCosts */

/* moved TreeClassify to classify.c/PredictTreeClassify */

/* moved FollowAllBranches to classify.c/PredictFollowAllBranches */

/* moved FindLeaf to classify.c/PredictFindLeaf */

/* moved FindLeafGen to classify.c */

/* moved RuleClassify to classify.c/PredictRuleClassify */

/* FindOutcome was identical to classify.c/FindOutcome */

/* Satisfies was identical to classify.c/Satisfies */

/* Matches was identical to classify.c/Matches */

/* CheckActiveSpace was identical to classify.c/CheckActiveSpace */

/* MarkActive was identical to classify.c/MarkActive */

/* moved BoostClassify to classify.c/PredictBoostClassify */

/* moved SelectClass to classify.c/PredictSelectClass */

/* moved SelectClassGen to classify.c */

/* moved MisclassCost to classify.c */

/* moved Classify to classify.c/PredictClassify */

/* moved Interpolate to classify.c/PredictInterpolate */

/* GetFile was identical to utility.c/GetFile */

/* integrated CheckFile into modelfiles.c/CheckFile */

/* moved ProcessOption to utility.c/PredictProcesssOption */

/* modified utility.c/Pmalloc to be identical to Pmalloc */

/* modified utility.c/Prealloc to be identical to Prealloc */

/* modified utility.c/Pcalloc to be identical to Pcalloc */

/* modified utility.c/Error to include functionality from Error when in predict
   mode */

/* Denominator was identical to utility.c/Denominator */

/* GetInt was identical to utility.c/GetInt */

/* DateToDay was identical to utility.c/DateToDay */

/* TimeToSecs was identical to utility.c/TimeToSecs */

/* SetTSBase was identical to utility.c/SetTSBase */

/* TStampToMins was identical to utility.c/TStampToMins */

/* moved FreeLastCase to utility.c/PredictFreeLastCase */

/* moved FreeGlobals to utility.c */

/* moved Predict to utility.c */

/* modified getnames.c/FreeNames to recognize predict mode */

/* FreeTree was identical to trees.c/FreeTree */

/* FreeRule was identical to rules.c/FreeRule */

/* FreeRuleTree was identical to ruletree.c/FreeRuleTree */

/* FreeRules was identical to rules.c/FreeRules */

/* FreeVector was identical to rules.c/FreeVector */
