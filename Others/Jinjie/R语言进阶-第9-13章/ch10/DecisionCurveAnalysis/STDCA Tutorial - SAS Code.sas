OPTIONS FORMDLIM=" ";
LIBNAME home "O:\Outcomes\Andrew\Methodology Work\Vickers dca function updates\SAS Tutorial\Data";
OPTIONS MAUTOSOURCE SASAUTOS=(sasautos "O:\Outcomes\Andrew\Methodology Work\Vickers dca function updates\SAS Code");

%PUT _user_;
GOPTIONS RESET = ALL;

symbol1 i=join c=green;
symbol2 i=join c=red;
symbol3 i=join c=blue;
symbol4 i=join c=darkred;
symbol5 i=join c=gray;

/* PROGRAM: STDCA Tutorial Final SAS Code */

*DATA SETUP;
/*The code in this file is set up in a different order than the code in the tutorial file. The data setup section
	of this file creates a final dataset "dca" in the home directory that includes all variables necessary to run
	all examples below.*/

*Creates a separate variable “t” for time so that “ttcancer” is not overwritten;
DATA stdca; SET home.origdca(RENAME=(ttcancer=_t));
	ttcancer = _t;
RUN;

*Run the Cox model;
PROC PHREG DATA=stdca;
	MODEL _t*cancer(0) = age famhistory marker;
	BASELINE OUT=baseline COVARIATES=stdca SURVIVAL=surv_func / NOMEAN METHOD=pl;
RUN;

*the probability of failure at 1.5 years is calculated by subtracting the probability of survival from 1;
PROC SQL NOPRINT UNDO_POLICY=none;
	CREATE TABLE base_surv2 AS
	SELECT DISTINCT
		patientid, age, famhistory, marker, 1-min(surv_func) AS pr_failure18
	FROM baseline (WHERE=(_t<=1.5))
	GROUP BY patientid, age, famhistory, marker
	;

	*merge survival estimates with original data;
	CREATE TABLE stdca AS
	SELECT A.*, B.pr_failure18
	FROM stdca A
		LEFT JOIN base_surv2 B
		ON (A.patientid=B.patientid) AND (A.age=B.age) AND (A.famhistory=B.famhistory) AND (A.marker=B.marker);
	;
QUIT;

DATA stdca; SET stdca;
	LABEL pr_failure18="Probability of Failure at 18 months";
	status = 0;
	IF cancer=1 THEN status=1;
	ELSE IF cancer=0 & dead=1 THEN status=2;
RUN;

*ANALYSES / EXAMPLES;

*DCA for cancer within 18 months using probability of failure predicted from age, family history, and marker;
%STDCA(data=stdca, out=survivalmult, outcome=cancer, ttoutcome=ttcancer, timepoint=1.5, predictors=pr_failure18, xstop=0.5);

*Run the decision curve analysis specifying the competing risks option;
%STDCA(data=stdca, outcome=status, ttoutcome=ttcancer, timepoint=1.5, predictors=pr_failure18, competerisk=yes, xstop=0.5);

*Run the decision curve analysis without competing risks option to output Kaplan Meier estimates;
%STDCA(data=stdca, out=km, outcome=cancer, ttoutcome=ttcancer, timepoint=1.5, predictors=pr_failure18, xstop=0.5);

*Run the decision curve analysis with ccompeting risks option to output competing risks estimates;
%STDCA(data=stdca, out=cr, outcome=status, ttoutcome=ttcancer, timepoint=1.5, predictors=pr_failure18, competerisk=yes, xstop=0.5);

*Sort by threshold variable;
PROC SORT DATA=km OUT=kmsort;
	BY threshold;
RUN;

*Rename the variables so that we know they are the Kaplan Meier estimates;
DATA kmsort; SET kmsort(RENAME=(pr_failure18=kmmodel all=kmall));
	LABEL kmmodel="Kaplan-Meier: Pr(Failure) at 1.5 years";
	LABEL kmall="Kaplan-Meier: Treat All";
RUN;

*Sort by threshold variable;
PROC SORT DATA=cr OUT=crsort;
	BY threshold;
RUN;

*Rename the variables so that we know they are the Competing Risk estimates;
DATA crsort; SET crsort(RENAME=(pr_failure18=crmodel all=crall));
	LABEL crmodel="Competing Risk: Pr(Failure) at 1.5 years";
	LABEL crall="Competing Risk: Treat All";
RUN;

*Merge Kaplan-Meier and Competing Risk data using threshold probabilities;
DATA crsort;
	MERGE kmsort crsort;
	BY threshold;
RUN;


*create graph (decision curve) with treat none, treat all, Kaplan-Meier method, and Competing Risks method;
PROC GPLOT DATA=crsort;
	axis1 ORDER=(-0.05 to 0.2 by 0.05) LABEL=(ANGLE=90 "Net Benefit") MINOR=none;
	axis2 ORDER=(0.0 to 0.5 by 0.1) LABEL=("Threshold Probability") MINOR=none;

	legend1 LABEL=none ACROSS=1 DOWN=5 POSITION=(bottom center outside) CBORDER=black
	VALUE=("KM: Treat All" "CR: Treat All" "Pr(Failure) at 1.5 Years (KM)" "Pr(Failure) at 1.5 Years (CR)" "None");

	PLOT kmall*threshold
		 crall*threshold
		 kmmodel*threshold
		 crmodel*threshold
		 none*threshold / OVERLAY VAXIS=axis1 HAXIS=axis2 LEGEND=legend1;
		 SYMBOL INTERPOL=JOIN;
RUN;
QUIT;

