OPTIONS FORMDLIM=" ";
LIBNAME home "O:\Outcomes\Andrew\Methodology Work\Vickers dca function updates\SAS Tutorial\Data";

/*
PROGRAM: DCA Tutorial Final SAS Code
*/

*to get the same style graph as the Stata graphs shown in the tutorial, add this code to the top of your
SAS program file, and add these options to the %DCA macro command "vaxis=ORDER=(-0.05 to 0.15 by 0.05) LABEL=(ANGLE=90 "Net Benefit") MINOR=none, haxis=ORDER=(0 to 1 by 0.2) LABEL=("Threshold Probability") MINOR=none, legend=LABEL=none ACROSS=1 CBORDER=black";
%PUT _user_;
GOPTIONS RESET = ALL;

symbol1 i=join c=green;
symbol2 i=join c=red;
symbol3 i=join c=blue;
symbol4 i=join c=darkred;
symbol5 i=join c=gray;

*Set our directory;
*LIBNAME home "C:\Decision Curve Analysis";

*Source file to use DCA command;
OPTIONS MAUTOSOURCE SASAUTOS=(sasautos "O:\Outcomes\Andrew\Methodology Work\Vickers dca function updates\SAS Code");

*DATA SETUP;
/*The code in this file is set up in a different order than the code in the tutorial file. The data setup section
	of this file creates a final dataset "dca" in the home directory that includes all variables necessary to run
	all examples below.*/

*Use original data to create variable "cancerpredmarker";
PROC LOGISTIC DATA=home.origdca DESCENDING OUTMODEL=dcalog;
	MODEL cancer = marker age famhistory;
	*save out predictions in the form of probabilities;
	SCORE CLM OUT=dca(RENAME=(P_1=cancerpredmarker));
RUN;

DATA dca; SET dca;
	*use the coefficients from the Brown model to calculate log odds;
	logodds_Brown = 0.75*(famhistory) + 0.26*(age) - 17.5;

	*convert log odds from published model to predicted probability;
	phat_Brown = exp(logodds_Brown) / (1 + exp(logodds_Brown));
	LABEL phat_Brown="Brown Model";

	*Create variable for patients to be treated based on risk group, joint approach, conditional approach;
	*Treat high risk patients only;
	high_risk = (risk_group="high");
	LABEL high_risk="Treat Only High Risk Group";
	*Treat based on joint approach;
	joint = (risk_group="high") | (cancerpredmarker > 0.15);
	LABEL joint="Treat via Joint Approach";
	*Treat based on conditional approach;
	conditional = (risk_group="high") | (risk_group="intermediate" & cancerpredmarker > 0.15);
	LABEL conditional="Treat via Conditional Approach";
	
	*Identify intermediate risk patients, since only these patients have marker measured in the conditional test;
	intermediate_risk = (risk_group="intermediate");

RUN;

*DATA SETUP FOR CASE-CONTROL EXAMPLE;

*Create dataset for case-control example using only case control patients;
DATA cc_dca; SET home.origdca;
	IF casecontrol=0 THEN DELETE;
	ELSE OUTPUT;
RUN;

*Create the model and save out the linear predictor;
PROC LOGISTIC DATA=cc_dca DESCENDING;
	MODEL cancer = famhistory age;
	OUTPUT OUT=cc_dca XBETA=xb;
RUN;

*The true risk is stored as a macro variable;
%LET true = 0.05;

*The observed risk, which is the mean of our data, is stored in a macro variable;
PROC MEANS DATA=cc_dca;
	VAR cancer;
	OUTPUT OUT=risk MEAN=risk;
RUN;

DATA _NULL_; SET risk;
	CALL SYMPUT("design",risk);
RUN;

*The Bayes factor is stored as a macro variable;
%LET Bayes = (%SYSEVALF(&true.)/(1-%SYSEVALF(&true.)))/(%SYSEVALF(&design.)/(1-%SYSEVALF(&design.)));
%LET Bayes = %SYSEVALF(%SYSFUNC(log(&Bayes.)));

*We add the Bayes factor to the linear predictor;
*Convert to a probability;
DATA cc_dca; SET cc_dca;
	adj_xb = xb + &Bayes.;
	phat = exp(adj_xb)/(1+exp(adj_xb));
RUN;

*ANALYSIS / EXAMPLES;

*Univariate DCA using family history;
%DCA(data=dca, outcome=cancer, predictors=famhistory, graph=yes);

*Univariate DCA for family history, between 0-35%;
%DCA(data=dca, outcome=cancer, predictors=famhistory, graph=yes, xstop=0.35);

*Multivariable DCA using cancer probability (predicted from marker, age and family history), compared to family history alone;
%DCA(data=dca, outcome=cancer, predictors=cancerpredmarker famhistory, graph=yes, xstop=0.35);

*DCA to assess published model ("Brown model") using DCA;
%DCA(data=dca, outcome=cancer, predictors=phat_Brown, xstop=0.35);

*DCA to compare treatment for different risk groups: high risk, joint approach, conditional approach;
%DCA(data=dca, outcome=cancer, predictors=high_risk joint conditional, graph=yes, xstop=0.35);

*Store the harm of measuring the marker, calculate the proportion of patients who have the marker,
and calculate the harm of the conditional approach; 

%LET harm_marker = 0.0333;

PROC MEANS DATA=home.dca;
	VAR intermediate_risk;
	OUTPUT OUT=meanrisk MEAN=meanrisk;
RUN;

DATA _NULL_; SET meanrisk;
	CALL SYMPUT("meanrisk",meanrisk);
RUN;

%LET harm_conditional = %SYSEVALF(&meanrisk.*&harm_marker.);

*DCA to compare high risk, joint and conditional approaches using calculated harms;
%DCA(data=dca, outcome=cancer, predictors=high_risk joint conditional, harm=0 &harm_marker. &harm_conditional., xstop=0.35);

*DCA to compare high risk, joint and conditional approaches using calculated harms and save out net benefit for each threshold by 0.05;
%DCA(data=dca, outcome=cancer, predictors=marker, probability=no, xstart=0.05, xstop=0.35, xby=0.05, graph=no, out=home.dcamarker);

*DCA for interventions avoided;
%DCA(data=dca, outcome=cancer, predictors=marker, probability=no, intervention=yes, xstart=0.05, xstop=0.35);

*DCA using case-control design;
%DCA(data=cc_dca, out=test, outcome=phat, predictors=phat, xstop=0.35, probability=yes);


*TEN FOLD CROSS VALIDATION - CORRECTION FOR OVERFIT;

*In SAS, we will use a macro to perform the 10-fold crossvalidation (correction for overfit);
%MACRO CROSSVAL;

	*To skip the optional loop used for running the cross validation multiple times,
	either 1) change it to "%DO x = 1 %TO 1" or 2) omit this line of code and take
	care to change any code which references "&x.";
	%DO x = 1 %TO 200;

		*Load original data and create a variable to be used to 'randomize' patients;
		DATA dca_of; SET home.origdca;
			u = RAND("Uniform");
		RUN;

		*Sort by the event to ensure equal number of patients with the event are in each group;
		PROC SORT DATA=dca_of;
			BY cancer u;
		RUN;

		*Assign each patient into one of ten groups;
		DATA dca_of; SET dca_of;
			group=MOD(_n_,10) + 1;
		RUN;

		*Loop through to run through for each of the ten groups;
		%DO y = 1 %TO 10;

			*First for the "base" model, fit the model excluding the yth group.;
			PROC LOGISTIC DATA=dca_of OUTMODEL=base&y. DESCENDING NOPRINT;
				MODEL cancer = age famhistory;
				WHERE group ne &y.;
			RUN;

			*Put yth group into base test dataset;
			DATA basetest&y.; SET dca_of;
				WHERE group = &y.;
			RUN;

			*Apply the base model to the yth group and save the predicted probabilities of the yth group
			(that was not used in creating the model);
			PROC LOGISTIC INMODEL=base&y. NOPRINT;
				SCORE DATA=basetest&y. OUT=base_pr&y.;
			RUN;

			*Likewise, for the second "final" model, fit the model excluding the yth group;
			PROC LOGISTIC DATA=home.dca_of OUTMODEL=final&y. DESCENDING NOPRINT;
				MODEL cancer = age famhistory marker;
				WHERE group ne &y.;
			RUN;

			*Put yth group into final test dataset;
			DATA finaltest&y.; SET dca_of;
				WHERE group = &y.;
			RUN;

			*Apply the final model to the yth group and save the predicted probabilities of the yth group
			(that was not used in creating the model);
			PROC LOGISTIC INMODEL=final&y. NOPRINT;
				SCORE DATA=finaltest&y. OUT=final_pr&y.;
			RUN;

		%END;

		*Combine base model predictions for all 10 groups;
		DATA base_pr(RENAME=(P_1=base_pred));
			SET base_pr1-base_pr10;
		RUN;

		*Combine final model predictions for all 10 groups;
		DATA final_pr(RENAME=(P_1=final_pred));
			SET final_pr1-final_pr10;
		RUN;

		*Sort and merge base model and final model prediction data together;
		PROC SORT DATA=base_pr NODUPKEYS;
			BY patientid;
		RUN;

		PROC SORT DATA=final_pr NODUPKEYS;
			BY patientid;
		RUN;

		DATA all_pr;
			MERGE base_pr final_pr;
			BY patientid;
		RUN;

		*Run decision curve and save out results;
		*If you are running the crossvalidation only once, rename the "out=" file as "dca" rather than "dca&x.";
		*For those excluding the optional multiple cross validation, this decision curve (to be seen by excluding
		"graph=no") and the results (saved under the name of your choosing) would be the decision curve corrected for overfit;
		%DCA(data=all_pr, out=dca&x., outcome=cancer, predictors=base_pred final_pred, graph=no, xstop=0.5);

	*This "%END" statement ends the initial loop for the multiple cross validation. It is also necessary for those who
	avoided the multiple cross validation by changing the value in the DO loop from 200 to 1;
	%END;
		
	*The following is only used for the multiple 10 fold cross validation.;

	*Append all values of the multiple cross validation;
	DATA _NULL_;
		CALL SYMPUTX("n",&x.-1);
	RUN;

	DATA allcv_pr;
		SET dca1-dca&n.;
	RUN;

	*Calculate the average net benefit across all iterations of the multiple cross validation;
	PROC MEANS DATA=allcv_pr NOPRINT;
		CLASS threshold;
		VAR all base_pred final_pred;
		OUTPUT OUT=minfinal MEAN=all base_pred final_pred;
	RUN;

	*Save out average net benefit and label variables so that the plot legend will have the proper labels.;
	DATA allcv_pr(KEEP=base_pred final_pred all none threshold);
		SET allcv_pr(DROP=base_pred final_pred all) minfinal;
		LABEL all="(Mean) Net Benefit: Treat All";
		LABEL none="(Mean) Net Benefit: Treat None";
		LABEL base_pred="(Mean) Net Benefit: Base Model";
		LABEL final_pred="(Mean) Net Benefit: Full Model";
	RUN;

%MEND CROSSVAL;

*Run the crossvalidation macro;
%CROSSVAL;

*Plotting the figure of all the net benefits;
PROC GPLOT DATA=allcv_pr;
	axis1 ORDER=(-0.05 to 0.15 by 0.05) LABEL=(ANGLE=90 "Net Benefit") MINOR=none;
	axis2 ORDER=(0 to 0.5 by 0.1) LABEL=("Threshold Probability") MINOR=none;
	legend LABEL=NONE ACROSS=1 CBORDER=BLACK;

	PLOT all*threshold
		 none*threshold
		 base_pred*threshold
		 final_pred*threshold /
		 VAXIS=axis1
		 HAXIS=axis2
		 LEGEND=legend OVERLAY;
		 SYMBOL INTERPOL=JOIN;
RUN;
QUIT;
