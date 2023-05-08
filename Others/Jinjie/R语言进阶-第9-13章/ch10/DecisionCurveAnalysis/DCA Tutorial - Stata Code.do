
/************************************************
** Decision Curve Analysis for Binary Outcomes **
*************************************************/

/* Basic Data Set-up */
	cd "C:\Decision Curve Analysis"
	use "dca.dta", clear

/* Univariate Decision Curve Analysis */
	//Test whether family history is associated with cancer
	logit cancer famhistory
	//Run the decision curve: family history is coded as 0 or 1, i.e. a probability
	//so no need to specify the “prob” option
	dca cancer famhistory
	
	//Restricting Threshold probability
	dca cancer famhistory, xstop(0.35) xlabel(0(0.05)0.35)

/* Multivariable Decision Curve Analysis */
	//run the multivariable model 
	logit cancer marker age famhistory
	//save out predictions in the form of probabilities
	predict cancerpredmarker

	//Run decision curve analysis
	dca cancer cancerpredmarker famhistory, xstop(0.35) xlabel(0(0.05)0.35)

/* Evaluation of Published Models */
	//Use the coefficients from the Brown model
	g logodds_Brown = 0.75*(famhistory) + 0.26*(age) - 17.5
	//Convert to predicted probability
	g phat_Brown = invlogit(logodds_Brown)
	label var phat_Brown "Risk from Brown Model"
	//Run the decision curve
	dca cancer phat_Brown, xstop(0.35) xlabel(0(0.05)0.35)

/* Joint or Conditional Tests */
	//Create a variable for the strategy of treating only high risk patients
	//This will be 1 for treat and 0 for don’t treat
	g high_risk = risk_group=="high"
	label var high_risk "Treat Only High Risk Group"
	//Treat based on Joint Approach
	g joint = risk_group =="high" | cancerpredmarker > 0.15
	label var joint "Treat via Joint Approach"
	//Treat based on Conditional Approach
	g conditional = risk_group=="high" | /// 
	(risk_group == "intermediate" & cancerpredmarker > 0.15)
	label var conditional "Treat via Conditional Approach"

	//Run decision curve analysis
	dca cancer high_risk joint conditional, xstop(0.35) 

/* Incorporating Harms into Model Assessment */
	//the harm of measuring the marker is stored in a local
	local harm_marker = 0.0333
	//in the conditional test, only patients at intermediate risk 
	//have their marker measured
	g intermediate_risk = (risk_group=="intermediate")
	//harm of the conditional approach is proportion of patients who have the marker 
	//measured multiplied by the harm
	sum intermediate_risk
	local harm_conditional = r(mean)*`harm_marker' 
	//Run the decision curve
	dca cancer high_risk joint conditional, ///
		harm(0 `harm_marker' `harm_conditional') xstop(0.35) xlabel(0(0.05)0.35)

	
/* Saving out Net Benefit Values */
	//Run the decision curve and save out net benefit results
	//Specifying xby(.05) since we’d want 5% increments
	dca cancer marker, prob(no) xstart(0.05) xstop(0.35) xby(0.05) nograph ///
		saving("DCA Output marker.dta") replace
	//Load the data set with the net benefit results
	use "DCA Output marker.dta", clear
	//Calculate difference between marker and treat all
	//Our standard approach is to biopsy everyone so this tells us
	//how much better we do with the marker
	g advantage = marker - all 
	label var advantage "Increase in net benefit from using Marker model"

/* Interventions Avoided */
	//Load the original data
	use "dca.dta", clear
	//Run decision curve, specifying intervention
	dca cancer marker, prob(no) intervention xstart(0.05) xstop(0.35) 

/**************************************************
** Decision Curve Analysis for Survival Outcomes **
**************************************************/

/*Basic Data Set-up*/
	//Declaring survival time data: follow-up time variable is ttcancer 
	//and the event is cancer 
	stset ttcancer, f(cancer)

	//Run the cox model and save out baseline survival in the “surv_func” variable
	stcox age famhistory marker, basesurv(surv_func)
	//get linear predictor for calculation of risk
	predict xb, xb
	//Obtain baseline survival at 1.5 years = 18 months
	sum surv_func if _t <= 1.5
	//We want the survival closest to 1.5 years
	//This will be the lowest survival rate for all survival times =1.5
	local base = r(min)
	*Convert to a probability
	g pr_failure18 = 1 - `base'^exp(xb)
	label var pr_failure18 "Probability of Failure at 18 months"

	//Run the decision curve analysis (with a smoother)
	stdca pr_failure18, timepoint(1.5) xstop(0.5) smooth

/* Decision Curve Analysis with Competing Risks */

	//Create failure variable
	g status = 0
	replace status = 1 if cancer==1
	replace status = 2 if cancer==0 & dead==1

	//We declare our survival data with the new event variable 
	stset ttcancer, f(status=1)
	//Run the decision curve specifying the competing risk option
	stdca pr_failure18, timepoint(1.5) compet1(2) smooth xstop(.5)

	/*Plotting Kaplan Meier model and Competing Risk model*/
		//Kaplan Meier Model:
		stset ttcancer, f(cancer)
		//Run the decision curve saving out the net benefits
		stdca pr_failure18, timepoint(1.5) xstop(.5) nograph saving("km.dta", replace) 

		//Competing Risk Model: 
		stset ttcancer, f(status=1)
		stdca pr_failure18, timepoint(1.5) compet1(2) xstop(.5) nograph saving("cr.dta", replace) 

		use km.dta, clear
		//Sort by the threshold so that we can merge later
		sort threshold
		//Rename the variables so that we know they are the Kaplan Meier estimates
		rename pr_failure18 kmmodel
		label var kmmodel "Kaplan-Meier: Pr(Failure) at 1.5 years"
		rename all kmall
		label var kmall "Kaplan-Meier: Treat All"
		save kmsort.dta, replace

		use cr.dta, clear
		sort threshold
		//Rename the variables so that we know they are the Competing Risk estimates
		rename pr_failure18 crmodel
		label var crmodel "Competing Risk: Pr(Failure) at 1.5 years"
		rename all crall
		label var crall "Competing Risk: Treat All"
		merge 1:1 threshold using kmsort.dta

		//Plotting the curves
		twoway (line kmall crall threshold if kmall>-0.05 & ///
		crall > -0.05, sort) || (line kmmodel crmodel none threshold, ///
		sort ytitle("Net Benefit"))

/********************************************************
** Assessing Clinical Utility in a Case-Control Design **
********************************************************/
	//Load the original data
	use "dca.dta", clear
	//Use only the data from the case control study
	drop if casecontrol == 0
	//Create the model
	logit cancer famhistory age
	//Save out the linear predictor, rather than the probability
	predict xb, xb

	//The true risk stored in a local
	local true = 0.05	
	sum cancer
	//The observed risk, which is the mean of our data, is stored in a local
	local design = r(mean)	
	//The Bayes factor is stored in a local
	local Bayes=log((`true'/(1-`true'))/(`design'/(1-`design')))
	// We add the Bayes factor to the linear predictor
	replace xb=xb+`Bayes'

	//Convert to a probability
	g phat=invlogit(xb)
	//Run the decision curve
	dca2 phat phat, xstop(.35) xlabel(0(0.05)0.35)


	
	
/***************************
** Correction for Overfit **
****************************/
	//Load Dataset
	use "dca.dta", clear
	//To skip this optional loop used for running the cross validation multiple times 
	//either 1) change it to “forvalues i=1(1)200 {” or
	//2) omit this piece of line and take care to change any code which references “i” 
	forvalues i=1(1)200 {
		//Local macros to store the names of model.
		local prediction1 = "base"
		local prediction2 = "full"
		//Create variables to later store probabilities from each prediction model
		quietly g `prediction1'=.
		quietly g `prediction2'=.

		//Create a variable to be used to ‘randomize’ the patients.
		quietly g u = uniform()
		//Sort by the event to ensure equal number of patients with the event are in each 
		//group
		sort cancer u
		//Assign each patient into one of ten groups
		g group = mod(_n, 10) + 1

		//Loop through to run through for each of the ten groups
		forvalues j=1(1)10 {
			//First for the “base” model:
			//Fit the model excluding the jth group.
			quietly logit cancer age famhistory if group!=`j'
			//Predict the probability of the jth group. 
			quietly predict ptemp if group==`j'
			//Store the predicted probabilities of the jth group (that was not used in 
			//creating the model) into the variable previously created
			quietly replace `prediction1' = ptemp if group==`j'
			//Dropping the temporary variable that held predicted probabilities for all
			//patients
			drop ptemp
			//Likewise, for the second “final” model
			quietly logit cancer age famhistory marker if group!=`j'
			quietly predict ptemp if group==`j'
			quietly replace `prediction2' = ptemp if group==`j'
			drop ptemp
		}

		//Creating a temporary file to store the results of each of the iterations of our 
		//decision curve for the multiple the 10 fold cross validation
		//This step may omitted if the optional forvalues loop was excluded.
		tempfile dca`i'
		//Run decision curve, and save the results to the tempfile.
		//For those excluding the optional multiple cross validation, this decision curve 
		//(to be seen by excluding “nograph” and the results (saved under the name of your 
		//choosing) would be the decision curve corrected for overfit.
		quietly dca cancer `prediction1' `prediction2', xstop(.5) nograph ///
		saving("`dca`i''") 
		drop u group `prediction1' `prediction2'
	} //This closing bracket, ends the initial loop for the multiple cross validation. 
	//It is necessary also necessary for those who avoided the multiple cross validation 
	//by changing the value of the forvalues loop from 200 to 1*/

	//The following is only used for the multiple 10 fold cross validations. 
	use "`dca1'", clear 
	forvalues i=2(1)200 {
		//Append all values of the multiple cross validations into the first //file
		append using "`dca`i''"
	}
	//Calculate the average net benefit across all iterations of the multiple //cross validation
	collapse all none base full base_i full_i, by(threshold)
	save "Cross Validation DCA Output.dta", replace

	//Labeling the variables so that the legend will have the proper labels
	label var all "(Mean) Net Benefit: Treat All"
	label var none "(Mean) Net Benefit: Treat None"
	label var base "(Mean) Net Benefit: Base Model"
	label var full "(Mean) Net Benefit: Full Model"
	label var base_i "(Mean) Intervention: Base Model"
	label var full_i "{Mean) Intervention: Full Model"
	//Plotting the figure of all the net benefits.  
	twoway (line all threshold if all>-0.05, sort) || (line none base full threshold, sort)


