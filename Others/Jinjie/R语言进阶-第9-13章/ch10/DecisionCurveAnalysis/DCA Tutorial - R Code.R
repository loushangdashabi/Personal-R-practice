#################################################
## Decision Curve Analysis for Binary Outcomes ##
#################################################

# Basic Data Set-up
  #Set our directory
  setwd("C:\\Decision Curve Analysis")
  #Source file to use dca command
  source("dca.R")
  data.set = read.delim("dca.txt", header=TRUE, sep="\t")
  attach(data.set)

# Univariate Decision Curve Analysis
  #Test whether family history is associated with cancer
  summary(glm(cancer ~ famhistory, family=binomial(link="logit")))
  #Run the decision curve: family history is coded as 0 or 1, i.e. a probability
  #so no need to specify the “prob” option
  dca(data=data.set, outcome="cancer", predictors="famhistory")
  
  #Restricting Threshold Probability
  dca(data=data.set, outcome="cancer", predictors="famhistory", xstop=0.35) 
  
# Multivariable Decision Curve Analysis
  #run the multivariable model
  model = glm(cancer ~ marker + age + famhistory, family=binomial(link="logit"))
  #save out predictions in the form of probabilities
  data.set$cancerpredmarker = predict(model, type="response")

  #Run decision curve
  dca(data=data.set, outcome="cancer", predictors=c("cancerpredmarker","famhistory"), 
  xstop=0.35)

# Evaluation of Published Models
  #Use the coefficients from the Brown model
  logodds_Brown = 0.75*(famhistory)+0.26*(age)-17.5
  #Convert to predicted probability
  data.set$phat_Brown = exp(logodds_Brown)/(1+exp(logodds_Brown))
  #Run the decision curve analysis
  dca(data=data.set, outcome="cancer", predictors="phat_Brown", xstop=0.35)

# Joint or Conditional Tests
  #Create a variable for the strategy of treating only high risk patients
  #This will be 1 for treat and 0 for don’t treat
  data.set$high_risk = ifelse(risk_group=="high", 1, 0)
  #Treat based on Joint Approach
  data.set$joint = ifelse(risk_group=="high" | cancerpredmarker > 0.15, 1, 0)
  #Treat based on Conditional Approach
  data.set$conditional = ifelse(risk_group=="high" | (risk_group=="intermediate" & 
  cancerpredmarker > 0.15), 1, 0)

  #Run decision curve analysis
  dca(data=data.set, outcome="cancer", predictors=c("high_risk", "joint", 
  "conditional"), xstop=0.35)

#Incorporating Harms into Model Assessment
  #the harm of measuring the marker is stored in a scalar
  harm_marker = 0.0333
  #in the conditional test, only patients at intermediate risk 
  #have their marker measured
  intermediate_risk = ifelse(risk_group=="intermediate", c(1), c(0))
  #harm of the conditional approach is proportion of patients who have the marker 
  #measured multiplied by the harm
  harm_conditional = mean(intermediate_risk)*harm_marker
  #Run the decision curve
  dca(data=data.set, outcome="cancer", predictors=c("high_risk", "joint",
  "conditional"), harm=c(0, harm_marker, harm_conditional),
  xstop=0.35)

# Saving out Net Benefit Values
  #Run the decision curve, specify xby=0.05 since we want 5% increments
  output = dca(data=data.set, outcome="cancer", predictors="marker", probability=F, 
  xstart=0.05, xstop=0.35, xby=0.05, graph=F) 
  #Calculate difference between marker and treat all
  #Our standard approach is to biopsy everyone so this tells us
  #how much better we do with the marker
  output$net.benefit$advantage=output$net.benefit$marker-output$net.benefit$all 
  #To view the table, simply call on the variable that it’d stored in
  output

# Interventions Avoided
  dca(data=data.set, outcome="cancer", predictors="marker", probability=FALSE, 
  intervention=TRUE, xstart=0.05, xstop=0.35)

###################################################
## Decision Curve Analysis for Survival Outcomes ##
###################################################

# Basic Data Set-up
  #Source file to use stdca command
  source("stdca.R")
  #Creates a survival object with time to event variable as ttcancer and the event is 
  #cancer. 
  Srv = Surv(data.set$ttcancer, data.set$cancer)


  #Load survival library
  library(survival)
  #Run the cox model
  coxmod = coxph(Srv ~ age + famhistory + marker, data=data.set)
  #the probability of failure is calculated by subtracting the probability of 
  #survival from 1. 
  data.set$pr_failure18 = c(1- (summary(survfit(coxmod,
  newdata=data.set), times=1.5)$surv))

  #Run the decision curve analysis (with a smoother)
  stdca(data=data.set, outcome="cancer", ttoutcome="ttcancer", timepoint=1.5, 
  predictors="pr_failure18", xstop=0.5, smooth=TRUE)

# Decision Curve Analysis with Competing Risks

  #Create failure variable
  data.set$status = data.set$cancer + data.set$dead * (data.set$cancer==0) * 2

  #We declare the survival data variables within the stdca function and run 
  #the decision curve analysis
  stdca(data=data.set, outcome="status", ttoutcome="ttcancer", timepoint=1.5, 
  predictors="pr_failure18", cmprsk=TRUE, smooth=TRUE, xstop=0.5)

  #Plotting Kaplan Meier model and Competing Risk model
    #Kaplan Meier Model
    km = stdca(data=data.set, outcome="cancer", ttoutcome="ttcancer", timepoint=1.5, 
    predictors="pr_failure18", xstop=0.5)
  
    #Competing Risk Model
    cr = stdca(data=data.set, outcome="status", ttoutcome="ttcancer", timepoint=1.5, 
    predictors="pr_failure18", cmprsk=T, xstop=0.5)
  
    #Plotting the curves
    plot(km$net.benefit.threshold, km$net.benefit.none, type = "l", lwd=2, 
    xlim=c(0,.50), ylim=c(-.05, .20), xlab = "Threshold Probability", 
    ylab = "Net Benefit")
    lines(km$net.benefit$threshold, km$net.benefit$all, type="l", col=8, lwd=2)
    lines(km$net.benefit$threshold, cr$net.benefit$all, type="l", col=8, lwd=2, lty=2)
    lines(km$net.benefit$threshold, km$net.benefit$pr_failure18, type="l", col=1)
    lines(cr$net.benefit$threshold, cr$net.benefit$pr_failure18, type="l", col = 1, 
    lty=2)
    legend("topright", cex=0.8, legend=c("None", "KM All", "CR All", "KM Model", "CR 
    Model"), col=c(17, 8, 8, 1, 1), lwd=c(2, 2, 2, 1, 1), lty=c(1, 1, 2, 1, 2))

#########################################################                            
## Assessing Clinical Utility in a Case-Control Design ##
#########################################################
                            
  #Use only the data from the case control study
  casecontrol = subset(data.set, casecontrol ==1)
  #Create the model
  model = glm(cancer~ age + famhistory, family=binomial(link="logit"), 
  data=casecontrol)
  #Save out the linear predictor
  xb = predict(model)

  #The true risk is stored in a scalar
  true = 0.05
  #The observed risk, the mean of our data, is stored in a scalar
  design = mean(casecontrol$cancer)
  #The Bayes factor is stored in a scalar
  Bayes = log((true/(1-true))/(design/(1-design)))
  #We add the Bayes factor to the linear predictor
  xb = xb+Bayes

  #Convert to a probability
  casecontrol$phat = exp(xb)/(1+exp(xb))
  #Run the decision curve
  dca(data=casecontrol, outcome="phat", predictors="phat", xstop=0.35)

############################
## Correction for Overfit ##
############################
  #To skip this optional loop used for running the cross validation multiple times 
  #either 1) change it to “for (i in 1:1) {” or
  #2) omit this piece of line and take care to change any code which references “i” 
  for (i in 1:200) {
    #Create variables to later store probabilities from each prediction model
    data.set$pred1=NA
    data.set$pred2=NA
  
    #Create a variable to be used to ‘randomize’ the patients. 
    u = runif(750,0,1)
    #Sort by event to ensure equal number of patients with the event are in each group
    data.set = data.set[order(data.set$cancer, u),]
    #Assign each patient into one of ten groups
    data.set$group = (seq(1:dim(data.set)[1]) %% 10)+1
  
    #As R predicts probabilities based on the data that was used to create the model, 
    #we will need to calculate the probabilities ourselves.
    #Set the constant term as 1. 
    #Create variables to store the log of the odds Xß, and hold the model coefficients
    data.set$xb1=NA
    data.set$xb2=NA
    data.set$cons=1
    mod1=NULL
    mod2=NULL
    #Loop through to run through for each of the ten groups
    for (j in 1:10) {
      #First for the “base” model:
        #Predict the probability of the jth group. 
        mod1[[j]] = glm(cancer~age+famhistory, data=subset(data.set,group!=j),
        family=binomial)
        #Calculating and storing Xß
        data.set$xb1[data.set$group==j]=data.matrix(data.set[data.set$group==j,c("cons","age",
        "famhistory")]) %*% mod1[[j]]$coef 

      #Likewise, for the second “final” model
        mod2[[j]] = glm(cancer~age+famhistory+marker, data=subset(data.set,group!=j),
        family=binomial)   
        data.set$xb2[data.set$group==j]=data.matrix(data.set[data.set$group==j,c("cons","age",
        "famhistory", "marker")]) %*% mod2[[j]]$coef 
    }
    #Now calculate the probability of having the event for each model
    data.set$pred1 = 1/(1+exp(-1*(data.set$xb1)))
    data.set$pred2 = 1/(1+exp(-1*(data.set$xb2)))
  
    #Running the decision curve.
    #For those excluding the optional multiple cross validation, this decision curve 
    #to be seen by excluding “graph=FALSE” and the results (saved under the name of your
    #choosing) would be the decision curve corrected for overfit.
    output = dca(data=data.set, outcome="cancer", predictors=c("pred1", "pred2"), 
    graph=FALSE, xstop=.5)
    
    #The following is only used for the multiple 10 fold cross validations.   
    if(i==1){
      dcaoutput = output$net.benefit
    } else{
      #Append all result of the multiple cross validations into the first
      #file
      dcaoutput = rbind(dcaoutput, output$net.benefit)
    }
  } #This closing bracket, ends the initial loop for the multiple cross validation. 
  #It is also necessary for those who avoided the multiple cross validation by 
  #changing the value of the for loop from 200 to 1
  
  #Only used for the multiple cross validation. Calculate the average net
  #benefit across all iterations.
  data=aggregate(dcaoutput, by=list(dcaoutput$threshold), mean)[-1]

  #Plotting the aces and “Treat None” Model
  plot(data$threshold, data$none, type="l", xlim=c(0, 0.50), ylim=c(-0.05, 0.15),
  lwd=2, xlab="Threshold Probability", ylab="Net Benefit")
  #Plotting the “Treat All” Model
  lines(data$threshold, data$all, type="l", col=8, lwd=2)
  #Plotting the “Base” (pred1: only using age and family history) Model
  lines(data$threshold, data$pred1, type="l", col=2, lwd=2)
  #Plotting the “Full” (pred2: using the marker, age and family history) Model
  lines(data$threshold, data$pred2, type="l", col=1, lwd=2)
  # Adding a legend to distinguish each of the models.
  legend("topright", cex=0.8, legend=c("(Mean) Net Benefit: Treat None", "(Mean) Net
  Benefit: Treat All", "(Mean) Net Benefit: Base Model", "(Mean) Net Benefit:
  Full Model"), col=c(17, 8, 2, 1), lwd=c(2, 2, 2, 2))

