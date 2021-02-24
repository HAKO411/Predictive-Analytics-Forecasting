database<-read.csv2(file="quebec.csv", header=TRUE)

library(apollo)								# run apollo package
apollo_initialise()

#set some controls

#indicate the name (in quotes) of the column in the data which contains the identifer variable
#for individual decision makers. For our data set that is "ID"


apollo_control=list(modelName="Model 2",
                    modelDescr="Some Description",indivID="obs")


#Define name and starting values for the coefficients to be estimated

apollo_beta=c(asc_ge              = 0,
              asc_deo              = 0,
              asc_dee		           = 0,
              asc_oo		           = 0,
              asc_oe		           = 0,
              asc_ee		           = 0,
              asc_we		           = 0,
              asc_wee		           = 0,
              
              asc_income_ge              = 0,
              asc_income_deo              = 0,
              asc_income_dee		           = 0,
              asc_income_oo		           = 0,
              asc_income_oe		           = 0,
              asc_income_ee		           = 0,
              asc_income_we		           = 0,
              asc_income_wee		           = 0,
              
              beta_OC	           = 0,
              beta_FC	           = 0,
              beta_FC_income 	           = 0
              
)


#all coefficients may be altered, none is fixed

apollo_fixed=NULL


#check if you have defined everything necessary  

apollo_inputs = apollo_validateInputs()


apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)			 ### Attach inputs and detach after
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 ### function exit		
  
  P = list()								 ### Create list of probabilities P
  
  V = list()								 ### List of utilities
  V[['gg']]		= 		                               beta_OC * op_cost.1 + beta_FC * fix_cost.1 + beta_FC_income * fix_cost.1*income
  V[['ge']] 	=   asc_ge	+ asc_income_ge*income + beta_OC * op_cost.2 + beta_FC * fix_cost.2 + beta_FC_income * fix_cost.2*income
  V[['deo']] 	=   asc_deo	+ asc_income_deo*income + beta_OC * op_cost.3 + beta_FC * fix_cost.3 + beta_FC_income * fix_cost.3*income
  V[['dee']] 	=   asc_dee	+ asc_income_dee*income + beta_OC * op_cost.4 + beta_FC * fix_cost.4 + beta_FC_income * fix_cost.4*income
  V[['oo']] 	=   asc_oo	+ asc_income_oo*income + beta_OC * op_cost.5 + beta_FC * fix_cost.5 + beta_FC_income * fix_cost.5*income
  V[['oe']]  	=   asc_oe	+ asc_income_oe*income + beta_OC * op_cost.6 + beta_FC * fix_cost.6 + beta_FC_income * fix_cost.6*income
  V[['ee']]  	=   asc_ee	+ asc_income_ee*income + beta_OC * op_cost.7 + beta_FC * fix_cost.7 + beta_FC_income * fix_cost.7*income
  V[['we']]  	=   asc_we	+ asc_income_we*income + beta_OC * op_cost.8 + beta_FC * fix_cost.8 + beta_FC_income * fix_cost.8*income
  V[['wee']] 	=   asc_wee	+ asc_income_wee*income + beta_OC * op_cost.9 + beta_FC * fix_cost.9 + beta_FC_income * fix_cost.9*income
  
  mnl_settings = list(						      							### Define settings for model 
    alternatives = c(gg=1, ge=2, deo=3, dee=4, oo=5, oe=6, ee=7, we=8, wee=9),					### component	
    avail        =  list(gg=avail.1, ge=avail.2, deo=avail.3, dee=avail.4, oo=avail.5,
                         oe=avail.6, ee=avail.7, we=avail.8, wee=avail.9),
    choiceVar    = choice,
    V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)	 ### Compute probabilities using model
  
  #P = apollo_panelProd(P, apollo_inputs, functionality)	 ### Take product across observation
  ### for same ID
  
  P = apollo_prepareProb(P, apollo_inputs, functionality)	 ### Prepare and return outputs of function
  
  return(P)
}


Model2 = apollo_estimate(apollo_beta,
                         apollo_fixed,
                         apollo_probabilities,
                         apollo_inputs)

apollo_modelOutput(Model2)

#Remove an alternative from Model 2
# IIA Test


#1. Check which alternative to remove
head(database)

#2. We need to remove the info of the database for which choice is alternative nº9
database_bu<-database
database <-subset(database, choice!=9)

library(apollo)								# run apollo package
apollo_initialise()

#set some controls

#indicate the name (in quotes) of the column in the data which contains the identifer variable
#for individual decision makers. For our data set that is "ID"


apollo_control=list(modelName="Model 3",
                    modelDescr="Some Description",indivID="obs")


#Define name and starting values for the coefficients to be estimated

apollo_beta=c(asc_ge              = 0,
              asc_deo              = 0,
              asc_dee		           = 0,
              asc_oo		           = 0,
              asc_oe		           = 0,
              asc_ee		           = 0,
              asc_we		           = 0,
              
              asc_income_ge              = 0,
              asc_income_deo              = 0,
              asc_income_dee		           = 0,
              asc_income_oo		           = 0,
              asc_income_oe		           = 0,
              asc_income_ee		           = 0,
              asc_income_we		           = 0,
              
              beta_OC	           = 0,
              beta_FC	           = 0,
              beta_FC_income 	           = 0
              
)


#all coefficients may be altered, none is fixed

apollo_fixed=NULL


#check if you have defined everything necessary  

apollo_inputs = apollo_validateInputs()


apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)			 ### Attach inputs and detach after
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 ### function exit		
  
  P = list()								 ### Create list of probabilities P
  
  V = list()								 ### List of utilities
  V[['gg']]		= 		                               beta_OC * op_cost.1 + beta_FC * fix_cost.1 + beta_FC_income * fix_cost.1*income
  V[['ge']] 	=   asc_ge	+ asc_income_ge*income + beta_OC * op_cost.2 + beta_FC * fix_cost.2 + beta_FC_income * fix_cost.2*income
  V[['deo']] 	=   asc_deo	+ asc_income_deo*income + beta_OC * op_cost.3 + beta_FC * fix_cost.3 + beta_FC_income * fix_cost.3*income
  V[['dee']] 	=   asc_dee	+ asc_income_dee*income + beta_OC * op_cost.4 + beta_FC * fix_cost.4 + beta_FC_income * fix_cost.4*income
  V[['oo']] 	=   asc_oo	+ asc_income_oo*income + beta_OC * op_cost.5 + beta_FC * fix_cost.5 + beta_FC_income * fix_cost.5*income
  V[['oe']]  	=   asc_oe	+ asc_income_oe*income + beta_OC * op_cost.6 + beta_FC * fix_cost.6 + beta_FC_income * fix_cost.6*income
  V[['ee']]  	=   asc_ee	+ asc_income_ee*income + beta_OC * op_cost.7 + beta_FC * fix_cost.7 + beta_FC_income * fix_cost.7*income
  V[['we']]  	=   asc_we	+ asc_income_we*income + beta_OC * op_cost.8 + beta_FC * fix_cost.8 + beta_FC_income * fix_cost.8*income

  
  mnl_settings = list(						      							### Define settings for model 
    alternatives = c(gg=1, ge=2, deo=3, dee=4, oo=5, oe=6, ee=7, we=8),					### component	
    avail        =  list(gg=avail.1, ge=avail.2, deo=avail.3, dee=avail.4, oo=avail.5,
                         oe=avail.6, ee=avail.7, we=avail.8),
    choiceVar    = choice,
    V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)	 ### Compute probabilities using model
  
  #P = apollo_panelProd(P, apollo_inputs, functionality)	 ### Take product across observation
  ### for same ID
  
  P = apollo_prepareProb(P, apollo_inputs, functionality)	 ### Prepare and return outputs of function
  
  return(P)
}


Model3 = apollo_estimate(apollo_beta,
                         apollo_fixed,
                         apollo_probabilities,
                         apollo_inputs)

apollo_modelOutput(Model3)
apollo_modelOutput(Model2)

#Get Beta vectors for the Original model (o) and subset (s)
Beta_o<-Model2$estimate
Beta_s<-Model3$estimate

v_o<-Model2$varcov         #Covariance matrix for original model
v_s<-Model3$varcov         #Covariance matrix for subset model

#When we want to put the betas into the formula in order to substract the betas from the original model with the subset model 
# we will realize that there are model variables on the original model than in the subset, as the original is taking the choice 9. So we have to get rid of it first.

Beta_o<-Beta_o[c(1:7,9:15,17:19)]                 # to remove the 'chosen' estimated  in beta estimate of original model
v_o<-v_o[c(1:7,9:15,17:19),c(1:7,9:15,17:19)]

Beta_diff<-Beta_s-Beta_o
v_diff<-v_s-v_o
#In order to do that we have to calculate the inverse matrix for the Betas vectors
#Inverse Matrix calculation:
solve(v_diff)
#Solve the Formula HF-t
(Beta_diff%*%solve(v_diff))%*%Beta_diff
 

  #Not reject H0 -> we check on the Chi Table with the number of the lenght of Beta_o


#Willignes to Pay with Model 2
# tell us how much would we be willing to pay through fixcost for reduction in operating cost
wtp<-Model2$estimate["beta_OC"]/(Model2$estimate["beta_FC"]+Model2$estimate["beta_FC_income"]*database$income)

plot(database$income,wtp,xlab="income",ylab="willingness to pay")

#The plot tells us ->When you have less income you are more willingness to pay to reduce the Operations Costs
