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

#Assume a price increase for operation cost of gas by 15%. Predict the market shares of the alternatives under this price scenario.

database<-read.csv2(file="quebec.csv", header=TRUE)
database$op_cost.1<-database$op_cost.1*1.15

apollo_inputs = apollo_validateInputs()

#Matrix with preidctions for each choice maker
predictions2 = apollo_prediction(Model2, apollo_probabilities, apollo_inputs)
predictions2 <-data.frame(predictions2)
predictions2 <-predictions2[3:11]

colMeans(predictions2) #gives the mean per column i  order to calculate market share


#Play around with the other variables in the data set and come up with your own model specification. Which one do you prefer? Why?