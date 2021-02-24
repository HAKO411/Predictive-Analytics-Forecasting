database<-read.csv2(file="quebec.csv", header=TRUE)

library(apollo)								# run apollo package
apollo_initialise()

#set some controls

#indicate the name (in quotes) of the column in the data which contains the identifer variable
#for individual decision makers. For our data set that is "ID"


apollo_control=list(modelName="Baseline Model",
                    modelDescr="Some Description",indivID="obs")


#Define name and starting values for the coefficients to be estimated

apollo_beta=c(asc_2              = 0,
              asc_3              = 0,
              asc_4		           = 0,
              asc_5		           = 0,
              asc_6		           = 0,
              asc_7		           = 0,
              asc_8		           = 0,
              asc_9		           = 0,
              beta_oc		         = 0,
              beta_fc		         = 0		
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
  V[['gg']]		= 		     beta_oc * op_cost.1 + beta_fc * fix_cost.1 
  V[['ge']] 	=   asc_2	+beta_oc * op_cost.2 + beta_fc * fix_cost.2 
  V[['deo']] 	=   asc_3	+beta_oc * op_cost.3 + beta_fc * fix_cost.3
  V[['dee']] 	=   asc_4	+beta_oc * op_cost.4 + beta_fc * fix_cost.4
  V[['oo']] 	=   asc_5	+beta_oc * op_cost.5 + beta_fc * fix_cost.5
  V[['oe']]  	=   asc_6	+beta_oc * op_cost.6 + beta_fc * fix_cost.6
  V[['ee']]  	=   asc_7	+beta_oc * op_cost.7 + beta_fc * fix_cost.7
  V[['we']]  	=   asc_8	+beta_oc * op_cost.8 + beta_fc * fix_cost.8
  V[['wee']] 	=   asc_9	+beta_oc * op_cost.9 + beta_fc * fix_cost.9
  
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


BaseModel = apollo_estimate(apollo_beta,
                            apollo_fixed,
                            apollo_probabilities,
                            apollo_inputs)

apollo_modelOutput(BaseModel)



#2. compute predicted choice probabilities

#"by hand"


asc<-c(0,estimates[1:8])   #? 'estimates' not found
beta_1<-estimates[9]
beta_2<-estimates[10]

op_cost<-database[1,14:22]
fix_cost<-database[1,23:31]

util<-asc+beta_1*op_cost+beta_2*fix_cost

e_util<-exp(util)

p<-e_util/sum(e_util) 



#"using R"


predictions = apollo_prediction(BaseModel, apollo_probabilities, apollo_inputs)

predictions<-data.frame(predictions)
predictions[1,]

# Questions 2 Disposable Income and willing to pay

inc<-3.2500
a_wtp<-Model2$estimate["beta_OC"]/(Model2$estimate["beta_FC"]+Model2$estimate["beta_FC_income"]*inc)
r<-1/a_wtp