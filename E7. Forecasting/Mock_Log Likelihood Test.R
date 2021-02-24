#Quiz



#write.csv2(database,"Telephone.csv")

database<-read.csv2("Telephone.csv",header=TRUE)


#a) Determine the mean number of users in a household that has chosen the option "local flat"


temp<-subset(database, choice==3)
mean(temp$users)



#b)

library(apollo)

apollo_initialise()

#set some controls

#indicate the name (in quotes) of the column in the data which contains the identifer variable
#for individual decision makers. For our data set that is "ID"

#Model 1


apollo_control=list(modelName="MC2",modelDescr="Some Description",indivID="ID")


#Define name and starting values for the coefficients to be estimated

apollo_beta=c(asc_BM             = 0,
              asc_LF             = 0,
              asc_EF		   = 0,
              asc_MF		   = 0,
              beta_cost		   = 0		
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
  V[['SM']]		= 		+ beta_cost * log(cost2)
  V[['BM']] 	= asc_BM    + beta_cost * log(cost1)
  V[['LF']] 	= asc_LF    + beta_cost * log(cost3) 
  V[['EF']] 	= asc_EF    + beta_cost * log(cost4)
  V[['MF']] 	= asc_MF    + beta_cost * log(cost5)
  
  
  
  mnl_settings = list(						      							### Define settings for model 
    alternatives = c(SM=2, BM=1, LF=3, EF=4,MF=5),									### component	
    avail        =  list(SM=avail2,BM=avail1,LF=avail3,EF=avail4,MF=avail5),
    choiceVar    = as.character(choice),
    V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)	 ### Compute probabilities using model
  
  #P = apollo_panelProd(P, apollo_inputs, functionality)	 ### Take product across observation
  ### for same ID
  
  P = apollo_prepareProb(P, apollo_inputs, functionality)	 ### Prepare and return outputs of function
  
  return(P)
}


Tel1 = apollo_estimate(apollo_beta,
                       apollo_fixed,
                       apollo_probabilities,
                       apollo_inputs)

apollo_modelOutput(Tel1)





#c)


apollo_initialise()

#set some controls

#indicate the name (in quotes) of the column in the data which contains the identifer variable
#for individual decision makers. For our data set that is "ID"

#Model 1


apollo_control=list(modelName="MC2",modelDescr="Some Description",indivID="ID")


#Define name and starting values for the coefficients to be estimated

apollo_beta=c(asc_BM             = 0,
              asc_LF             = 0,
              asc_EF		   = 0,
              asc_MF		   = 0,
              asc_BM_u           = 0,
              asc_LF_u           = 0,
              asc_EF_u		   = 0,
              asc_MF_u		   = 0,
              beta_cost		   = 0		
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
  V[['SM']]		= 						        + beta_cost * log(cost2)
  V[['BM']] 	= asc_BM    +asc_BM_u*users		+ beta_cost * log(cost1)
  V[['LF']] 	= asc_LF    +asc_LF_u*users		+ beta_cost * log(cost3) 
  V[['EF']] 	= asc_EF    +asc_EF_u*users		+ beta_cost * log(cost4)
  V[['MF']] 	= asc_MF    +asc_MF_u*users		+ beta_cost * log(cost5)
  
  
  
  mnl_settings = list(						      							### Define settings for model 
    alternatives = c(SM=2, BM=1, LF=3, EF=4,MF=5),									### component	
    avail        =  list(SM=avail2,BM=avail1,LF=avail3,EF=avail4,MF=avail5),
    choiceVar    = as.character(choice),
    V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)	 ### Compute probabilities using model
  
  #P = apollo_panelProd(P, apollo_inputs, functionality)	 ### Take product across observation
  ### for same ID
  
  P = apollo_prepareProb(P, apollo_inputs, functionality)	 ### Prepare and return outputs of function
  
  return(P)
}


Tel2 = apollo_estimate(apollo_beta,
                       apollo_fixed,
                       apollo_probabilities,
                       apollo_inputs)

apollo_modelOutput(Tel2)

T=-2*(Tel1$LLout-Tel2$LLout)





