#Elasticity formula: P is the probability that choice maker n choses alternative I. X is Value of fixed cost. Beta is the coeeficient for the attribute that we are considering.
# Compute direct price elasticities for fixed and operational cost.

FC<-database[,23:31]
OC<-database[,14:22]

#1º. Compute probabilities
probs= apollo_prediction(Model2, apollo_probabilities, apollo_inputs)
probs<-data.frame(probs)
probs<-probs[,3:11]

#2º. Compute the coefficients
Beta_oc<-Model2$estimate["beta_OC"]
Beta_fc<-Model2$estimate["beta_FC"]+Model2$estimate["beta_FC_income"]*database$income


elast.ic<-Beta_fc*FC*(1-probs)
elast.oc<-Beta_oc*OC*(1-probs)


#cross elasticities

#select the beta of the cost with the name of the corresponding column of the data base and select 
#from the probs matrix the corresponding column of the alternative to do the cross elasticity
crosselast.ic=-Beta_fc*FC$fix_cost.5*probs$oo 
crosselast.oc=-Beta_oc*OC$op_cost.5*probs$oo

hist(elast.ic[,7],breaks=50,xlab="Price elasticity of installation cost of electric-electric alternative")
plot(FC[,7],elast.ic[,7],pch=20,xlab="Installation cost of electric-electric alternative",
ylab="Price elasticity of installation cost of electric-electric alternative ")

install.packages("ggplot2")
library(ggplot2)

temp<-data.frame(elast.ic$fix.)


