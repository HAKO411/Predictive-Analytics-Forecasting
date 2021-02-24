inc<-3.2500
a_wtp<-Model2$estimate["beta_OC"]/(Model2$estimate["beta_FC"]+Model2$estimate["beta_FC_income"]*inc)
r<-1/a_wtp

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