#Willignes to Pay with Model 2

wtp<-Model2$estimate["beta_OC"]/(Model2$estimate["beta_FC"]+Model2$estimate["beta_FC_income"]*database$income)

plot(database$income,wtp,xlab="income",ylab="willingness to pay")

#The plot tells us ->When you have less income you are more willingness to pay to reduce the Operations Costs
