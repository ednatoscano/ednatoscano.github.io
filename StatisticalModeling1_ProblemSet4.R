############################
### PROBLEM SET 4
# PROBLEM 1

data<-read.table("leukoplakia.txt", sep="\t", dec=".", header=TRUE)
attach(data)

interaction.plot(factor(Alcohol), factor(Smoker), Leukoplakia)

# dataset: leukoplakia.txt
# Y = Leukoplakia: yes = 1, no = 0
# X1 = Alcohol: no = 1, less than 40g = 2, less than 80g = 3, more than 80g = 4
# X2 = Smoker: yes = 1, no = 0

# a) Assume Yi ~ Bern(mu_jh)
# Model: logit(mu_jh) = B0 + Bj + alpha_h
# MLE for the probability when: xi1 = 3 = "less than 80g", xi2 = 1 = "yes".

model_main_bernoulli<-glm(Leukoplakia ~ factor(Alcohol) + factor(Smoker), 
                          family=binomial(link="logit"), data=data)
summary(model_main_bernoulli)

newdata<-data.frame(Alcohol=3, Smoker=1)
predict(model_main_bernoulli, newdata=newdata, type="response")

fitted.values <- fitted(model_main_bernoulli, type="response")
# visualizing the data with the fitted values
par(mfrow=c(2,1))
interaction.plot(factor(Alcohol), factor(Smoker), Leukoplakia)
interaction.plot(factor(Alcohol), factor(Smoker), fitted.values)
#data.frame(data, fitted.values)

# b) 
model_H0 <- glm(Leukoplakia ~factor(Alcohol), family=binomial(link="logit"), data=data)
summary(model_H0)

anova(model_H0, model_main_bernoulli, test="Chi")
anova(model_H0, model_main_bernoulli, test="Chi")$Deviance[2]
anova(model_H0, model_main_bernoulli, test="Chi")$"Pr(>Chi)"[2]
anova(model_H0, model_main_bernoulli, test="Chi")$"Pr(>Chi)"[2] < 0.05
# Reject H0, therefore X2 is statistically significant to the main effect model

# c) Estimate the odd ratio
newdata<-data.frame(Alcohol=c(4,3), Smoker=c(1,1))
pred<-predict(model_main_bernoulli, newdata=newdata, type="response")
predict.data<-data.frame(newdata,pred)

# Odd ratio
pred[1] # More than 80g, yes
pred[2] # Less than 80 gr, yes

Odds.Ratio <- (pred[1]/(1-pred[1])) / (pred[2] / (1-pred[2]))
Odds.Ratio
1/Odds.Ratio


# If  the Odd Ratio is far away from the value 1 it indicates that 
# If it is positive number it mean that the probability of pred[1] is higher thatn pred[2]
# the farther the OR is from value 1 there is an indication that probability 
# in this subpopulation 

# d)
# H0 Model
model_main_quasibinomial <- glm(Leukoplakia ~ factor(Alcohol) + factor(Smoker), 
                                family=quasibinomial(link="logit"), data=data)
summary(model_main_quasibinomial)

# H1 Model
model_interaction_quasibinomial <- glm(Leukoplakia ~ factor(Alcohol)*factor(Smoker), 
                                family=quasibinomial(link="logit"), data=data)
summary(model_interaction_quasibinomial)

anova(model_main_quasibinomial, model_interaction_quasibinomial, test="F")
anova(model_main_quasibinomial, model_interaction_quasibinomial, test="F")$F[2]
anova(model_main_quasibinomial, model_interaction_quasibinomial, test="F")$"Pr(>F)"[2]
anova(model_main_quasibinomial, model_interaction_quasibinomial, test="F")$"Pr(>F)"[2] < 0.05
# Do not reject the model that best suits the data is the main 
# effect model

# e) Main effect model
# i. Probit link 
# ii. Cauchy link 
# iii) Gumbel link (complementary log-log)

model_probit <- glm(Leukoplakia ~ factor(Alcohol) + factor(Smoker), 
                    family=binomial(link="probit"), data=data)
model_cauchy <- glm(Leukoplakia ~ factor(Alcohol) + factor(Smoker), 
                 family=binomial(link="cauchit"), data=data)
model_gumbel <- glm(Leukoplakia ~ factor(Alcohol) + factor(Smoker), 
                    family=binomial(link="cloglog"), data=data)
#AIC
AIC(model_probit) # 267.8715
AIC(model_cauchy) # 267.0245
AIC(model_gumbel) # 268.1663
# Thus, the Cauchy model has the smalles value

# MSE
sum((Leukoplakia - fitted(model_probit, type="response"))^2)/27 # 1035.537
sum((Leukoplakia - fitted(model_cauchy, type="response"))^2)/27 # 1035.537
sum((Leukoplakia - fitted(model_gumbel, type="response"))^2)/27 # 1389.629
# The model with the lowest MSE value is Cauchy

####################################################################
# PROBLEM 2

data<-read.table("applejuiceCRA7152.txt", sep="\t", dec=".", header=TRUE)
attach(data)

plot(data)

# dataset: leukoplakia.txt
# Y = Growth
# X1=pH
# X2=Nisin concentration
# X3=Temperature
# X4=Brix Concentration
# Y=Growth (1=Yes, 0=No)
# a)

# i. Identity link g(µi) = µi,
# ii. log link g(µi) = log(µi),
# iii. Inverse link g(µi) = 1/µi
# iv. logit link g(µi) = logit(µi),
# v. Probit link g(µi) = ?????1(µi),
# vi. Cauchy link g(µi) = F ???1 cauchy(µi),
# vii. Gumbel link g(µi) = log (??? log(1 ??? µi)),
par(mfrow=c(4,1))
plot(pH, jitter(Growth, amount=0.03))
plot(Nisin, jitter(Growth, amount=0.03))
plot(Temperature, jitter(Growth, amount=0.03))
plot(Brix, jitter(Growth, amount=0.03))
coplot(jitter(Growth)~Nisin| pH)
# According to the plot, the distribution that would model the data the best 
# would be Bernoulli distribution.

# BERNOULLI (BINOMIAL) DISTRIBUTION
# i. Identity link g(µi) = µi, NOT AVAILABLE
# ii. log link g(µi) = log(µi), NOT AVAILABLE
# iii. Inverse link g(µi) = 1/µi

# iv. logit link g(µi) = logit(µi)
model_binomial_logit <- glm(Growth~pH+Nisin+Temperature+Brix, family=binomial(link="logit"), data=data)
summary(model_binomial_logit)
AIC(model_binomial_logit) # 62.33065
sum((Growth - fitted(model_binomial_logit, type="response"))^2)/27 # 0.3104147

step(model_binomial_logit)

# v. Probit link
model_binomial_probit <- glm(Growth~pH+Nisin+Temperature+Brix, family=binomial(link="probit"), data=data)
summary(model_binomial_probit)
AIC(model_binomial_probit) # 62.01991
sum((Growth - fitted(model_binomial_probit, type="response"))^2)/27 # 0.3215375

# vi. Cauchy link
model_binomial_cauchy <- glm(Growth~pH+Nisin+Temperature+Brix, family=binomial(link="cauchit"), data=data)
summary(model_binomial_cauchy)
AIC(model_binomial_cauchy) # 55.77467 LOWEST
sum((Growth - fitted(model_binomial_cauchy, type="response"))^2)/27 # 0.1907238 LOWEST

# vii. Gumbel link
model_binomial_gumbel <- glm(Growth~pH+Nisin+Temperature+Brix, family=binomial(link="cloglog"), data=data)
summary(model_binomial_gumbel)
AIC(model_binomial_gumbel) # 65.16154
sum((Growth - fitted(model_binomial_gumbel, type="response"))^2)/27 # 0.3376689

#______________________________________________________________________
# Since Cauchy link function has both the lowest AIC and MSE value, we select this model
# however, we try to check if all the variables are significant:
# H0: Only X1 is significant
model_binomial_cauchy_H0 <- glm(Growth~pH, family=binomial(link="cauchit"), data=data)
model_binomial_cauchy_H1 <- glm(Growth~pH+Nisin+Temperature+Brix, family=binomial(link="cauchit"), data=data)

anova(model_binomial_cauchy_H0, model_binomial_cauchy_H1, test="Chi")
anova(model_binomial_cauchy_H0, model_binomial_cauchy_H1, test="Chi")$Deviance[2]
anova(model_binomial_cauchy_H0, model_binomial_cauchy_H1, test="Chi")$"Pr(>Chi)"[2] < 0.05
# Reject H0, Model H1 is best model

# Selecting Cauchy link function, we try to check if
# H0: X4 is significant
model_binomial_cauchy_H0 <- glm(Growth~pH+Nisin+Temperature, family=binomial(link="cauchit"), data=data)
model_binomial_cauchy_H1 <- glm(Growth~pH+Nisin+Temperature+Brix, family=binomial(link="cauchit"), data=data)

anova(model_binomial_cauchy_H0, model_binomial_cauchy_H1, test="Chi")
anova(model_binomial_cauchy_H0, model_binomial_cauchy_H1, test="Chi")$Deviance[2]
anova(model_binomial_cauchy_H0, model_binomial_cauchy_H1, test="Chi")$"Pr(>Chi)"[2] < 0.05
# Do not reject H0, X4 is significant significant
AIC(model_binomial_cauchy_H1)
AIC(model_binomial_cauchy_H0) # Has a better AIC value

# We chose this model:
summary(model_binomial_cauchy_H0)

fit<-fitted(model_binomial_cauchy_H0, type="response")
plot(pH, Growth ,main="Binomial model Cauchy link vs Fitted values", pch=19, col="blue")
points(pH, fit, pch=15, cex=1,col="green")

plot(Nisin, Growth,main="Binomial model Cauchy link vs Fitted values", pch=19, col="blue")
points(Nisin, fit, pch=15, cex=1,col="orange")

plot(Temperature, Growth,main="Binomial model Cauchy link vs Fitted values", pch=19, col="blue")
points(Temperature, fit, pch=15, cex=1,col="orange")



# Simulation with predict data
predict_data<-data.frame(pH=5.5, Nisin=50, Temperature=25:55)
predicted<-predict(model_binomial_cauchy_H0, newdata=predict_data, type="response")
plot(25:55, predicted, type="n")
lines(25:55, predicted, col="red", cex=1.5)

predict_data<-data.frame(pH=3.5, Nisin=0, Temperature=25:55)
predicted<-predict(model_binomial_cauchy_H0, newdata=predict_data, type="response")
plot(25:55, predicted, type="n")
lines(25:55, predicted, col="red", cex=1.5)


# b) Expected value mu_i
# X1 = 4.5, X2 = 20, X3 = 30, X4 = 17.
newdata<-data.frame(pH=4.5, Nisin=20, Temperature=30)
predict(model_binomial_cauchy_H0, newdata=newdata, type="response")


# c) Calculate the 95% con???dence interval estimate for the expected value µi 
newdata<-data.frame(pH=4.5, Nisin=20, Temperature=30)
predict(model_binomial_cauchy_H0, newdata=newdata, type="response")

eta<-predict(model_binomial_cauchy_H0, newdata=newdata, type="link", se.fit=TRUE)

link.lowerbound<-eta$fit-qnorm(0.975)*eta$se.fit
link.upperbound<-eta$fit+qnorm(0.975)*eta$se.fit

mu.lowerbound<-exp(link.lowerbound)/(1+exp(link.lowerbound))
mu.upperbound<-exp(link.upperbound)/(1+exp(link.upperbound))
c(mu.lowerbound, mu.upperbound) # Probability Mu_i*


# d)
#Let us assume that there are 100 apple juices with explanatory variables
#are set on values
#X1 = 4.5, X2 = 20, X3 = 30, X4 = 17.
#How many of these 100 juices are such that bacterial spores of Alicyclobacil-
#  lus Acidoterrestris CRA7152 are occuring in them? Create 80% prediction
#interval for the number of apple juices a???ected by Alicyclobacillus Aci-
#doterrestris CRA7152 bacteria
### d)

newdata<-data.frame(pH=4.5, Nisin=20, Temperature=30)
mu.f<-predict(model_binomial_cauchy_H0, newdata=newdata, type="response")
YS.pred<-100*mu.f # point prediction

mu.hat<-predict(model_binomial_cauchy_H0, newdata=data, type="response")
index<-dim(data)[1]
n<-data$Growth #amount of sampling units

e.b<-numeric()

for(b in 1:1000){
  
  sum.yb<-numeric()
  for(i in 1:index){
    
    sum.yb[i]<-sum(sample(0:1,n[i], replace=TRUE,prob=c(1-mu.hat[i],mu.hat[i])))
    
  }
  
  model.B<-glm(cbind(sum.yb, n-sum.yb)~pH+Nisin+Temperature, family=binomial(link="cauchit"), data=data)
  newdata<-data.frame(pH=4.5, Nisin=20, Temperature=30)
  mu.fB<-predict(model.B, newdata=newdata, type="response")
  YS.predB<-100*mu.fB
  
  yf.b<-sample(0:1,100,prob=c(1-mu.f,mu.f), replace=TRUE)
  
  e.b[b]<-sum(yf.b)-YS.predB
  
}

var.error<-var(e.b)
var.error

z<-qnorm(c(0.9))
lower.bound<-YS.pred-z*sqrt(var.error)
upper.bound<-YS.pred+z*sqrt(var.error)
c(lower.bound, upper.bound)












model.mainQuasi<-glm(Growth~pH+Nisin+Temperature, family=quasibinomial(link="logit"), data=data)
summary(model.mainQuasi)
AIC(model.mainQuasi)


library(ggplot2)
plot(pH,Growth, pch=19, col="blue", xlim=c(0,80))
points(Nisin, Growth, pch=15, cex=1,col="green")
points(Temperature, Growth, pch=15, cex=1,col="orange")
lines(seq(3.5,5.5, length=74), predict(model_binomial_cauchy_H0, newdata=data.frame(pH=seq(3.5,5.5, length=74)), type="response"), col="pink")


plot(pH,Growth, pch=19, col="blue")
lines(seq(3.5,5.5, length=74), predict(model_binomial_cauchy_H0, newdata=data.frame(pH=seq(3.5,5.5, length=74)), type="response"), col="pink")



lines(seq(3.5,5.5,1), predict(model_binomial_cauchy_H0, newdata=data.frame(Time=seq(0,73,1)), type="response"), col="pink")

lines(seq(0,73,1), predict(model_binomial_cauchy_H0, newdata=data.frame(pH=seq(0,73,1)), type="response"), col="pink")



lines(seq(15,45,1), predict(model.inverse,newdata=data.frame(pH=seq(15,45,1)), type="response"), col="black")
lines(seq(15,45,1), predict(model.log,newdata=data.frame(Time=seq(15,45,1)), type="response"), col="red")


fit<-fitted(model_binomial_cauchy_H0, type="response")
plot(Growth, pH,main="Binomial model Cauchy link vs Fitted values", pch=19, col="blue")
points(fit, pH, pch=15, cex=1,col="green")

points(Growth, Nisin, pch=15, cex=1,col="yellow")
points(fit, Nisin, pch=15, cex=1,col="orange")

plot(Growth, Temperature,main="Binomial model Cauchy link vs Fitted values", pch=19, col="blue")
points(fit, Temperature, pch=15, cex=1,col="orange")

