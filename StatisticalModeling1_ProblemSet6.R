# PROBLEM SET 6
#_______________________________________________________________________________

# PROBLEM 1

data<-read.table("tirereliability.txt", sep="\t", dec=".", header=TRUE)
head(data)
attach(data)

# Y = complete
# T = survival.
# X = wedge

plot(wedge, survival, main = "Tire survival regarding Wedge")

Surv(survival, complete)

# a)
# Calculate the estimate for the parameter ??.
# Cox proportional model

library(survival)
model<-coxph(Surv(survival, complete)~wedge, data=data)
summary(model)
beta0 <- coef(model)
beta0

# b) Estimate the value of the survival function 
# S(t|xi) = P (T ??? t|xi) at the time point t = 1.00 when xi = 0.6.

newdata<-data.frame(wedge=c(0.6))
sf<-survfit(model, newdata=newdata)

summary(sf)

summary(sf, times=1.00) # point estimate
plot(sf,conf.int=c("none"))

sf<-survfit(model, newdata=newdata,conf.type="plain")
summary(sf)
plot(sf, main="Survival Function plot")

# c) Estimate the hazard ratio

newdata<-data.frame(wedge=c(0.6, 1.6))
risk<-predict(model, newdata=newdata, type="risk")
risk[1]/risk[2] # hazard ration

# d) Cox proportional hazards regression model
# where X1 = wedge, X2 = peelForce and X3 = interBelt. Test at 5% signi???cance level, 
# is the explanatory variable X1 = wedge statistically signi???cant variable.

model<-coxph(Surv(survival, complete)~wedge, data=data)
model.H1<-coxph(Surv(survival, complete) ~ wedge + peelForce + interBelt + wedge*peelForce, data=data)
summary(model.H1)

model.H0<-coxph(Surv(survival, complete) ~ peelForce + interBelt, data=data)
summary(model.H0)
anova(model.H0, model.H1)
anova(model.H0, model.H1)$"P(>|Chi|)"[2]
# Do not reject H0, therefore the explanatory variable X1: wedge is not statistically
# significant

# e) where X1 = wedge, X2 = peelForce and X3 = interBelt. Create 95% CI for the 
# survival function S(t|xi) = P (T ??? t|xi) at the time point t = 1.00 when 
# xi1 = 0.6, xi2 = 0.8, and xi3 = 0.7

model<-coxph(Surv(survival, complete) ~ wedge + peelForce + interBelt + wedge*peelForce, data=data)
summary(model)

newdata<-data.frame(wedge=0.6, peelForce=0.8, interBelt=0.7)
sf<-survfit(model, newdata=newdata,conf.type="plain")
sf<-survfit(model, newdata=newdata)
summary(sf, times=1) 

summary(sf, times=1)$lower
summary(sf, times=1)$upper

################################################################################

# PROBLEM 2

#Let X = wedge. Consider the Weibull proportional hazards regression model

# a) Estimate the Hazard Ration

library(eha)

model.wph<-phreg(Surv(survival, complete)~wedge, data=data, dist="weibull")
summary(model.wph)
coef(model.wph)

p <- exp(coef(model.wph)[3])
lambda <- exp(coef(model.wph)[2])
beta <- coef(model.wph)[1]
x <- 0.6
lambda.star <- lambda/exp((x*beta)/p)
mu_0.6 <- lambda.star*gamma(1+(1/p))
mu_0.6

x1 <- 1.6
lambda.star1 <- lambda/exp((x1*beta)/p)
mu_1.6 <- lambda.star1*gamma(1+(1/p))
mu_1.6

hazard_Ratio <- mu_0.6/mu_1.6
hazard_Ratio


# b)
p<-exp(coef(model.wph)[3])
lambda<-exp(coef(model.wph)[2])
beta<-coef(model.wph)[1]
x<-1.6

lambda.star<-lambda/exp((x*beta)/p)

mu<-lambda.star*gamma(1+(1/p))
mu

#expected survival time es 1.80

# c) 

t.star<-rweibull(10000, shape=p, scale=lambda.star)
lowerbound<-quantile(t.star, c(0.1))
upperbound<-quantile(t.star, c(0.9))
lowerbound
upperbound


qweibull(0.1, shape=p, scale=lambda.star)
qweibull(0.9, shape=p, scale=lambda.star)


# d) #X1 = wedge, X2 = peelForce and X3 = interBelt. 
model.wph_d<-phreg(Surv(survival, complete)~ wedge + peelForce + interBelt +
                   wedge*peelForce, data=data, dist="weibull")

summary(model.wph_d)
coef(model.wph_d)

plot(model.wph, fn = "sur")

p<-exp(coef(model.wph_d)[6])
lambda<-exp(coef(model.wph_d)[5])
beta<-t(t(coef(model.wph_d)[1:4]))
x<-t(t(c(0.6, 0.8, 0.7, 0.6*0.8)))
lambda.star<-lambda/exp((t(x)%*%beta)/p)

survival<-1-pweibull(1.00,shape=p, scale=lambda.star)
survival

survival.function<-1-pweibull(0:2,shape=p, scale=lambda.star)
plot(0:2, survival.function, type="l", col="red", main = "Survival Function") 



