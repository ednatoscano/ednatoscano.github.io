############################
### PROBLEM SET 5
# PROBLEM 1

data<-read.table("canopycover.txt", sep="\t", dec=".", header=TRUE)
attach(data)

# Y: canopycover
# xi1: basalarea
# xi2: dbh.mean
# X3: species

coplot(canopycover~basalarea| dbh.mean * factor(species))

plot(basalarea, canopycover)
plot(dbh.mean, canopycover)
plot(factor(species), canopycover)

# a)
# main effect

# Normal identity link
model_normal_identity <- glm(canopycover~basalarea + dbh.mean + factor(species), family=gaussian(link="identity"), data=data)
summary(model_normal_identity)

plot(basalarea, canopycover)
newdata<-data.frame(basalarea=seq(0,50, length=114))
lines(seq(0,50, length=114), predict(model_normal_identity, newdata=newdata, type="response"), col="red", lwd=2)

plot(dbh.mean, canopycover)
newdata<-data.frame(dbh.mean=seq(0,35, length=114))
lines(seq(0,35, length=114), predict(model_normal_identity, newdata=newdata, type="response"), col="orange", lwd=2)

AIC(model_normal_identity) # -275.7166
sum((canopycover - fitted(model_normal_identity, type="response"))^2)/114 # 0.004775945

# Gamma log link
model_gamma_log <- glm(canopycover~basalarea + dbh.mean + factor(species), family=Gamma(link="log"), data=data)
summary(model_gamma_log)

plot(basalarea, canopycover)
newdata<-data.frame(basalarea=seq(0,50, length=114))
lines(seq(0,50, length=114), predict(model_gamma_log, newdata=newdata, type="response"), col="red", lwd=2)

plot(dbh.mean, canopycover)
newdata<-data.frame(dbh.mean=seq(0,35, length=114))
lines(seq(0,35, length=114), predict(model_gamma_log, newdata=newdata, type="response"), col="orange", lwd=2)

AIC(model_gamma_log) # -372.2039
sum((canopycover - fitted(model_gamma_log, type="response"))^2)/114 # 0.007594314

# Inverse gaussian log link
model_ig_log <- glm(canopycover~basalarea + dbh.mean + factor(species), family=inverse.gaussian(link="log"), data=data)
summary(model_ig_log)

plot(basalarea, canopycover)
newdata<-data.frame(basalarea=seq(0,50, length=114))
lines(seq(0,50, length=114), predict(model_ig_log, newdata=newdata, type="response"), col="red", lwd=2)

plot(dbh.mean, canopycover)
newdata<-data.frame(dbh.mean=seq(0,35, length=114))
lines(seq(0,35, length=114), predict(model_ig_log, newdata=newdata, type="response"), col="orange", lwd=2)

AIC(model_ig_log) # -300.5095
sum((canopycover - fitted(model_ig_log, type="response"))^2)/114 # 0.007287518

# Beta logit
library(betareg)
model_beta_logit <- betareg(canopycover~basalarea + dbh.mean + factor(species), link="logit", data=data)
summary(model_beta_logit)

plot(basalarea, canopycover)
newdata<-data.frame(basalarea=seq(0,50, length=114))
lines(seq(0,50, length=114), predict(model_beta_logit, newdata=newdata, type="response"), col="red", lwd=2)

plot(dbh.mean, canopycover)
newdata<-data.frame(dbh.mean=seq(0,35, length=114))
lines(seq(0,35, length=114), predict(model_beta_logit, newdata=newdata, type="response"), col="orange", lwd=2)

AIC(model_beta_logit) # -408.0153
sum((canopycover - fitted(model_beta_logit, type="response"))^2)/114 # 0.001404962

# Base on AIC and MSE, we select the Beta logit model as the best fit, since it 
# has the lowest value among all the other distributions.

# b) MLE of mu_i when xi???1 = 20, xi???2 = 15, and xi???3 =pine.
newdata<-data.frame(basalarea=20,dbh.mean=15, species="pine")
predict(model_beta_logit, newdata=newdata, type="response") # expected value of mu_i

# c) 95% confidence interval estimate for the expected value µi

eta<-predict(model_beta_logit, newdata=newdata, type="link") 
modeldata<-expand.grid(basalarea=20,dbh.mean=15, species=c("pine","spruce"))
X<-model.matrix(~basalarea + dbh.mean + factor(species), data=modeldata)
xf<-t(t(X[1,]))
cov.eta<-t(xf)%*%vcov(model_beta_logit)[-5,-5]%*%xf

lowerbound<-exp(eta-qnorm(0.975)*sqrt(cov.eta))/(1+exp(eta-qnorm(0.975)*sqrt(cov.eta)))
upperbound<-exp(eta+qnorm(0.975)*sqrt(cov.eta))/(1+exp(eta+qnorm(0.975)*sqrt(cov.eta)))
c(lowerbound, upperbound)

# d) 80% prediction interval for the new observation yf

library(mvtnorm) 

model_beta_logit <- betareg(canopycover~basalarea + dbh.mean + factor(species), link="logit", data=data)
newdata<-data.frame(basalarea=20,dbh.mean=15, species="pine")
pred<-predict(model_beta_logit, newdata=newdata, type="response")
pred
eta.f<-predict(model_beta_logit, newdata=newdata, type="link")
phi.hat<-coef(model_beta_logit)[5]

modeldata<-expand.grid(basalarea=20,dbh.mean=15, species=c("pine","spruce"))
X<-model.matrix(~basalarea + dbh.mean + factor(species), data=modeldata)
xf<-t(t(X[1,]))

etarow<-c(t(xf),0)
phirow<-c(rep(0,length(xf)),1)
A<-rbind(etarow,phirow)

cov.etaphi<-A%*%vcov(model_beta_logit)%*%t(A)
etaphi.star<-rmvnorm(1000, mean = c(eta.f,phi.hat), sigma = cov.etaphi)

muf.star<-exp(etaphi.star[,1])/(1+exp(etaphi.star[,1]))
phi.star<-etaphi.star[,2]

p.star<-muf.star*phi.star
q.star<-phi.star*(1-muf.star)

yf.star<-rbeta(1000, shape1=p.star, shape2=q.star)

lower.bound<-quantile(yf.star, c(0.1))
upper.bound<-quantile(yf.star, 1-c(0.1))
c(lower.bound, upper.bound)

# e) Test at 5% signi???cance level, is the explanatory variable X2 = dbh.mean
#    statistically signi???cant variable in the two way interaction model

model_H0 <- betareg(canopycover~basalarea*factor(species), link="logit", data=data)
model_H1 <- betareg(canopycover~basalarea*dbh.mean + basalarea*factor(species) + dbh.mean*factor(species), link="logit", data=data)

library(lmtest)
lrtest(model_H0, model_H1)
lrtest(model_H0, model_H1)$Chisq[2]
# p-value 2.2e-16 < 0.01, reject H0, therefor X2:dbh.mean is statistically significant

################################################################################
# PROBLEM 2

data<-read.table("NitrogenYield.txt", sep="\t", dec=".", header=TRUE)
attach(data)

# Y = Yield
# X = Nitrogen a???ects the amount of yield measured in variable 

# Let us assume the normality Yi ??? N(µi, ??2).

# Model the expected value µi by the second degree polynomial model

plot(Nitrogen, Yield)

# a) Model the expected value µi by the second degree polynomial model
model.poly<-lm(Yield~Nitrogen+I(Nitrogen^2))
summary(model.poly)

newdata<-data.frame(Nitrogen=10:200)
lines(newdata$Nitrogen, predict(model.poly, newdata=newdata), col="red", lwd=2)

coef(model.poly)[3] # B2_hat

# b) Model the expected value µi by the exponential model
model.exp<-glm(Yield~log(Nitrogen), family=gaussian(link="log"), data=data)
summary(model.exp)
lines(newdata$Nitrogen, predict(model.exp, newdata=newdata, type="response"), col="green", lwd=2)

newdata <- data.frame(Nitrogen=150)
predict(model.exp, newdata=newdata, type="response")

# c)
library(nlme)
model.asymp<-nls(Yield~SSasymp(Nitrogen, Asym,R0,lrc), data=data)
summary(model.asymp)
newdata<-data.frame(Nitrogen=10:200)
lines(newdata$Nitrogen, predict(model.asymp, newdata=newdata, type="response"), col="orange", lwd=2)

coef(model.asymp)[1]

# d)

model.mm<-nls(Yield~SSmicmen(Nitrogen, Vm, K), data=data)
summary(model.mm)
lines(newdata$Nitrogen, predict(model.mm, newdata=newdata, type="response"), col="purple", lwd=2)

newdata <- data.frame(Nitrogen=150)
predict(model.mm, newdata=newdata, type="response")[1]

# e)
library(mvtnorm)
model.asymp<-nls(Yield~SSasymp(Nitrogen, Asym,R0,lrc), data=data)
summary(model.asymp) 
predict(model.asymp, newdata=newdata, type="response")[1]

beta <- coef(model.asymp)
cov.beta<- vcov(model.asymp)

beta.star<-rmvnorm(1000, mean = beta, sigma = cov.beta)
newdata <- data.frame(Nitrogen=150)

Asym<-beta.star[,1]
R0<-beta.star[,2]
lrc<-beta.star[,3]

mu.star<-Asym+(R0-Asym)*exp(-exp(lrc)*newdata$Nitrogen)

# 95% - Confidence Intervals
alpha <- 1-0.95

conf.lowerbound<-quantile(mu.star, c(alpha/2))
conf.upperbound<-quantile(mu.star, c(1-(alpha/2)))
conf.lowerbound
conf.upperbound

# 80% - Prediction Intervals
alpha <- 1-0.8

sigma2<-sigma(model.asymp)^2
yf.star<-rnorm(1000, mean=mu.star, sd=sqrt(sigma2))
pred.lowerbound<-quantile(yf.star, c(alpha/2))
pred.upperbound<-quantile(yf.star, c(1-(alpha/2)))
pred.lowerbound
pred.upperbound

################################################################################
# PROBLEM 3

data<-read.table("caffeine.txt", sep="\t", header=TRUE, dec=".")
attach(data)
head(data)

# Y = Caffeine
# X1 = Brand
# X2 = Formulation

library(ggplot2)
ggplot(data, aes(x=Brand, y=Caffeine, color=Formulation)) + geom_point()

# Candidate models:
#*****************************
# > LINEAR COVARIANCE MODELS
#   Linear model

#   Main effect model
model_main <- lm(Caffeine ~ factor(Brand) + factor(Formulation))
AIC(model_main) #1427.895
mean(residuals(model_main, type="response")^2) #21.7226




#   Main Interaction model
model_interaction <- lm(Caffeine ~ factor(Brand) * factor(Formulation))
AIC(model_interaction) #1374.177
mean(residuals(model_interaction, type="response")^2) #17.22208



## Interaction effect has the lowest AIC, but if we test both models

# H0 : Model M1|2 is the true model, main effect
# H1 : Model M12 is the true model. interaction 
anova(model_main, model_interaction, test="F")
anova(model_main, model_interaction, test="F")$"Pr(>F)"[2] < 0.05
# reject H0, therefor the Interaction effect model is the best model

# Let's check X1 and X2 significance
model_H0 <- lm(Caffeine ~ factor(Brand))
anova(model_H0, model_interaction, test="F")$"Pr(>F)"[2] < 0.05
# Reject H0, therefore, X2: Formulation is significant

model_H0 <- lm(Caffeine ~ factor(Formulation))
anova(model_H0, model_interaction, test="F")$"Pr(>F)"[2] < 0.05
# Reject H0, therefore, X1: Brand is significant






#*****************************
# > GENERALIZED LINEAR MODEL
# Assume Yi ??? N(µi, ??2)
library("ggpubr")
p1 <- ggplot(data, aes(x=Brand, y=Caffeine)) + 
  geom_point(aes(shape = Formulation, color=Formulation)) + 
  scale_shape_manual(values = c(24,25))

# Main effect normal(identity) model
model.main.ident<-glm(Caffeine~factor(Brand)+factor(Formulation), 
                    family=gaussian(link="identity"), data=data)
summary(model.main.ident)

p2 <- ggplot(data, aes(x=Brand, y=fitted(model.main.ident))) + 
  geom_point(aes(shape = Formulation, color=Formulation)) + 
  scale_shape_manual(values = c(24,25))

figure <- ggarrange(p1, p2, labels = c("Real values", "Fitted values"),
                    ncol = 1, nrow = 2)
figure

AIC(model.main.ident) #1427.895
mean(residuals(model.main.ident, type="response")^2) #21.7226

interaction.plot(Brand, Formulation, Caffeine)
interaction.plot(Brand, Formulation, fitted(model.main.ident, type="response"))



# Main effect log link model

model.main.log<-glm(Caffeine~factor(Brand)+factor(Formulation), 
                    family=gaussian(link="log"), data=data)
summary(model.main.log)
data.frame(data, fitted(model.main.log))

p2 <- ggplot(data, aes(x=Brand, y=fitted(model.main.log))) + 
  geom_point(aes(shape = Formulation, color=Formulation)) + 
  scale_shape_manual(values = c(24,25))

figure <- ggarrange(p1, p2, labels = c("Real values", "Fitted values"),
                    ncol = 1, nrow = 2)
figure

AIC(model.main.log) #1422.861
mean(residuals(model.main.log, type="response")^2) #21.27166

interaction.plot(Brand, Formulation, Caffeine)
interaction.plot(Brand, Formulation, fitted(model.main.log, type="response"))



# Inverse Gaussian main effect model

model.main.inverse<-glm(Caffeine~factor(Brand)+factor(Formulation), 
                    family=gaussian(link="inverse"), data=data)
summary(model.main.inverse)

p2 <- ggplot(data, aes(x=Brand, y=fitted(model.main.inverse))) + 
  geom_point(aes(shape = Formulation, color=Formulation)) + 
  scale_shape_manual(values = c(24,25))

figure <- ggarrange(p1, p2, labels = c("Real values", "Fitted values"),
                    ncol = 1, nrow = 2)
figure

AIC(model.main.inverse) #1417.453
mean(residuals(model.main.inverse, type="response")^2) #20.79773


model.inter.inverse<-glm(Caffeine~factor(Brand)*factor(Formulation), 
                        family=gaussian(link="inverse"), data=data)
AIC(model.inter.inverse) # 1374.177
mean(residuals(model.inter.inverse, type="response")^2) #20.79773

#interaction.plot(Brand, Formulation, Caffeine)
#interaction.plot(Brand, Formulation, fitted(model.main.inverse, type="response"))


# Inverse Gaussian interaction effect model

model.interaction.inverse<-glm(Caffeine~factor(Brand)*factor(Formulation), family=gaussian(link="inverse"), data=data)
summary(model.interaction.inverse)
AIC(model.interaction.inverse) #1374.177
mean(residuals(model.inter.inverse, type="response")^2) #17.22208

model.interaction.log<-glm(Caffeine~factor(Brand)*factor(Formulation), family=gaussian(link="log"), data=data)
summary(model.interaction.log)
AIC(model.interaction.log) #1374.177
mean(residuals(model.interaction.log, type="response")^2) #17.22208

model.interaction.identity<-glm(Caffeine~factor(Brand)*factor(Formulation), family=gaussian(link="identity"), data=data)
summary(model.interaction.identity)
AIC(model.interaction.identity) #1374.177
mean(residuals(model.interaction.identity, type="response")^2) #17.22208

# INVERSE GAUSSIAN MODEL

ig.1<-glm(Caffeine~factor(Brand)+factor(Formulation), family=inverse.gaussian(link="identity"), data=data)
AIC(ig.1)
mean(residuals(ig.1, type="response")^2)

ig.2<-glm(Caffeine~factor(Brand)+factor(Formulation), family=inverse.gaussian(link="log"), data=data)
AIC(ig.2)
mean(residuals(ig.2, type="response")^2)

ig.3<-glm(Caffeine~factor(Brand)+factor(Formulation), family=inverse.gaussian(link="inverse"), data=data)
AIC(ig.3)
mean(residuals(ig.3, type="response")^2)

# After trying all these models, the model that had the lowest AIC and MSE 
# is the Interaction Effect Gaussian Identity Link model
p1 <- ggplot(data, aes(x=Brand, y=Caffeine)) + 
  geom_point(aes(shape = Formulation, color=Formulation)) + 
  scale_shape_manual(values = c(24,25))
p2 <- ggplot(data, aes(x=Brand, y=fitted(model.interaction.identity))) + 
  geom_point(aes(shape = Formulation, color=Formulation)) + 
  scale_shape_manual(values = c(24,25))

figure <- ggarrange(p1, p2, labels = c("Real values", "Fitted values"),
                    ncol = 1, nrow = 2)
figure


# Linear models: 
AIC(model_interaction) #1374.177
mean(residuals(model_interaction, type="response")^2) #17.22208

# Generalized Linear Models:
AIC(model.main.inverse) #1417.453
mean(residuals(model.main.inverse, type="response")^2) #20.79773

# Inverse Gaussian
AIC(ig.1) #1417.453
mean(residuals(ig.1, type="response")^2) #20.79773



# b) 80% Prediction interval

model.interaction.identity
## prediction interval

newdata<-data.frame(Brand="Coke", Formulation="Diet")
pred<-predict(model.interaction.identity, newdata=newdata, type="response") #point prediction
pred

xf <- (c(1,0,0,0))
xf

Var.Yf<-summary(model.interaction.identity)$dispersion
D.f<-pred #estimate of expected value, point prediction
Var.ef<-Var.Yf+(D.f^2)*t(xf)%*%vcov(model.interaction.identity)%*%xf

lower.yf<-pred-qnorm(0.9)*sqrt(Var.ef)
upper.yf<-pred+qnorm(0.9)*sqrt(Var.ef)
c(lower.yf, upper.yf)

## CORREJIR CODIGO DE PREDICCION

xf<-t(t(c(1,0,0,0)))

pred<-t(xf)%*%coef(model.interaction.identity)
X<-model.matrix(model.interaction.identity)
sigma2<-summary(model.interaction.identity)$sigma^2
cov.error<-sigma2*(1+t(xf)%*%solve(t(X)%*%X)%*%xf)
cov.error

t<-qt(0.9, df=236)

lowerbound<-pred-t*sqrt(cov.error)
upperbound<-pred+t*sqrt(cov.error)
lowerbound
upperbound


# c) Test at 5% signi???cance level, is the explanatory variable 
# X1 = Brand statistically signi???cant variable

# Since we picked the Gaussian identity interaction effect model,
# we compare the two models

model_H0 <- glm(Caffeine~factor(Formulation), family=gaussian(link="identity"), data=data)
model_H1 <- glm(Caffeine~factor(Brand)*factor(Formulation), family=gaussian(link="identity"), data=data)

anova(model_H0, model_H1, test="F")
anova(model_H0, model_H1, test="F")$F[2]
anova(model_H0, model_H1, test="F")$"Pr(>F)"[2]
# Since p-value < 0.05, therefore we reject H0, thus, explanatory variable X1
# is statistically significant for the model.




