# PROBLEM SET 2
# STATISTICAL MODELING I

# PROBLEM 1
# dataset: canoladiesel

data<-read.table("canoladiesel.txt", sep="\t", dec=".", header=TRUE)
attach(data)
# X1: Time (15, 30, 45 min)
# X2: Temperature (240, 255, 270 C)
# X3: Methanol (1, 1.5, 2)
# Y: Yield

# a) Assume Yi ~ N(mu_i, sigma^2)
# Model_inverse : 1/mu_i = B0 + B1xi

normal_1_inverse<-glm(Yield~Time, family=gaussian(link="inverse"), data=data)
summary(normal_1_inverse)

plot(Time, Yield)
newdata<-data.frame(Time=40)
predict(normal_1_inverse, newdata=newdata, type="response")

# b) Assume Yi ~ Gamma(mu_i, phi)
# Models:
# M1_identity
# M1_inverse
# M1_log

gamma_1_identity <- glm(Yield ~ Time, family=Gamma(link="identity"), data=data)
gamma_1_inverse <- glm(Yield ~ Time, family=Gamma(link="inverse"), data=data)
gamma_1_log <- glm(Yield ~ Time, family=Gamma(link="log"), data=data)

AIC(gamma_1_identity)
AIC(gamma_1_inverse)
AIC(gamma_1_log)
# The model that has the lowest AIC value is Gamma log, therefore, according
# to AIC, the model M1_identity fits the best to the data


# c) Assume Yi ~ Gamma(mu_i, phi), 95% confidence intervals for mu_i

gamma_main_123_log <- glm(Yield ~ Time + Temp + Methanol, 
                          family = Gamma(link = "log"), data=data)

new_data <- data.frame(Time = c(40), Temp = c(260), Methanol = c(1.75))
mu_i_hat<- predict(gamma_main_123_log, newdata = new_data, type = "response", interval = "confidence", level = 0.975, se.fit = TRUE)

## THIS IS WRONG!!!!
lowerbound<-mu_i_hat$fit-qnorm(0.975)*mu_i_hat$se.fit
upperbound<-mu_i_hat$fit+qnorm(0.975)*mu_i_hat$se.fit

#### CORRECT: WE FIRST calculate ETA_i for link functions and then move to the 
#    normal mu_i, by using inverse function g^-1
eta <- predict(gamma_main_123_log, newdata = new_data, type = "link", se.fit = TRUE)
lowerbound_link<-eta$fit-qnorm(0.975)*eta$se.fit
upperbound_link<-eta$fit+qnorm(0.975)*eta$se.fit

lower <- exp(lowerbound_link)
upper <- exp(upperbound_link)
c(lower,pred,upper)

# d) Assume Yi ~ Gamma(mu_i, phi), test at 5% significance level
# If X3 is statistically significant in model M_main_123_log

# H0 model M_main_3_log 
gamma_main_3_log <- glm(Yield ~ Time + Temp, family = Gamma(link="log"), data=data)

# H1 model
gamma_main_123_log <- glm(Yield ~ Time + Temp + Methanol, 
                          family = Gamma(link = "log"), data=data)


# ANOVA
anova(gamma_main_3_log, gamma_main_123_log, test="F")
anova(gamma_main_3_log, gamma_main_123_log, test="F")$"Pr(>F)"[2]

# Is p-value > alpha=0.05?
(anova(gamma_main_3_log, gamma_main_123_log, test="F")$"Pr(>F)"[2])<0.05
# Reject H0, therefore, X3 is not statistically significant.

# e) Consider model M_main_123_log: 
# competing distributions:
# Yi ~ N(mu_i, sigma^2)
normal_main_123_log <- glm(Yield ~ Time + Temp + Methanol, 
                           family = gaussian(link = "log"), data=data)
# Yi ~ Gamma(mu_i, phi)
gamma_main_123_log <- glm(Yield ~ Time + Temp + Methanol, 
                          family = Gamma(link = "log"), data=data)
# Yi ~ IG(mu_i, phi)
inv_gaussian_main_123_log <- glm(Yield ~ Time + Temp + Methanol, 
                                 family = inverse.gaussian(link = "log"), data=data)

# Pearson's residuals, should follow a standard normal distribution
plot(fitted(normal_main_123_log, type="response"), residuals(normal_main_123_log, type="pearson"))
plot(fitted(gamma_main_123_log, type="response"), residuals(gamma_main_123_log, type="pearson"))
plot(fitted(inv_gaussian_main_123_log, type="response"), residuals(inv_gaussian_main_123_log, type="pearson"))

# Shapiro Wilks test
shapiro.test(residuals(normal_main_123_log, type="pearson")) 
# p-value = 0.004203, reject H0, residuals do not fit a normal distribution with 95% confidence.

shapiro.test(residuals(gamma_main_123_log, type="pearson")) 
# p-value = 0.2114, do not reject H0, residuals are distributed according to Normal dist

shapiro.test(residuals(inv_gaussian_main_123_log, type="pearson")) 
# p-value = 0.9483, do not reject H0, residuals are distributed according to Normal dist

shapiro.test(residuals(normal_main_123_log, type="pearson"))[2] < 0.05
shapiro.test(residuals(gamma_main_123_log, type="pearson"))[2] < 0.05
shapiro.test(residuals(inv_gaussian_main_123_log, type="pearson"))[2] < 0.05

# Therefore, the models Yi ~ Gamma(mu_i, phi) and Yi ~ IG(mu_i, phi) have residuals that
# statistically fit a normal distribution, IG dist has the highest p-value

# If we check their AIC value:
AIC(gamma_main_123_log) # 121.5555
AIC(inv_gaussian_main_123_log) # 128.6878
AIC(normal_main_123_log) # 142.6993
# Yi ~ Gamma(mu_i, phi) has the lowest AIC model

# If we check the MSE
mean(residuals(gamma_main_123_log, type="pearson")^2) # MSE = 0.1345295
mean(residuals(inv_gaussian_main_123_log, type="pearson")^2) # MSE = 0.018071


# Taking into considerations the pearson's residuals plot, the shapiro test, AIC
# value and MSE value, the model that best fit the data is Yi ~ IG(mu_i, phi)

################################################################################
# PROBLEM 2
# dataset: ratstime.txt

data<-read.table("ratstime.txt", sep="\t", dec=".", header=TRUE)
attach(data)
# X1: poison (I, II, III)
# X2: treat (A, B, C, D)
# Y: Time

# a) Main Effect model:
# mu_jh = B_0 + B_j + Alpha_h
# Distributional assumptions
# Yi ~ Normal(mu_jh, sigma^2)
# Yi ~ Gamma(mu_jh, Phi)
# Yi ~ IG(mu_jh, Phi)

# Most suitable?

# Main Effect normal(identity) model Yi ~ Normal(mu_jh, sigma^2)
normal_main_jh <- glm(time~factor(poison)+factor(treat), family=gaussian(link="identity"), data=data)
summary(normal_main_jh)
interaction.plot(poison, treat, fitted(normal_main_jh, type="response"))

# Main Effect (identity) model Yi ~ Gamma(mu_jh, Phi)
gamma_main_jh <- glm(time~factor(poison)+factor(treat), family=Gamma(link="identity"), data=data)
summary(gamma_main_jh)
interaction.plot(poison, treat, fitted(gamma_main_jh, type="response"))

# Main Effect (identity) model Yi ~ IG(mu_jh, Phi)
ig_main_jh <- glm(time~factor(poison)+factor(treat), family=inverse.gaussian(link="identity"), data=data)
summary(ig_main_jh)
interaction.plot(poison, treat, fitted(ig_main_jh, type="response"))

# AIC values
AIC(normal_main_jh) # -33.21821
AIC(gamma_main_jh) # -62.23033
AIC(ig_main_jh) # -71.183
# The lowest AIC value is with Yi ~ IG(mu_jh, Phi)

# MSE values
mean(residuals(normal_main_jh, type="response")^2) # MSE = 0.02189297
mean(residuals(gamma_main_jh, type="response")^2) # MSE = 0.02379278
mean(residuals(ig_main_jh, type="response")^2) # MSE = 0.02535005
# The lowest MSE value is with Yi ~ Normal(mu_jh, sigma^2)

# Residuals plot
plot(fitted(normal_main_jh, type="response"), residuals(normal_main_jh, type="pearson")^2)
plot(fitted(gamma_main_jh, type="response"), residuals(normal_main_jh, type="pearson")^2)
plot(fitted(ig_main_jh, type="response"), residuals(normal_main_jh, type="pearson")^2)

# Shapiro normality test for residuals
shapiro.test(residuals(normal_main_jh, type="pearson"))
shapiro.test(residuals(gamma_main_jh, type="pearson"))
shapiro.test(residuals(ig_main_jh, type="pearson"))

# Best AIC: Yi ~ IG(mu_jh, Phi)
# Best MSE: Yi ~ Normal(mu_jh, sigma^2)
# Best residuals plot: None
# Normality test: None are normal
# Based on this, since the MSE valus are not that different with each other, we
# pick the model that has the lowest AIC, which is this case has a bigger AIC
# value compared to the others.

# Although Yi ~ IG(mu_jh, Phi) i might not be the best model to fit the data, 
# it is the most suitable in this case.

# b) Assume Yi ~ Gamma(mu_jh, Phi), based on MSE
# mu_jh = B_0 + B_j + Alpha_h
# log(mu_jh) = B_0 + B_j + Alpha_h
# 1/mu_jh = B_0 + B_j + Alpha_h

gamma_identity <- glm(time~factor(poison)+factor(treat), family=Gamma(link="identity"), data=data)
gamma_log <- glm(time~factor(poison)+factor(treat), family=Gamma(link="log"), data=data)
gamma_ig <- glm(time~factor(poison)+factor(treat), family=Gamma(link="inverse"), data=data)

interaction.plot(poison, treat, fitted(gamma_identity, type="response"))
interaction.plot(poison, treat, fitted(gamma_log, type="response"))
interaction.plot(poison, treat, fitted(gamma_ig, type="response"))

mean(residuals(gamma_identity, type="response")^2) # MSE = 0.02379278
mean(residuals(gamma_log, type="response")^2) # MSE = 0.01939997
mean(residuals(gamma_ig, type="response")^2) # MSE = 0.01900666

# The lowest AIC value is with Gamma inverse link function

# c) Assume Yi ~ IG(mu_jh, Phi)
# H0: log(mu_jh) = B_0 + B_j + Alpha_h
# H1: log(mu_jh) = B_0 + B_j + Alpha_h + Gammajh

model.H0<-glm(time~factor(poison) + factor(treat), family=inverse.gaussian(link="log"), data=data)
model.H1<-glm(time~factor(poison) + factor(treat) + factor(poison):factor(treat), family=inverse.gaussian(link="log"), data=data)
anova(model.H0, model.H1, test="F")
anova(model.H0, model.H1, test="F")$F[2]
anova(model.H0, model.H1, test="F")$"Pr(>F)"[2]
anova(model.H0, model.H1, test="F")$"Pr(>F)"[2] < 0.05
# Do not reject H0, therefore, there is statistical evidence to say that the model 
# that fits the data best is log(mu_jh) = B_0 + B_j + Alpha_h



# If we were to fit a interaction effect model


# d) Assume Yi ~ Gamma(mu_jh, Phi), 80% PREDICTION interval for y_f
# when xf1 = II, xf2 = B
main_gamma_log <- glm(time~factor(poison) + factor(treat), family=Gamma(link="log"), data=data)

new_data <- data.frame(poison=c("II"), treat=c("B"))
pred <- predict(main_gamma_log, newdata=new_data, type="response") # point prediction
pred

## WRONG:
xf<-cbind(model.matrix(main_gamma_log)[1,])
Var.Yf<-summary(main_gamma_log)$dispersion
D.f<-pred #estimate of expected value, point prediction
Var.ef<-Var.Yf+(D.f^2)*t(xf)%*%vcov(main_gamma_log)%*%xf

# CORRECT:
model.matrix(main_gamma_log)
xf<-cbind(c(1,1,0,1,0,0))
phi<-summary(main_gamma_log)$dispersion
Var.Yf<- phi*(pred^2)
D.f<-pred 
Var.ef<-Var.Yf+(D.f^2)*t(xf)%*%vcov(main_gamma_log)%*%xf

lower.yf<-pred-qnorm(0.9)*sqrt(Var.ef)
upper.yf<-pred+qnorm(0.9)*sqrt(Var.ef)
lower.yf
upper.yf

# e) Assume Yi ~ Gamma(mu_jh, Phi)
summary(main_gamma_log)
newdata<- data.frame(poison=c("I", "II"), treat=c("D", "B"))

pred<-predict(main_gamma_log, newdata=newdata, type="response")
pred #point predictions of the two sets of variables

t(t(coef(main_gamma_log)))

x1f<-cbind(c(1,0,0,0,0,1)) ### I and D
x2f<-cbind(c(1,1,0,1,0,0)) ## II and B
Xf<-t(cbind(x1f,x2f))

k<-cbind(c(-1,1))

phi<-summary(main_gamma_log)$dispersion #variance of the model
Var.Y1f<-phi*(pred[1]^2) # variance of Gamma distributions is Var(Yi) = Phi*Mu_i^2
Var.Y2f<-phi*(pred[2]^2)

D.f<-diag(pred)   ####!!!!!!!!! Depending on the link function, if we have identity
#                               link function, then the partial derivatives is 1
#                               so it would be the identity matrix, wouldn't cause
#                               any effect on the result
#                               If it's not identity link, it is as this example

Var.ef<-Var.Y1f+Var.Y2f+t(k)%*%D.f%*%Xf%*%vcov(main_gamma_log)%*%t(Xf)%*%D.f%*%k

lower.diff<-(pred[2]-pred[1])-qnorm(0.9)*sqrt(Var.ef)
upper.diff<-(pred[2]-pred[1])+qnorm(0.9)*sqrt(Var.ef)
lower.diff
upper.diff

exp(lower.diff)
exp(upper.diff)



betahat<-cbind(coef(main_gamma_log))
betahat

pred2<-(t(x2f)-t(x1f))%*%betahat
pred[2]-pred[1]

pred[1]
t(x1f)%*%betahat
exp(t(x1f)%*%betahat)

pred[2]
t(x2f)%*%betahat
exp(t(x2f)%*%betahat)


