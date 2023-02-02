###########################
### PROBLEM SET 3
# PROBLEM 1

data<-read.table("galapagos.txt", sep="\t", dec=".", header=TRUE)
attach(data)
# Y = Species
# X1 = Area - Surface area of island, hectares,
# X2 = Elevation - Elevation in m,
# X3 = Nearest- Distance to closest island, km,
# X4 = Scruz- Distance from Santa Cruz Island, km,
# X5 = Adjacent- Area of closest island, hectares.


plot(Area, Species)
plot(Area, log(Species))
plot(log(Area), log(Species))

model_poisson_log<-glm(Species~ Area + Elevation + Nearest + Scruz + Adjacent, 
                       family=poisson(link="log"), data=data)
summary(model_poisson_log)



# a) MLE for expected value mu_i
newdata<-data.frame(Area=58.27, Elevation = 198, Nearest = 1.1, Scruz = 88.3, 
                    Adjacent = 0.57)
predict(model_poisson_log, newdata=newdata, type="response")

eta <- predict(model_poisson_log, newdata = newdata, type = "link", 
               interval = "confidence", level = 0.975, se.fit = TRUE)
lowerbound_link<-eta$fit-qnorm(0.975)*eta$se.fit
upperbound_link<-eta$fit+qnorm(0.975)*eta$se.fit

c(exp(lowerbound_link), exp(upperbound_link))

# b) 
model_H0 <- glm(Species~ Area, 
                family=poisson(link="sqrt"), data=data)
summary(model_H0)


plot(Area, fitted(model_H0, type="response"))
points(Area, Species)
lines(sort(Area), sort(fitted(model_H0, type="response")), lwd=4, col="red")


model_H1 <- glm(Species~ Area + Elevation + Nearest + Scruz + Adjacent,
                family=poisson(link="sqrt"), data=data)
summary(model_H1)

anova(model_H0, model_H1, test="Chi") # Chi squared to test poisson distribution
anova(model_H0, model_H1, test="Chi")$Deviance[2] #Phi
anova(model_H0, model_H1, test="Chi")$"Pr(>Chi)"[2]
# p-value 2.2e-16 < 0.05, Reject H0, therefore, variables
# X2, X3, X4, X5 are statistically significant

# c) Prediction Interval - Poisson exponential model
model_poisson_exp <- glm(Species~ log(Area) + log(Elevation) + log(Nearest) +
                           log(Scruz) + log(Adjacent), 
                         family=poisson(link="log"), data=data)
summary(model_poisson_exp)
plot(fitted(model_poisson_exp, type="response"), Species)

# Confidence interval
newdata<-data.frame(Area=58.27, Elevation = 198, Nearest = 1.1, Scruz = 88.3, 
                    Adjacent = 0.57)
predict(model_poisson_exp, newdata=newdata, type="response")

eta <- predict(model_poisson_exp, newdata = newdata, type = "link", 
               interval = "confidence", level = 0.9, se.fit = TRUE)
lowerbound_link<-eta$fit-qnorm(0.9)*eta$se.fit
upperbound_link<-eta$fit+qnorm(0.9)*eta$se.fit

exp(lowerbound_link)
exp(upperbound_link)

# prediction interval
newdata<-data.frame(Area=58.27, Elevation = 198, Nearest = 1.1, Scruz = 88.3, 
                    Adjacent = 0.57)
pred <- predict(model_poisson_exp, newdata=newdata, type="response")

t(t(coef(model_poisson_exp)))
xf <- t(cbind(1, log(58.27), log(198), log(1.1), log(88.3), log(0.57)))
#ANOTHER WAY to obtain xf
newdata_test<-data.frame(Area=58.27,Elevation=198,Nearest=1.1,Scruz=88.3,Adjacent=0.57)
xf_test<-t(cbind(1,log(newdata_test)))
xf_test


# CORRECT
Var.eYf<-pred*(1+pred*t(xf)%*%vcov(model_poisson_exp)%*%xf)
lower.Yf<-pred-qnorm(0.9)*sqrt(Var.eYf)
upper.Yf<-pred+qnorm(0.9)*sqrt(Var.eYf)
lower.Yf
upper.Yf





# d) 
library(MASS)
model_negbin_log <- glm.nb(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, 
                           link="log", data=data)
summary(model_negbin_log)

fitted(model_negbin_log, type="response")[1]
plot(fitted(model, type="response"), fitted(model.q, type="response"))

newdata<-data.frame(Area=25.09, Elevation = 346, Nearest = 0.6, Scruz = 0.6, 
                    Adjacent = 1.84)
pred <- predict(model_negbin_log, newdata=newdata, type="response")
pred


mean(residuals(model.NB,type="response")^2)
mean(residuals(model,type="response")^2)
mean(residuals(model.q,type="response")^2)

model.exponentialQ<-glm(Species~log(Area)+log(Elevation)+log(Nearest)+log(Scruz)+log(Adjacent), family=quasipoisson(link="log"),data=data)
summary(model.exponentialQ)

mean(residuals(model.exponentialQ,type="response")^2)

############################################################################
# PROBLEM 2

data<-read.table("chromoabnormal.txt", sep="\t", dec=".", header=TRUE)
attach(data)

# Y: ca
# t: cells
# X1: doseamt
# X2: doserate

# Ratio Z = Y/t = ca / cells

# a) MLE Expected value mu_i
model_poisson_log_ratio <- glm(ca ~ offset(log(cells))*doseamt*doserate,
                                family=poisson(link="log"), data=data)

summary(model_poisson_log_ratio)

newdata<-data.frame(doseamt=4, doserate = 0.75, cells = 64070) 
pred<-predict(model_poisson_log_ratio, newdata=newdata, type="response") # E(Yi)
pred

#Same resutl with:
xf<-t(cbind(1,4,0.75,4*0.75))
exp(log(64070)+t(xf)%*%coef(model_poisson_log_ratio))

# extra plots

y<-ca/cells

plot(ca/cells~doseamt)
plot(ca/cells~doserate)
coplot(ca/cells~doseamt| doserate)

# b) MLE prediction for the ratio and Prediction intervals for the ratio
newdata<-data.frame(doseamt=4, doserate = 0.75, cells = 64070) 
pred<-predict(model_poisson_log_ratio, newdata=newdata, type="response")

ratio.estimate<-pred/newdata$cells # E(Zi) = E(Yi / t_i) = Mu_i / t_i
ratio.estimate 

# Z_f = Y_f/t_f
ratio.prediction<-pred/newdata$cells
ratio.prediction

t(t(coef(model_poisson_log_ratio)))
model.matrix(model_poisson_log_ratio)
xf<-t(cbind(1, 4, 0.75, 4*0.75))

Var.eYf<-pred*(1+pred*t(xf)%*%vcov(model_poisson_log_ratio)%*%xf)
lower.Yf<-pred-qnorm(0.9)*sqrt(Var.eYf)
upper.Yf<-pred+qnorm(0.9)*sqrt(Var.eYf)
cbind(lower.Yf, upper.Yf)

Var.eZf<-((1/newdata$cells)^2)*Var.eYf
lower.Zf<-ratio.prediction-qnorm(0.9)*sqrt(Var.eZf)
upper.Zf<-ratio.prediction+qnorm(0.9)*sqrt(Var.eZf)
cbind(lower.Zf, upper.Zf)

# c) 
model_quasi_H0 <- glm(ca ~ offset(log(cells)) + doseamt,
                      family=quasipoisson(link="log"), data=data)
model_quasi_H1 <- glm(ca ~ offset(log(cells)) + doseamt * doserate,
                      family=quasipoisson(link="log"), data=data)
anova(model_quasi_H0, model_quasi_H1, test="F")
anova(model_quasi_H0, model_quasi_H1, test="F")$F[2]
anova(model_quasi_H0, model_quasi_H1, test="F")$"Pr(>F)"[2] < 0.05
# Reject H0, therefore X2 is statistically significant

# d) Models:
library(MASS)
model_poisson_log <- glm(ca ~ offset(log(cells)) + doseamt + doserate,
                         family=poisson(link="log"), data=data)
model_quasi_log <- glm(ca ~ offset(log(cells)) + doseamt + doserate,
                       family=quasipoisson(link="log"), data=data)
summary(model_poisson_log)$dispersion
summary(model_quasi_log)$dispersion
model_negbin_log <- glm.nb(ca ~ offset(log(cells)) + doseamt + doserate, 
                           link="log", data=data)

AIC(model_poisson_log) # 464.3543
AIC(model_quasi_log) # NA
AIC(model_negbin_log) # 277.7973
# The model with the smallest AIC value is Negative Binomial 

# MSE
sum((ca - fitted(model_poisson_log, type="response"))^2)/27 # 1035.537
sum((ca - fitted(model_quasi_log, type="response"))^2)/27 # 1035.537
sum((ca - fitted(model_negbin_log, type="response"))^2)/27 # 1389.629
# The model with the lowest MSE value is Poisson model and Quasi Poisson model

coplot(fitted(model_poisson_log, type="response") ~ doseamt | doserate )
coplot(fitted(model_quasi_log, type="response") ~ doseamt | doserate )
coplot(fitted(model_negbin_log, type="response") ~ doseamt | doserate )

# model_poisson_log
fit<-fitted(model_poisson_log, type="response")
plot(main="Log Poisson model vs Fitted values", doserate, ca, pch=19, col="blue")
points(doserate, fit, pch=15, cex=1,col="green")
points(doseamt, ca, pch=19,  col="orange")
points(doseamt, fit, pch=15, cex=1,col="yellow")
legend("topleft", legend=c("Amount of dose in Grays", "Fitted values for dose", 
                "Rate of dose in Grays/hour", "Fitted values for rate"),
       col=c("blue", "green", "orange", "yellow"), lty=1, cex=1)

# model_quasi_log
fit<-fitted(model_quasi_log, type="response")
plot(main="Log Quasi Poisson model vs Fitted values", doserate, ca, pch=19, col="blue")
points(doserate, fit, pch=15, cex=1,col="green")
points(doseamt, ca, pch=19,  col="orange")
points(doseamt, fit, pch=15, cex=1,col="yellow")
legend("topleft", legend=c("Amount of dose in Grays", "Fitted values for dose", 
                           "Rate of dose in Grays/hour", "Fitted values for rate"),
       col=c("blue", "green", "orange", "yellow"), lty=1, cex=1)

# model_negbin_log
fit<-fitted(model_negbin_log, type="response")
plot(main="Log Negative Binomial model vs Fitted values", doserate, ca, pch=19, col="blue")
points(doserate, fit, pch=15, cex=1,col="green")
points(doseamt, ca, pch=19,  col="orange")
points(doseamt, fit, pch=15, cex=1,col="yellow")
legend("topleft", legend=c("Amount of dose in Grays", "Fitted values for dose", 
                           "Rate of dose in Grays/hour", "Fitted values for rate"),
       col=c("blue", "green", "orange", "yellow"), lty=1, cex=1)

# Pearson's residuals

plot(fitted(model_poisson_log, type="response"), residuals(model_poisson_log, type="pearson"), main="Residuals - Model Poisson")
plot(fitted(model_quasi_log, type="response"), residuals(model_quasi_log, type="pearson"), main="Residuals - Model Quasi Poisson")
plot(fitted(model_negbin_log, type="response"), residuals(model_negbin_log, type="pearson"), main="Residuals - Model Negative Binomial")

shapiro.test(residuals(model_poisson_log, type="pearson"))[2] 
# p.value = 0.2114577, do not reject H0, residuals are distributed according to Normal dist

shapiro.test(residuals(model_quasi_log, type="pearson"))[2] 
# p.value = 0.2114577, do not reject H0, residuals are distributed according to Normal dist

shapiro.test(residuals(model_negbin_log, type="pearson"))[2]
# p.value = 0.01098415, reject H0, residuals are not distributed according to Normal dist

# AIC: Smallest value is Negative binomial
# MSE: smalles value is Poisson and Quasi Poisson model
# Pearson's residuals: Poisson and Quasi Poisson follow normal distribution
# According to these values, we could choose the model Poisson


# Teacher's ANSWER:
model.H1<-glm(ca~offset(log(cells))+doseamt*doserate, family=quasipoisson(link="log"), data=data)

MSE.i<-mean((ca-predict(model, newdata=data, type="response"))^2)
MSE.ii<-mean((ca-predict(model.H1, newdata=data, type="response"))^2)
library(MASS)
model.NB<-glm.nb(ca~offset(log(cells))+doseamt*doserate, data=data)
summary(model.NB)
MSE.iii<-mean((ca-predict(model.NB, newdata=data, type="response"))^2)

MSE.i
MSE.ii
MSE.iii

AIC(model)  ## Do not use these
AIC(model.H1) ##  !!!
AIC(model.NB) ## !!!

plot(fitted(model.H1, type="response"), residuals(model.H1, type="response"))
plot(fitted(model.NB, type="response"), residuals(model.NB, type="response"))

plot(fitted(model, type="response"), residuals(model, type="pearson")^2)
plot(fitted(model.H1, type="response"), residuals(model.H1, type="pearson")^2)
plot(fitted(model.NB, type="response"), residuals(model.NB, type="pearson")^2)

# ii. - Quasi-Poisson.
