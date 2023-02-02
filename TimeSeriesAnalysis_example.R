library(TSA)
library(tseries)
library(forecast)
library(olsrr)
library(lmtest)

# Take-home assignment

# Part 1

# a)

gdp_data <-read.table("C:\\Users\\ednat\\Documents\\TAMPERE UNIVERSITY\\MSc. Computing Sciencie - Statistical Data Analytics\\Period I\\DATA.STAT.780 Time Series Analysis\\GDPd.txt")
gdp = ts(gdp_data, freq=12, start=1980)

training_set_data = as.vector(gdp)[1:190]
training_set = ts(training_set_data, freq=12, start=1980)

test_set_data = as.vector(gdp)[191:216]
test_set =  ts(test_set_data, freq=12, start=c(1995,11))

plot(training_set, main = "Training set data", ylab="Gross Domestic  Product", col="deeppink4", xaxp = c(1980, 1997, 17))
# The TS seems to have an upward trend, there is also seasonality every year, not a lot of change in variability

# b) Seasonal differencing
training_set_seasonal_diff = diff(training_set, lag=12)
plot(training_set_seasonal_diff)

#___ Visual criteria
acf(as.vector(training_set_seasonal_diff), lag.max = 32, main="Sample ACF of Seasonal differencing of training set", col="red4") 
# Seasonal lags: no spikes
# Simple lags: slow decrease suggesting AR, unless MA(8) high order

pacf(as.vector(training_set_seasonal_diff), lag.max = 32, main="Sample PACF of Seasonal differencing of training set", col="red4")
# Seasonal lags: spikes at lag 12, 24
# Simple lags: spike at lag 1

#___ Armasubsets
suggest_order = armasubsets(y=training_set, nar=6, nma=6, y.name="LD(SC)", ar.method="ols")
plot(suggest_order)

suggest_order_diff = armasubsets(y=training_set_seasonal_diff, nar=6, nma=6, y.name="LD(SC)", ar.method="ols")
plot(suggest_order_diff)
# Suggested models are: AR(1), AR(4), AR(5), ARMA(1,2)

#___auro.arima
auto.arima(training_set,ic="aic")
# Suggested model is ARIMA(2,0,2)(1,1,0)[12] with drift 
auto.arima(training_set,ic="bic")
# Suggested model is ARIMA(1,0,0)(1,1,0)[12] with drift

# Candidate models:
# SARIMA(1,0,0)(2,1,0)[12]
# SARIMA(2,0,2)(1,1,0)[12]
# SARIMA(1,0,0)(1,1,0)[12]
# SARMA(1,1,3)[12]
# SARMA(1,1,0)[12]
# SARMA(3,1,3)[12]

# c) 
#checking aic for each candidate
model1 = arima(training_set, order=c(1,0,0), seasonal = list(order=c(2,1,0),period=12), method="ML") # aic = 970.63
model2 = arima(training_set, order=c(2,0,2), seasonal = list(order=c(1,1,0),period=12), method="ML") # aic = 966.71
model3 = arima(training_set, order=c(1,0,0), seasonal = list(order=c(1,1,0),period=12), method="ML") # aic = 973.69
model4 = arima(training_set, order=c(0,0,0), seasonal = list(order=c(1,1,3),period=12), method="ML") # aic = 1165.76
model5 = arima(training_set, order=c(0,0,0), seasonal = list(order=c(1,1,0),period=12), method="ML") # aic = 1172.33
model6 = arima(training_set, order=c(0,0,0), seasonal = list(order=c(3,1,3),period=12), method="ML") # aic = 1154.02


#checking the residuals of the candidate models with the minimum is candidate model 2
sarima_fit = arima(training_set, order=c(2,0,2), seasonal = list(order=c(1,1,0),period=12), method="ML")

# Doing residual diagnostics for the frst three candidate models 


#______MODEL 1  SARIMA(2,0,2)(1,1,0)[12]
checkresiduals(model1) 
autoplot(model1)
qqnorm(rstandard(model1))
qqline(rstandard(model1))
tsdiag(model1, gof=20, omit.initial=F)




#_____MODEL 2  SARIMA(2,0,2)(1,1,0)[12]
checkresiduals(sarima_fit) 

#n=190
#m=12
#checkresiduals(sarima_fit, lag=min(2*m, n/5)) 

# the residuals look okay, the ACF has a few spikes out fo the bound but very 
# close to the limit, Histogram looks according to Normal distribution 
autoplot(model2)
qqnorm(rstandard(model2))
qqline(rstandard(model2))
tsdiag(model2, gof=20, omit.initial=F)

ols_test_normality(rstandard(model2)) # P
shapiro.test(rstandard(model2)) #p-value = 6.723e-05, reject H0, residuals are not normal
Box.test(rstandard(model2),lag=24, fitdf=20, type="Ljung-Box")

Box.test(residuals(model2), 6)

#______MODEL 3
model3
checkresiduals(model3) 

autoplot(model3)
qqnorm(rstandard(model3))
qqline(rstandard(model3))
tsdiag(model3, gof=20, omit.initial=F)

ols_test_normality(rstandard(model3)) # P
shapiro.test(rstandard(model3)) #p-value = 6.723e-05, reject H0, residuals are not normal
Box.test(rstandard(model3),lag=24, fitdf=20, type="Ljung-Box")

Box.test(residuals(model3), 6)




# d
# The best model to choose is # SARIMA(2,0,2)(1,1,0)[12] according to aic value and 
# the residuals look okay.

#_______________________________________________________________________________
# Part 2
# a)
t = seq(1:190)
model_fit = lm(training_set~t+sin(2*pi*t/(12))+cos(2*pi*t/(12)))

plot(model_fit$fitted)
model_fit$fitted.values

plot(training_set_data, main = "Training set", ylab="Gross Domestic  Product", col="purple4", xaxp = c(1980, 1997, 17), type="l")
lines(seq(1:190), model_fit$fitted,col="red")
#Ask hyon about this plot


# b)
residuals = model_fit$residuals
acf(residuals) #seems to have an alternating decay 
pacf(residuals) #spikes at lag 1, maybe lag 2 and 3, lag 5, 8, 9
#Suggested model AR(1), AR(2)


suggest_order = armasubsets(y=residuals, nar=6, nma=6, y.name="LD(SC)", ar.method="ols")
plot(suggest_order) #ARMA(1,3), ARMA(5,3)
auto.arima(residuals, ic='aic') #suggests ARMA(4,3)


arima(residuals, order=c(0,0,1), method="ML") # aic = 1172.02
arima(residuals, order=c(1,0,3), method="ML") # aic = 1130.95
arima(residuals, order=c(5,0,3), method="ML") # aic = 1087.21
arima(residuals, order=c(4,0,3), method="ML") # aic = 1097.33

arma53 = arima(residuals, order=c(5,0,3), method="ML")

checkresiduals(arma53)
autoplot(arma53)
qqnorm(rstandard(arma53))
qqline(rstandard(arma53))

tsdiag(arma53, gof=20, omit.initial=T)

#H0: Data are normally distributed
shapiro.test(rstandard(arma53)) # p-value = 0.002994
#Reject H0, the residuals are not normally distributed

# H0:the sequence is a random sequence
runs(rstandard(arma53)) # p-value = 0.351
# Accept H0, residuals are random


#_______________________________________________________________________________
# Part 3
# a)
model.predictors = predict(model2, n.ahead = 25)


sarima100_110_12

#b)
#predictions
sarima100_110_12.predictors = predict(sarima100_110_12, n.ahead = 25)

predict_values = round(sarima100_110_12.predictors$pred,3)
predict_error = round(sarima100_110_12.predictors$se,3)

#predictions bounds
lower_bound = predict_values - (qnorm(0.975, 0, 1)*predict_error)
upper_bound = predict_values + (qnorm(0.975, 0, 1)*predict_error)

# series plot and predictions
plot(test_set_data, type="l", ylim = c(100,180), main = "Part 1 - model forecast")
lines(y=as.vector(lower_bound), x=1:25, lwd=2, col="mediumvioletred", lty="dashed")
lines(y=as.vector(upper_bound), x=1:25, lwd=2, col="mediumvioletred", lty="dashed")
lines(predict_values, x=1:25, type="b", pch=16, col= "red" )


# c) 

#predictions
# c) 

#predictions

# Deterministic Model of part 2
arma53

#Estimates of the model
B0 = 93.5032 
B1 = 0.2046
B =  -2.6228
A = -6.0246 

t_predict = (191:215)
mu_hat = B0 + (t_predict*B1) + (A*cos(pi*(t_predict/6))) + (B*sin(pi*(t_predict/6)))
mu_hat_ts = ts(mu_hat,freq=12, start=c(1995,11))


# ARMA model from the residuals 
arma53_predict = predict(arma53, n.ahead = 25)
arma53_predict_25 = arma53_predict$pred
arma53_predict_25_error = arma53_predict$se

arma53_predict_25 + mu_hat
part2_model_predictors = as.vector(arma53_predict_25) + mu_hat


#predictions bounds
lower_bound_model_part_2 = part2_model_predictors - (qnorm(0.975, 0, 1)*arma53_predict_25_error)
upper_bound_model_part_2 = part2_model_predictors + (qnorm(0.975, 0, 1)*arma53_predict_25_error)

# series plot and predictions
plot(test_set_data, type="l", ylim = c(100,180),  main = "PayPal_ Detalles de la transacción.pdfrt 2 - model forecast")
lines(y=as.vector(lower_bound_model_part_2), x=1:25, lwd=2, col="mediumvioletred", lty="dashed")
lines(y=as.vector(upper_bound_model_part_2), x=1:25, lwd=2, col="mediumvioletred", lty="dashed")
lines(part2_model_predictors, x=1:25, type="b", pch=16, col= "red" )
legend("topleft", legend=c("Test data", "Predicted values","Bounds"), lty=c(1,4,2), col=c("black", "red", "mediumvioletred"), lwd =2, cex=0.8)


# Comparison using MSE of each model
test_set_data_25 = as.vector(gdp)[191:215]
MSE_part1_model = (1/25)*sum((test_set_data_25 - sarima_predict_values)^2) # 45.50882
MSE_part2_model = (1/25)*sum((test_set_data_25 - part2_model_predictors)^2) #3 4.01728











t_predict = (191:215)
ts_t_predict = ts(t_predict, freq=12, start=c(1995,11))
mu_hat_ts = ts(mu_hat,freq=12, start=c(1995,11))
ts_predicted = ts(part2_model_predictors, freq=12, start=c(1995,11))

plot(test_set, type="l", ylim = c(100,180), main = "Part 1 - model forecast", xaxp = c(1996, 1998, 2))
lines(y=sarima_lower_bound, x=ts_t_predict, lwd=2, col="mediumvioletred", lty="dashed")
lines(y=sarima_upper_bound, x=ts_t_predict, lwd=2, col="mediumvioletred", lty="dashed")
lines(sarima_predict_values, x=1:25, type="b", pch=16, col= "red" )
legend("topleft", legend=c("Test data", "Predicted values","Bounds"), lty=c(1,4,2), col=c("black", "red", "mediumvioletred"), lwd =2, cex=0.8)





t_predict = (191:215)
ts_t_predict = ts(t_predict, start=191)
mu_hat_ts = ts(mu_hat, start=191)
ts_predicted = ts(part2_model_predictors, start=191)

ts_sarima_lower_bound = ts(as.vector(sarima_lower_bound), start=191)
ts_sarima_upper_bound = ts(as.vector(sarima_upper_bound), start=191)
ts_test_set_data = ts(test_set_data, start=191)

plot(ts_test_set_data, type="l", ylim = c(100,180), main = "Part 1 - model forecast")
lines(y=ts_sarima_lower_bound, x=191:215, lwd=2, col="mediumvioletred", lty="dashed")
lines(y=ts_sarima_upper_bound, x=191:215, lwd=2, col="mediumvioletred", lty="dashed")
lines(ts_predicted, x=191:215, type="b", pch=16, col= "red" )
legend("topleft", legend=c("Test data", "Predicted values","Bounds"), lty=c(1,4,2), col=c("black", "red", "mediumvioletred"), lwd =2, cex=0.8)





# d)