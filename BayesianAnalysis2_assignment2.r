# PROBLEM 1
# coagulation time, Yij ??? Normal(µi, ?? ).
# a)

library(rjags)

cat( "
model
{
# Likelihood
  for (i in 1:k)
  {
    for (j in 1:n[i]) 
    {
        y[i,j]  ~ dnorm(mu[i], tauinv) # tauinve=1/variance=1/tau
    } 
    mu[i]  ~ dnorm(phi, gaminv) #1/gamma
  }
  
# Priors
    phi  ~ dnorm(60, 0.001) # Variance = 1000 small values for it to be noninformative
    tauinv   ~ dgamma(102, 202)
    gaminv   ~ dgamma(6, 20) # non informative prior, commonly used priors 
    tau <- 1/tauinv # the ones we put in coagulation.par
    gam <- 1/gaminv # the ones we put in coagulation.par
}",
  file="coagulation.jag" )

Data = c(62, 60, 63, 59, NA, NA, NA, NA,    #diet1 n=4
         63,67,71,64,65,66, NA, NA,         #diet2 n=4  
         68,66,71,67,68,68,NA, NA,          #diet3 n=6
         52,62,60,61,63,64,63,59)           #diet4 n=8

Data = matrix(Data, nrow=4, byrow=T) #Convert to a matrix form
n=c(4,6,6,8) #Sample sizes for each treatment

coagulation.data = list(y=Data, n=n, k=4)
coagulation.j = jags.model(file="coagulation.jag", data=coagulation.data, n.chains=3,
                           n.adapt=2000)
coagulation.par = c("tau", "gam")

coagulation.res = coda.samples(coagulation.j, var = coagulation.par, n.iter = 10000, thin = 10 )
summary(coagulation.res)

plot(coagulation.res)


# b) Modify the model so that each µi has its own variance parameter:

cat( "
model
{
# Likelihood
  for (i in 1:k)
  {
    for (j in 1:n[i]) 
    {
        y[i,j]  ~ dnorm(mu[i], tauinv) # tauinv=1/variance=1/tau
    } 
    mu[i]  ~ dnorm(phi, gaminv[i]) #1/gamma
    gaminv[i] ~ dgamma(0.01,0.01)
    gam[i] <- 1/gaminv[i]
  }
  
# Priors
    phi  ~ dnorm(60, 0.001) # Variance = 1000 small values for it to be noninformative
    tauinv   ~ dgamma(0.01, 0.01)
    tau <- 1/tauinv # the ones we put in coagulation.par

}",
  file="coagulation2.jag" )

Data2 = c(62, 60, 63, 59, NA, NA, NA, NA,    #diet1 n=4
         63,67,71,64,65,66, NA, NA,         #diet2 n=4  
         68,66,71,67,68,68,NA, NA,          #diet3 n=6
         52,62,60,61,63,64,63,59)           #diet4 n=8

Data2 = matrix(Data, nrow=4, byrow=T) #Convert to a matrix form
n2=c(4,6,6,8) #Sample sizes for each treatment

coagulation.data2 = list(y=Data2, n=n2, k=4)
coagulation.j2 = jags.model(file="coagulation2.jag", data=coagulation.data2, n.chains=3,
                           n.adapt=2000)
coagulation.par2 = c("tau", "gam[1]", "gam[2]", "gam[3]", "gam[4]")

coagulation.res2 = coda.samples(coagulation.j2, var = coagulation.par2, n.iter = 10000, thin = 10 )
summary(coagulation.res2)

plot(coagulation.res2)


##################3
# PROBLEM 2
# DATA<-read.table("economic_dat.txt",header=TRUE,sep="\t")
# General linear model

cat(" 
model
{
  for(i in 1:I)
  {
    for(j in 1:J) #n[i]
    {
      y[i,j] ~ dnorm(mu[i,j], tauinv)
      mu[i,j] <- beta0+(beta1*x[j]) #(or replace by) beta0[i]+beta1*(x[j] - mean(x[]))
    }
  }
  # beta0~dnorm(0,1.0E-4)
  # beta1~dnorm(0,1.0E-4)
  
  beta0~dnorm(mu0,tauinv)
  beta1~dnorm(mu1, tauinv)
  
  mu0~dnorm(0,sqrt(100))
  mu1~dnorm(0,sqrt(100))
  
  tauinv~dgamma(0.01,0.01) # tauinv: precision, tau: varience
  tau<-1/sqrt(tauinv)
}",
  file = "economic.jag")

x = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,
      24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44)
y = structure(.Data = c(1.1933,1.2173,1.2544,1.2847,1.3198,1.3351,1.3601,1.4116,1.4517,1.4781,
                        1.5126,1.5306,1.5427,1.5639,1.5798,1.586,1.6107,1.6435,1.6713,1.7154,1.7558,1.7931,1.8195,1.8477,
                        1.8827,1.9398,1.976,2.0128,2.0441,2.0698,2.0979,2.1284,2.1632,2.2119,2.2651,2.325,2.3587,2.4056,
                        2.4522,2.5051,2.5607,2.5729,2.5866,2.6127,87.973,90.021,90.12,91.18,89.832,90.668,89.85,91.276,
                        89.964,91.566,91.405,91.691,89.341,90.31,89.324,89.436,88.815,89.92,90.48,92.367,91.637,94.256,
                        95.021,96.547,95.45,97.657,97.985,99.383,98.213,99.85,99.877,101.169,99.922,102.186,102.657,104.522,
                        103.445,106.05,105.963,107.644,106.425,108.741,108.897,110.26,9.392,13.637,14.392,13.301,
                        10.411,13.057,13.621,13.354,11.056,14.559,14.067,12.286,9.658,13.659,12.351,12.307,10.508,16.287,
                        16.697,15.523,13.54,19.19,18.827,17.215,13.848,20.319,20.403,19.179,16.79,24.4,24.215,22.042,18.182,
                        22.67,21.204,19.746,16.656,24.552,23.775,22.449,18.145,26.344,25.278,22.511,42.96,47.516,43.888,
                        40.298,40.016,39.073,40.484,38.703,41.919,45.398,46.635,39.97,41.554,48.384,46.041,46.411,47.558,
                        59.342,57.487,57.165,62.2,74.678,69.074,67.03,70.453,85.102,85.142,71.203,73.38,88.139,92.899,
                        81.838,75.122,90.515,89.907,77.163,84.508,99.279,94.556,87.039,87.696,102.934,102.095,84.497,
                        9.203,10.174,11.078,12.334,10.228,10.394,11.246,13.071,10.954,11.41,11.892,13.204,10.439,11.148,
                        11.556,13.37,11.114,12.043,13.165,15.534,13.002,14.602,15.684,19.088,15.436,16.117,16.782,20.491,
                        16.383,18.18,19.782,23.605,18.483,19.659,21.042,24.203,20.242,22.143,22.87,27.486,22.399,22.412,
                        22.223,25.648,30.363,25.812,26.026,38.081,21.909,26.615,27.039,40.17,23.641,30.477,30.065,43.805,
                        24.256,30.976,30.579,45.471,25.891,33.05,33.385,50.311,29.177,36.855,35.418,53.126,30.325,37.977,
                        37.028,54.369,31.594,37.617,36.297,52.457,30.718,41.09,40.706,60.491,34.723,42.871,42.072,64.101,
                        36.516,46.782,47.006,69.966), .Dim = c(6,44))

economic.data = list(y=y, x=x, I=6, J=44)
economic.init = list(list(beta0=0, beta1=0, tauinv=1), 
                    list(beta0=10, beta1=10, tauinv=5),
                    list(beta0=15, beta1=15, tauinv=6))
economic.j = jags.model(file="economic.jag", data=economic.data, inits=economic.init,
                        n.chains=3, n.adapt=2000)
economic.par=c("beta0", "beta1", "tau", "tauinv")

economic.res=coda.samples(economic.j, var=economic.par, n.iter=10000, thin=10)
summary(economic.res)
plot(economic.res)

###################################################################3
# PROBLEM 3
# using bernoulli for Y, and libking to beta1 and beta2 in a linear link 
# Yi ~ dbin(Theta_i)
# log(theta_i) <- beta1 + beta2*Ti


# a) Take Normal(0,100) prior for ??1 and ??2.

#y ~ Bin(n, theta), logit(theta)= log(theta/1-theta)= alpha+beta*x

cat( "
model{
 for (i in 1:N)
 {
   y[i] ~ dbern(p[i])
   probit(p[i]) <- beta1 + beta2*T[i]    #or use probit(p[i])
   # or use centering

 }
  # Priors 
  beta1~dnorm(0,1/100)
  beta2~dnorm(0,1/100)
  
  # LD50 <- (T_bar -beta1)/beta2 # posterior inference summary
 
}",
     file="ORing.jag" )

T = c(53,57,58,63,66,67,67,67,68,69,70,70,70,70,72,73,75,75,76,76, 78,79,81)
Y = c(1,1,1,1,0,0,0,0,0,0,0,0,1,1,0,0,0,1,0,0,0,0,0)

ORing.data= list(y=Y, T = T, N=23)
ORing.init= list( list(beta1=0, beta2= 0),list(beta1=5, beta2= 5),list(beta1=10, beta2= 10) )
# ORing.ini=list(beta1=1, beta2= 1)
ORing.j <- jags.model( file = "ORing.jag",data=ORing.data, n.chains = 3, inits=ORing.init, n.adapt = 2000)

ORing.par <- c("beta1", "beta2")

ORing.res <- coda.samples(ORing.j, var =ORing.par, n.iter = 10000, thin = 10 )
summary(ORing.res)

plot(ORing.res)

bl = as.matrix(ORing.res)
plot(bl)



# b) Repeat the modeling after you center the time T. After centering, LD50 is 
# de???ned as T ??? ??1/??2. Give the posterior inference summary for LD50.

cat( "
model{
 for (i in 1:N)
 {
   y[i] ~ dbern(p[i])
   logit(p[i]) <- beta1 + beta2*(T[i] - mean(T[]))    #or use probit(p[i])
   # or use centering

 }
  # Priors 
  beta1~dnorm(0,1/100)
  beta2~dnorm(0,1/100)
  
  LD50 <- mean(T[]) -(beta1/beta2) # posterior inference summary
  for(i in 1:N)
  {
    logit(p.new[i]) <- beta1 + (28 + (2*i) - mean(T[]))*beta2
 
  }
 
}",
     file="ORing2.jag" )

ORing.init2 = list(beta1=1, beta2= 1)
ORing.j2 <- jags.model( file = "ORing2.jag",data=ORing.data, n.chains = 3, inits=ORing.init2, n.adapt = 2000)

ORing.par2 <- c("LD50","beta1", "beta2", "p.new")

ORing.res2 <- coda.samples(ORing.j2, var =ORing.par2, n.iter = 10000, thin = 10 )
summary(ORing.res2)

plot(ORing.res2)

