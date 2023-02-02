# TAKE HOME ASSIGNMENT

library(rjags)
getwd()

# PROBLEM 1

# PROBLEM 1
source('BinomialBeta.r')
# a) Using noninformative prior Unif(0,1)
lkhd_data <- c(rep(1, 12), rep(0,8))
posterior_dist_a <- bb.sum(lkhd_data, 1, 1, 1)
bb.trip(lkhd_data, 1, 1, 1)

# b) Using prior Beta(10,2)
lkhd_data <- c(rep(1, 12), rep(0,8))
posterior_dist_b <- bb.sum(lkhd_data, 1, 10, 2)
bb.trip(lkhd_data, 1, 10, 2)

# c) Describe brie???y your ???ndings regarding two di???erent priors above. Draw a 
# short conclusion on this analysis for ??.

# From a) we can see that by using a flat prior, Unif(0,1) which is equal to 
# Beta(1,1), then the posterior is basically the same as the likelihood so the 
# prior distribution has no effect in the posterior distribution. In b) we can 
# see that with a Beta(10,2) that now the posterior density is now between the 
# likelihood and prior, this time the prior has a strong effect on the posterior 
# distribution. The posterior mean is between the prior mean and the likelihood 
# mean.


# PROBLEM 2

# a) Using Normal prior
library(rjags)
cat( " model
 {
 # likelihood
 for (i in 1:N){
  x[i]~ dnorm(mu, psi) # psi is for precision
  }
 # Prior
  mu~dnorm(0,1/(0.003*psi))
  psi ~ dgamma(0.35, 1.01)
  tau <- 1/psi
 }",
     file="drug.jag" )

drug.data =list( x=c(-9, -4, -21, -3, -20, -31, -17, -26, -26, -10, -23, -33,
                     -19, -19, -23), N=15)

drug.inits= list(mu=2, psi=1)

drug.m <- jags.model( file = "drug.jag", data = drug.data, n.chains = 3, 
                        inits = drug.inits, n.adapt = 2000 )

drug.par <- c("mu", "tau", "psi")

res <- coda.samples( drug.m, var = drug.par, n.iter = 10000, thin = 10)
class( res )   # [1] "mcmc.list"
summary(res)
plot(res) 

# a)
drug.par.psi <- c("psi")
res.psi <- coda.samples( drug.m, var = drug.par.psi, n.iter = 10000, thin = 10)
summary(res.psi)
plot(res.psi)

# b)
drug.par.mu <- c("mu")
res.mu <- coda.samples(drug.m, var = drug.par.mu, n.iter = 10000, thin = 10)
summary(res.mu)
plot(res.mu) 

coef(res.mu)

#c)
summary(res.mu)

drug.par.tau <- c("tau")
res.tau <- coda.samples( drug.m, var = drug.par.tau, n.iter = 10000, thin = 10)
summary(res.tau)
plot(res.tau) 


#d) COMMENTS


#e) (Extra Credit) Derive the parameter values for the above marginal 
#    posterior distributions by hand computation using the results in lab 6, 
#    problem 2. Compare posterior summaries with the results you get above.






# Problem 2 ANOTHER WAY

# a) Using Normal prior
# library(rjags)
# cat( " model
#  {
#  # likelihood
#  for (i in 1:N){
#   x[i]~ dnorm(mu1, psi1) # psi is for precision
#  }
#   
#  for (j in 1:M){
#   y[j]~ dnorm(mu2, psi2) # psi is for precision
#  }
#   
#  # Prior
#   mu1~dnorm(0,1/(0.003*psi1))
#   mu2~dnorm(0,1/(0.003*psi2))
#   
#   psi1 ~ dgamma(0.35, 1.01)
#   psi2 ~ dgamma(0.35, 1.01)
#   
#   tau1 <- 1/psi1
#   tau2 <- 1/psi2
#   
#   delta <- mu1-mu2
#  }", file="drug.jag" )
# 
# drug.data =list( x=c(210, 169, 187, 160, 167, 176, 185, 206, 173, 146, 174, 201, 198, 148, 154),
#                  y=c(201, 165, 166, 157, 147, 145, 168, 180, 147, 136, 151, 168, 179, 129, 131),
#                  N=15, M=15)
# 
# drug.inits= list(mu1=150, mu2= 150, psi1=1, psi2=1)
# 
# drug.m <- jags.model( file = "drug.jag", data = drug.data, n.chains = 1, 
#                       inits = drug.inits, n.adapt = 2000 )
# 
# drug.par <- c("mu1", "mu2", "tau1", "tau2", "delta")
# 
# res <- coda.samples( drug.m, var = drug.par, n.iter = 10000, thin = 10)
# class( res )   # [1] "mcmc.list"
# summary(res)
# plot(res) 






# PROBLEM 3
cat( " model
 {
 # likelihood
  for (i in 1:n)
  {
    mu[i] <- beta0 + beta1*weight[i] + beta2*sixcyl[i] + beta3*eightcyl[i]
    mpg[i] ~ dnorm(mu[i], psi) # psi is precision
  }
 # Prior
  beta0  ~ dnorm(0, 10000)
  beta1 ~ dnorm(0, 10000)
  beta2 ~ dnorm(0, 10000)
  beta3 ~ dnorm(0, 10000)
  
  psi    ~ dgamma(0.01, 0.01)
  sigma2  <- 1/sqrt(psi)
  
 }", file="gasoline.jag" )

mpg_data <- c(21.0, 21.0, 22.8, 21.4,18.7,18.1,14.3, 24.4, 22.8,19.2,17.8,16.4,17.3,
         15.2,10.4,10.4,14.7,32.4, 30.4, 33.9, 21.5, 15.5,15.2,13.3,19.2, 27.3,
         26.0, 30.4,15.8,19.7,15.0, 21.4)
weight_data <- c(2.620, 2.875, 2.320,3.215,3.440, 3.460, 3.570, 3.190, 3.150, 3.440,
             3.440, 4.070, 3.730, 3.780, 5.250, 5.424, 5.345, 2.200, 1.615, 
             1.835, 2.465, 3.520, 3.435, 3.840, 3.845, 1.935, 2.140, 1.513, 
             3.170, 2.770, 3.570, 2.780)
sixcyl_data <- c(1, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 1, 0, 0)
eightcyl_data <- c(0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1,
              1, 1, 1, 0, 0, 0, 1, 0, 1, 0)

gasoline.data <- list(weight = weight_data, sixcyl = sixcyl_data, 
                      eightcyl = eightcyl_data, mpg = mpg_data, n = 32)

gasoline.inits <- list(  list(beta0=0.1, beta1=0.1, beta2=0.1, beta3=0.1), 
                   list(beta0=0.5, beta1=0.5, beta2=0.5, beta3=0.5 ), 
                   list(beta0=1.0, beta1=1.0, beta2=1.0, beta3=1.0 )  )

gasoline.m <- jags.model( file = "gasoline.jag", data = gasoline.data, n.chains = 3, 
                       inits = gasoline.inits, n.adapt = 2000 )
gasoline.par <- c("beta1", "beta2", "beta3", "psi")

res <- coda.samples( gasoline.m, var = gasoline.par, n.iter = 10000, thin = 10)
class( res )  

summary(res)

#a)
gasoline.par_b1 <- c("beta1")
res_b1 <- coda.samples( gasoline.m, var = gasoline.par_b1, n.iter = 10000, thin = 10)
plot(res_b1) 

gasoline.par_b2 <- c("beta2")
res_b2 <- coda.samples( gasoline.m, var = gasoline.par_b2, n.iter = 10000, thin = 10)
plot(res_b2) 

gasoline.par_b3 <- c("beta3")
res_b3 <- coda.samples( gasoline.m, var = gasoline.par_b3, n.iter = 10000, thin = 10)
plot(res_b3) 

gasoline.par_psi <- c("psi")
res_psi <- coda.samples( gasoline.m, var = gasoline.par_psi, n.iter = 10000, thin = 10)
plot(res_psi) 

#b)
summary(res_b1)
#There is a 95% probability that beta1 lies in the interval (-0.0191391, 0.0200570)

summary(res_b2)
#There is a 95% probability that beta2 lies in the interval (-0.0194248, 0.0194190)

summary(res_b3)
#There is a 95% probability that beta2 lies in the interval (-0.0193488, 0.0188650)

summary(res_psi)
#There is a 95% probability that beta2 lies in the interval (-0.001306, 0.003541)


#c)

# From the model, b2 represents the effect in cars with six cylinders, and b3 
# represent the effect in cars with eight cylinders.
# to observe if the effect is different with a six cylinder or an eight cylinder
# we can take the marginal difference b2-b3 as a variable and calculate a 
# credible interval to check if zero is contained by this interval to see if the
# difference is zero, i.e.there is no significant effect between the cylinders.

cat( " model
 {
 # likelihood
  for (i in 1:n)
  {
    mu[i] <- beta0 + beta1*weight[i] + beta2*sixcyl[i] + beta3*eightcyl[i]
    mpg[i] ~ dnorm(mu[i], psi) # psi is precision
  }
 # Prior
  beta0  ~ dnorm(0, 10000)
  beta1 ~ dnorm(0, 10000)
  beta2 ~ dnorm(0, 10000)
  beta3 ~ dnorm(0, 10000)
  
  psi    ~ dgamma(0.01, 0.01)
  sigma2  <- 1/sqrt(psi)
  delta <- beta2 - beta3
  
 }", file="gasoline.jag" )

gasoline.m <- jags.model( file = "gasoline.jag", data = gasoline.data, n.chains = 3, 
                          inits = gasoline.inits, n.adapt = 2000 )
gasoline.par <- c("delta")

res <- coda.samples( gasoline.m, var = gasoline.par, n.iter = 10000, thin = 10)
class( res )  
summary(res)
plot(res)

# We get a 95% credible interval (-0.0273302, 0.0280335), we can observe that
# zero is contained by this interval, therefore we can conclude that there is
# that the difference between cars with six cylinders and cars with eight 
# cylinders is not statistical significant.


# d)
cat( " model
 {
 # likelihood
  for (i in 1:n)
  {
    mu[i] <- beta0 + beta1*weight[i] + beta2*sixcyl[i] + beta3*eightcyl[i]
    mpg[i] ~ dnorm(mu[i], psi) # psi is precision (hyon is sigma)
  }
 # Prior
  beta0  ~ dnorm(0, 10000)
  beta1 ~ dnorm(0, 10000)
  beta2 ~ dnorm(0, 10000)
  beta3 ~ dnorm(0, 10000)
  
  psi    ~ dgamma(0.01, 0.01)
  sigma2  <- 1/sqrt(psi)
  
  # Prediction
  #mu.new <- alpha + b.gen*gen.new+ b.age*age.new
  # bp.new ~dnorm(mu.new, psi)
  
  mu.new <- beta0 + beta1*weight.new + beta2*sixcyl.new + beta3*eightcyl.new
  mpg.new ~ dnorm(mu.new, psi)
  
 }", file="gasoline.jag" )

gasoline.data<-list(weight = weight_data, sixcyl = sixcyl_data, eightcyl = eightcyl_data, 
                      mpg = mpg_data, n = 32, weight.new=3.5, sixcyl.new=0, eightcyl.new=0)

gasoline.inits <- list(  list(beta0=0.1, beta1=0.1, beta2=0.1, beta3=0.1), 
                         list(beta0=0.5, beta1=0.5, beta2=0.5, beta3=0.1 ), 
                         list(beta0=1.0, beta1=1.0, beta2=1.0, beta3=0.1 )  )

gasoline.m <- jags.model( file = "gasoline.jag", data = gasoline.data, n.chains = 3, 
                          inits = gasoline.inits, n.adapt = 2000 )
gasoline.par <- c("mpg.new")

res <- coda.samples( gasoline.m, var = gasoline.par, n.iter = 10000, thin = 10)
class( res )  

summary(res)
plot(res)
