# INTRODUCTION TO BAYESIAN 2 - LAB 3

################################################################################
# PROBLEM 1

# /*****************  METROPOLIS-HASTINGS ALGORITHM  *****************/

#1. We want to estimate the mean of a gamma G(4.3, 6.2) random variable using
#a) Metropolis-Hastings with a gamma G(4, 7) candidate;

mh_algorithm <- function(theta_0,S)
{
  
  theta_s = rep(NA,S)      # initialize Storage
  theta_s[1]= theta_0              # store starting position
  accept_count=0             # For the number of acceptance
  
  for (s in 1:S)
  {   
    
    # target dist -> Gamma(4.3, 6.2)  -> p(??|x)
    # proposal dist -> Gamma(4, 7)    -> q(??|??*)
    theta_star = rgamma(1,4,7)      #current value, proposed, CANDIDATE: ??*
    
    alpha <- (dgamma(theta_star,4.3,6.2)*dgamma(theta_s[s],4,7)) / (dgamma(theta_s[s],4.3,6.2)*dgamma(theta_star,4,7))
    # ??(??(s), ??*) = min(1, (p(??*|x) * q(??(s)|??*)) / (p(??(s)|x) * q(??*|??(s))))
    
    if (runif(1) <= alpha)
    {
      theta_s[s+1]= theta_star       # Accept 
      accept_count = accept_count+1         # Update accepted count
    } #end of if
    else 
      theta_s[s+1]= theta_s[s]     # Reject
    
  } # end of for
  list(sample=theta_s,r=accept_count/S) #r: acceptance rate
}     

samples = mh_algorithm(2,5000)

# autocorrelation of samples
acf(samples$sample, main="Autocorrelation plot for theta samples")

# plot of samples
plot(samples$sample, type="l", xlab="iterations", ylab="theta values")

# acceptance rate
acceptance_rate = samples$r   # 0.7992

# histogram
hist(samples$sample, freq=FALSE, xlab="", ylab="", col="wheat2",
     main="M_H with Gam(4,7) proposal")
x= seq(0,1,length=100)
curve(dgamma(x,4.3,6.2), lwd=2, col="sienna", add=T)


# b) Metropolis-Hastings with an independent Normal(0.7, 0.11) proposal. Then, 
# increase the variability of the Normal proposal for better acceptance rates 
# of total samples.

mh_algorithm <- function(theta_0, S, sigma){
  
  theta_s = rep(NA,S)      # initialize Storage
  theta_s[1]= theta_0              # store starting position
  accept_count=0             # For the number of acceptance
  
  for (s in 1:S){   
    
    # target dist -> Gamma(4.3, 6.2)        -> p(??|x)
    # proposal dist -> Normal(0.7, 0.11)    -> q(??|??*)
    theta_star = rnorm(1,0.7,sqrt(sigma))      #current value, proposed
    
    alpha <- (dgamma(theta_star,4.3,6.2)*dnorm(theta_s[s],0.7,sqrt(sigma))) / 
      (dgamma(theta_s[s],4.3,6.2)*dnorm(theta_star,0.7,sqrt(sigma)))
    
    if (runif(1) <= alpha){
      theta_s[s+1]= theta_star       # Accept 
      accept_count = accept_count+1         # Update accepted count
    } #end of if
    else 
      theta_s[s+1]= theta_s[s]     # Reject
    
  } # end of for
  list(sample=theta_s,r=accept_count/S) #r: acceptance rate
}     

samples = mh_algorithm(0,5000, sqrt(0.11))
samples = mh_algorithm(0,5000, 0.5)
samples = mh_algorithm(0,5000, 0.8)
samples = mh_algorithm(0,5000, 1)

# autocorrelation of samples
acf(samples$sample, main="Autocorrelation plot for theta samples")

# plot of samples
plot(samples$sample, type="l", xlab="iterations", ylab="theta values")

# acceptance rate
acceptance_rate = samples$r  # 0.6048, 0.509, 0.4018, 0.3838
acceptance_rate







################################################################################
# PROBLEM 2

# a) First, write a function for the target density function. Plot the density 
# of the mixed distribution for a sequence of p ??? (0, 1) with length = 100, .

mix_beta <- function(x){
  0.5*dbeta(x,2,20) + 0.5*dbeta(x,20,2)
  }

seq = seq(0,1,length=100)
plot(seq,mix_beta(seq), type="l")

# b) Write an R function for a Metropolis-Hastings algorithm with the a random 
# walk proposal: N(p(s), 0.12), i.e. Normal distribution centered at the current 
# value. Make a histogram and the traceplot of 500 samples drawn using your 
# function. (Try several runs of 500 samples. What do you observe here?

# Random walk proposal: N(p(s), 0.12) centered a t the current value  
# use the random walk Metropolis samples in problem 
# rnodm wak is symetric proposisal, then we get alpha ration only for the target,
#then the , and they'r not indepentend,


N = 500

rw_sample = rep(0.5,N)

for (i in 2:N) {
  theta_star = rnorm(1,rw_sample[i-1],0.1)      
  
  alpha = min(1, mix_beta(theta_star)  /mix_beta(rw_sample[i-1]) )
  
  if (runif(1) < alpha) 
    rw_sample[i] = theta_star 
  else
    rw_sample[i] = rw_sample[i-1]   
}  # end of for

plot(rw_sample, type="l")
hist(rw_sample, 30,freq=FALSE)

# Doing several runs you get widely different shapes 


# c) Write a function for a Metropolis-Hastings algorithm using the 
# uniform(0,1) independent proposal. Make a histogram and the traceplot of 500 
# samples drawn using your function. Give brief comments comparing with your 
# results in b).

N = 500
rw_indep = rep(0.5,N)

for (i in 1:N) {
  theta_star = runif(1)      
  
  alpha = min(1, mix_beta(theta_star)  /mix_beta(rw_indep[i-1]) )
  
  if (runif(1) < alpha) 
    rw_indep[i] = theta_star 
  else
    rw_indep[i] = rw_indep[i-1]   
}  # end of for

plot(rw_indep, type="l")
hist(rw_indep, 30,freq=FALSE)
# the shape is more consistent to a bimodal distrbution as te mix beta has
# this one is working a lot better, to keep the bimodality
print(1-sum(rw_indep[-1]==rw_indep[-N])/(N-1))

###############################################################################

# /*************************  GBBS SAMPLER  **************************/
# PROBLEM 4
# b) Write an R function for a Gibbs sampler to draw samples from p(µ, ??|y). 
# Test your function by generating 10000 samples and make a histogram of them.

gibbs <- function(niter, mu=0, tau=1) {
  y_vec = rnorm(15,mean=3, sd=5)
  cat(mean(y_vec))
  n=length(y_vec)
  
  mu_samples = NULL
  tau_samples = NULL
  
  for(i in 1:niter){
    mu = rnorm(1, mean(y_vec), sd=sqrt(tau/n)) #full conditional for mu
    tau = (sum(y_vec - mu)**2) / rchisq(1,n)    #full conditional for tau
    mu_samples = append(mu_samples, mu)
    tau_samples = append(tau_samples,tau)
  }
  
  return(list(mu_vec = mu_samples, tau_vec=tau_samples))
}

# Get samples from joint posterior
gibbs_samples = gibbs(1000)
mu_gibbs = gibbs_samples$mu_vec
tau_gibbs = gibbs_samples$tau_vec

# Plot of marginal samples
par(mfrow=c(2,1))
hist(mu_gibbs, main="Gibbs sampling distribution for mu", nclass=50)
hist(tau_gibbs, main="Gibbs sampling distribution for tau", nclass=50)
