# Chapter 15
library(rethinking)
library(dplyr)
library(magrittr)
library(ggplot2)

# 15E1
# T ~ Poisson(mu)
# log(mu) <- a + b * logpop_mean
# logpop_obs ~ dnorm(logpop_mean, logpop_se)
# a ~ dnorm(0,1.5)
# b ~ dnorm(0,1)

# 15E2
# T ~ Poisson(mu)
# log(mu) <- a + b * logpop
# logpop ~ dnorm(nu, sigma_logpop)
# a ~ dnorm(0,1.5)
# b ~ dnorm(0,1)
# nu ~ dexp(1)
# sigma_logpop ~ dexp(1)

# 15M1
# Brain size model on p 506.
# Missing brain sizes are assumed to follow a normal distribution with unknown
# mean and variance.
# The missing values will be estimated using the model based on the corresponding
# data for M and K.

# 15M2
# Copying from book, but changing so neocortex.prop isn't standardized
data(milk)
d <- milk
d$neocortex.prop <- d$neocortex.perc / 100
d$logmass <- log(d$mass)
dat_list <- list(
  K = standardize( d$kcal.per.g ),
  B = ( d$neocortex.prop ),
  M = standardize( d$logmass ) )
m15.5 <- ulam(
  alist(
    K ~ dnorm( mu , sigma ),
    mu <- a + bB*B + bM*M,
    B ~ dnorm( nu , sigma_B ),
    c(a,nu) ~ dnorm( 0 , 0.5 ),
    c(bB,bM) ~ dnorm( 0, 0.5 ),
    sigma_B ~ dexp( 1 ),
    sigma ~ dexp( 1 )
  ) , data=dat_list , chains=4 , cores=1)
precis(m15.5, depth=2)
# The B_impute intervals aren't too close to 0 or 1,
# not sure using beta will change much.

m15.5_beta <- ulam(
  alist(
    K ~ dnorm( mu , sigma ),
    mu <- a + bB*B + bM*M,
    B ~ dbeta( 1, 1 ),
    # c(a,nu) ~ dnorm( 0 , 0.5 ),
    a ~ dnorm( 0 , 0.5 ),
    c(bB,bM) ~ dnorm( 0, 0.5 ),
    # sigma_B ~ dexp( 1 ),
    sigma ~ dexp( 1 )
  ) , data=dat_list , chains=4 , cores=1, start = list(B=.5))
precis(m15.5_beta, depth=2)