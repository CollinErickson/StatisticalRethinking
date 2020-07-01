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
# nu ~ dexp(1

)


