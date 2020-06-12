# Chapter 7
library(rethinking)
library(dplyr)
library(magrittr)
library(ggplot2)

# 6E1
# State the 3 motivating criteria that define information entropy
# Want to measure the uncertainty of a set of events.
# Three properties: continuous, increasing, additive
# only satisfied by -sum(p*log(p))

# 6E2
# Entropy of 70/30 coin
-sum(c(.7,.3)*log(c(.7,.3)))
curve(-(x*log(x) + (1-x)*log(1-x)))

# 6E3
# 4 sided die, 1 20%, 2 25%, 3 25%, 4 30%. Entropy?
-sum(c(.2,.25,.25,.3) * log(c(.2,.25,.25,.3)))

# 6E4
# 4 sided die, 1/3 1-3, never 4. Entropy?
# Won't work with 0
-sum(c(1/3,1/3,1/3,0) * log(c(1/3,1/3,1/3,0)))
# Can just exclude 0 since 0*log(0) = 0
-sum(c(1/3,1/3,1/3) * log(c(1/3,1/3,1/3)))

# 6M1
# Write down and compare the definitions of AIC, DIC, WAIC.
# Which is most general? Which assumptions are required to transform a more general
# criterion into a less general one?
# AIC = D_train + 2p = -2*lppd + 2*p
# Only reliable when (1) priors are flat or overwhelmed by likelihood,
# (2) posterior dist is approx multinormal,
# (3) N >> k, number of parameters
# DIC = replace p with .5*mean(var(deviance))
# Needs (2) and (3), but can accomodate informative priors.
# WAIC = -2(lppd - sum(var(log(p(y|theta))))), sum is over data points y, variance over theta
# No assumption about shape of posterior.
# WAIC is most general.

# 6M2
# Explain the difference between model selection and model averaging.
# What information is lost under model selection?
# What information is lost under model averaging?
# Model selection is picking a single model out of options.
# Model averaging averages a group of models.
# Model selection loses the uncertainty of parameter estimates.
# Model averaging can include suboptimal models.



