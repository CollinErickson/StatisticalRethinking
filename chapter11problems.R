# Chapter 11
library(rethinking)
library(dplyr)
library(magrittr)
library(ggplot2)

# Problems are for Chapter 11 (version 2),
# but are listed as 10xx.

# 10E1
curve(logit)
logit(.35)
# -.619

# 10E2
curve(inv_logit, from=-10,to=10)
# 3.2 = log(p/(1-p))
# p is inv_logit(3.2)
inv_logit(3.2)
# .961

# 10E3
# log odds are multiplied by exp(1.7)
exp(1.7)

# 10E4
# An offset is used to convert count to rate.
# In the monk example, the manuscripts are counted over different time periods,
# by day and by week. To put them on a comparable scale, we need an set to
# correct for this time difference.


# 10M1
# The aggregated version has the n choose k multiplier.

# 10M2
# changing the predictor by one unit increases the log odds by exp(1.7)

# 10M3
# Binomial needs a probability, which must be within 0 to 1.
# Using the logit link ensures the values are in 0 to 1.

# 10M4
# For Poisson, lambda must be positive. The log link
# can map the linear model output  onto the positive numbers.

# 10M5
# The rate must be between 0 and 1.
# A process known to have mean below 1.

# 10M6
# Binomial: Each trial has two outputs, with fixed mean.
# Poisson: Fixed mean, independent events in a fixed time.

# 10H1

## R code 11.1
library(rethinking)
data(chimpanzees)
d <- chimpanzees

## R code 11.2
d$treatment <- 1 + d$prosoc_left + 2*d$condition

# trimmed data list
dat_list <- list(
  pulled_left = d$pulled_left,
  actor = d$actor,
  treatment = as.integer(d$treatment) )
## R code 11.11
m11.4 <- ulam(
  alist(
    pulled_left ~ dbinom( 1 , p ) ,
    logit(p) <- a[actor] + b[treatment] ,
    a[actor] ~ dnorm( 0 , 1.5 ),
    b[treatment] ~ dnorm( 0 , 0.5 )
  ) , data=dat_list , chains=4 , log_lik=TRUE )
precis( m11.4 , depth=2 )

m11.4_quap <- quap(
  alist(
    pulled_left ~ dbinom( 1 , p ) ,
    logit(p) <- a[actor] + b[treatment] ,
    a[actor] ~ dnorm( 0 , 1.5 ),
    b[treatment] ~ dnorm( 0 , 0.5 )
  ) , data=dat_list  )
precis( m11.4_quap , depth=2 )

plot(coeftab(m11.4, m11.4_quap))
# They look about the same.
# a[2] has a longer right tail.
# quap means appear to be further from center.

# 10H2

# 10H3
# a
library(MASS)
data(eagles)
d <- eagles
str(d)
d$P_large <- ifelse(d$P=='L', 1, 0)
d$A_adult <- ifelse(d$A=='A', 1, 0)
d$V_large <- ifelse(d$V=='L', 1, 0)
m10h3quap <- quap(alist(
  y ~ dbinom(n, p),
  logit(p) ~ (a + bp*P_large + bv*V_large + ba*A_adult),
  a ~ dnorm(0,10),
  bp ~ dnorm(0,5),
  bv ~ dnorm(0,5),
  ba ~ dnorm(0,5)
), data=d)
precis(m10h3quap)
pairs(m10h3quap)

m10h3stan <- map2stan(m10h3quap)
precis(m10h3stan)
plot(coeftab(m10h3quap, m10h3stan))
pairs(m10h3stan)
# It seems okay, but the estimates are a bit different.
# The stan model has less normal parameter distributions.

# b
postcheck(m10h3stan)

# c
m10h3cstan <- map2stan(alist(
  y ~ dbinom(n, p),
  logit(p) ~ (a + bp*P_large + bv*V_large + ba*A_adult + bpa*P_large*A_adult),
  a ~ dnorm(0,10),
  bp ~ dnorm(0,5),
  bv ~ dnorm(0,5),
  ba ~ dnorm(0,5),
  bpa ~ dnorm(0,10)
), data=d)
precis(m10h3cstan)
pairs(m10h3cstan)
compare(m10h3stan, m10h3cstan)

