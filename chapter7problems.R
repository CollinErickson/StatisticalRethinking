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

# 6M3
# If the models are fit to different data, the difference in  model quality
# could be due to the data rather than the parameterization.

# 6M4
# What happens to the number of eff. param under DIC/WAIC as a prior becomes
# more concentrated? Why?
# p_eff = sum_i {var_theta log p(y_i | theta)}
# Stronger prior reduces variance of theta, reduces variance of
# log(p(y_i | theta)), reduces the number of effective parameters.

# 6M5
# Informative priors reduce overfitting because they require more data to get
# extreme parameters.

# 6M6
# Overly informative priors result in underfitting because it can require much
# more data to move parameters to the true value since they are anchored
# by the prior.

# Hard
data(Howell1)
d <- Howell1
d$age <- with(Howell1, (age-mean(age)) / sd(age))
set.seed(1000)
i <- sample(1:nrow(d), size=nrow(d)/2)
d1 <- d[i, ]
d2 <- d[-i, ]

m1 <- quap(alist(
  height ~ dnorm(mu, sigma),
  mu <- a + b1*age,
  a ~ dnorm(140, 10),
  b1 ~ dnorm(0, 20),
  sigma ~ dexp(1)
), data=d1)
precis(m1)

m2 <- quap(alist(
  height ~ dnorm(mu, sigma),
  mu <- a + b1*age + b2*age^2,
  a ~ dnorm(140, 20),
  b1 ~ dnorm(0, 20),
  b2 ~ dnorm(0, 20),
  sigma ~ dexp(1)
), data=d1)
precis(m2)

m3 <- quap(alist(
  height ~ dnorm(mu, sigma),
  mu <- a + b1*age + b2*age^2 + b3*age^3,
  a ~ dnorm(140, 20),
  b1 ~ dnorm(0, 20),
  b2 ~ dnorm(0, 20),
  b3 ~ dnorm(0, 20),
  sigma ~ dexp(1)
), data=d1)
precis(m3)

m4 <- quap(alist(
  height ~ dnorm(mu, sigma),
  mu <- a + b1*age + b2*age^2 + b3*age^3 + b4*age^4,
  a ~ dnorm(140, 20),
  b1 ~ dnorm(0, 20),
  b2 ~ dnorm(0, 20),
  b3 ~ dnorm(0, 20),
  b4 ~ dnorm(0, 20),
  sigma ~ dexp(1)
), data=d1)
precis(m4)

m5 <- quap(alist(
  height ~ dnorm(mu, sigma),
  mu <- a + b1*age + b2*age^2 + b3*age^3 + b4*age^4 + b5*age^5,
  a ~ dnorm(140, 20),
  b1 ~ dnorm(0, 20),
  b2 ~ dnorm(0, 20),
  b3 ~ dnorm(0, 20),
  b4 ~ dnorm(0, 20),
  b5 ~ dnorm(0, 20),
  sigma ~ dexp(1)
), data=d1)
precis(m5)

m6 <- quap(alist(
  height ~ dnorm(mu, sigma),
  mu <- a + b1*age + b2*age^2 + b3*age^3 + b4*age^4 + b5*age^5 + b6*age^6,
  a ~ dnorm(140, 20),
  b1 ~ dnorm(0, 20),
  b2 ~ dnorm(0, 20),
  b3 ~ dnorm(0, 20),
  b4 ~ dnorm(0, 20),
  b5 ~ dnorm(0, 20),
  b6 ~ dnorm(0, 20),
  sigma ~ dexp(1)
), data=d1)
precis(m6)

mods <- list(m1, m2, m3, m4, m5, m6)

# 6H1
# lapply(mods, WAIC)
compare(m1, m2, m3, m4, m5, m6)

# The most terms has the lowest WAIC.
# pWAIC is higher for 2nd order than 4th order

# 6H2
par(mfrow=c(2,3))
for (i in 1:6) {
  ageseq <- seq(-2,3,l=51)
  heightsim <- sim(mods[[i]], data=list(age=ageseq))
  heightmean <- colMeans(heightsim)
  heightHPDI <- apply(heightsim, 2, HPDI, prob=.97)
  with(d1, plot(height ~ age))
  shade(heightHPDI, ageseq)
  lines(ageseq, heightmean)
  with(d2, points(height ~ age, col=2))
}
# poly 1 and 2 are clearly underfitting.
# We don't see any overfitting, except outside the region of data,
# because the data is very dense.

# 6H3
m_ens <- ensemble(m1, m2, m3, m4, m5, m6, data=list(age=ageseq))
par(mfrow=c(1,1))
heightsim <- m_ens$sim
heightmean <- colMeans(heightsim)
heightHPDI <- apply(heightsim, 2, HPDI, prob=.97)
with(d1, plot(height ~ age))
shade(heightHPDI, ageseq)
lines(ageseq, heightmean)
with(d2, points(height ~ age, col=2))
# Looks like the higher order models.

# 6H4
for (i in 1:6) {
  samp <- extract.samples(mods[[i]], data=d2)
  meansamp <- colMeans(samp)
  # mu <- with(meansamp, a + sum(meansamp[2:(1+i)] * d2$age ^ (1:i)))
  mu <- rep(meansamp['a'], nrow(d2))# + sweep(d2$age ^ (1:i), 1, meansamp[2:(1+i)], "*")
  for (j in 1:i) {
    mu <- mu + d2$age^j * meansamp[1+j]
  }
  sigma <- meansamp['sigma']
  plot(mu, d2$height); abline(a=0,b=1)
  llh <- sum(dnorm(d2$height, mu, sigma, log=TRUE))
  # Deviance is -2*llh
  cat('deviance', i, -2 * llh, '\n')
}

# 6H5
# Compare these to
compare(m1, m2, m3, m4, m5, m6)
# Fairly close, except m4 and m5 are lower than m6 with the test data


# 6H6

m6h6 <- quap(alist(
  height ~ dnorm(mu, sigma),
  mu <- a + b1*age + b2*age^2 + b3*age^3 + b4*age^4 + b5*age^5 + b6*age^6,
  a ~ dnorm(140, 20),
  b1 ~ dnorm(0, .5),
  b2 ~ dnorm(0, .5),
  b3 ~ dnorm(0, .5),
  b4 ~ dnorm(0, .5),
  b5 ~ dnorm(0, .5),
  b6 ~ dnorm(0, .5),
  sigma ~ dexp(1)
), data=d1)
precis(m6h6)


ageseq <- seq(-2,3,l=51)
heightsim <- sim(m6h6, data=list(age=ageseq))
heightmean <- colMeans(heightsim)
heightHPDI <- apply(heightsim, 2, HPDI, prob=.97)
with(d1, plot(height ~ age))
shade(heightHPDI, ageseq)
lines(ageseq, heightmean)
with(d2, points(height ~ age, col=2))
# It tails off horribly for large age
# It is consistently underpredicting for low age,
# then overpredicting for medium age, then under again by the end.
compare(m1, m2, m3, m4, m5, m6, m6h6)
# It has the second highest WAIC, so it doesn't seem very good.

# Calculate deviance
samp <- extract.samples(m6h6, data=d2)
meansamp <- colMeans(samp)
mu <- rep(meansamp['a'], nrow(d2))# + sweep(d2$age ^ (1:i), 1, meansamp[2:(1+i)], "*")
for (j in 1:6) {
  mu <- mu + d2$age^j * meansamp[1+j]
}
sigma <- meansamp['sigma']
plot(mu, d2$height); abline(a=0,b=1)
llh <- sum(dnorm(d2$height, mu, sigma, log=TRUE))
# Deviance is -2*llh
cat('deviance', i, -2 * llh, '\n')
# WAIC is only lower than linear model, and is fairly
# close to the predicted WAIC
