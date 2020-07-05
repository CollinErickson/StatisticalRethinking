# Chapter 13
library(rethinking)
library(dplyr)
library(magrittr)
library(ggplot2)

# 13E1
# a
# lower stdev is more shrinkage since they will be pulled more
# towards the mean of the prior.

# 13E2
# Making up data to see if I can get code to work
d <- tibble(group=sample(1:4, 200, T), x=rnorm(200), p=inv_logit(group^2+.6*x), y=rbinom(1, 1, p))
str(d)
m1 <- ulam(alist(
  y ~ dbinom(1, p),
  logit(p) <- a[group] + b*x,
  a[group] ~ dnorm(a_bar, sigma_a),
  a_bar ~ dnorm(0,5),
  b ~ dnorm(0,0.5),
  sigma_a ~ dexp(1)
), data=d)
precis(m1, depth=2)
rethinking::plotchains(m1)

# 13E3
# basically same as above
m1 <- ulam(alist(
  y ~ dnorm(mu, sigma),
  mu <- a[group] + b*x,
  a[group] ~ dnorm(a_bar, sigma_a),
  a_bar ~ dnorm(0,5),
  b ~ dnorm(0,1),
  sigma_a ~ dexp(1),
  sigma ~ dexp(1)
))#, data=d)

# 13E4
# y ~ dpois(lambda)
# log(lambda) ~ a[group] + b*x
# a[group] ~ dnorm(a_bar, sigma_a)
# b ~ dnorm(0,1)
# a_bar ~ dnorm(0,1)
# sigma_a ~ dexp(1)

# 13E5
# y ~ dpois(lambda)
# log(lambda) ~ a[group1] + a[group2] + b*x
# a[group1] ~ dnorm(a_bar1, sigma_a1)
# a[group2] ~ dnorm(a_bar2, sigma_a2)
# b ~ dnorm(0,1)
# a_bar1 ~ dnorm(0,1)
# a_bar2 ~ dnorm(0,1)
# sigma_a1 ~ dexp(1)
# sigma_a2 ~ dexp(1)

# 13M1
data(reedfrogs)
d <- reedfrogs
d$tank <- 1:nrow(d)
precis(d)
head(d)
dat1 <- list(S=d$surv,N=d$density, tank=d$tank)
m1 <- ulam(alist(
  S ~ dbinom(N, p),
  logit(p) <- a[tank],
  a[tank] ~ dnorm(0,1.5)
), data=dat1, chains=4, log_lik=T)
precis(m1, depth=2)
precis(m1, depth=2) %>% plot


mbase <- ulam(alist(
  S ~ dbinom(N, p),
  logit(p) <- a[tank],
  a[tank] ~ dnorm(a_bar, sigma),
  a_bar ~ dnorm(0,1),
  sigma ~ dexp(1)
), data=dat1, chains=4, log_lik=T)
precis(mbase)

dat2 <- dat1
dat2$is_big <- d$size=="big"
msize <- ulam(alist(
  S ~ dbinom(N, p),
  logit(p) <- a[tank] + bs*is_big,
  a[tank] ~ dnorm(a_bar, sigma),
  a_bar ~ dnorm(0,1),
  bs ~ dnorm(0,1),
  sigma ~ dexp(1)
), data=dat2, chains=4, log_lik=T)
precis(msize)

dat3 <- dat1
dat3$is_pred <- d$pred=="pred"
mpred <- ulam(alist(
  S ~ dbinom(N, p),
  logit(p) <- a[tank] + bp*is_pred,
  a[tank] ~ dnorm(a_bar, sigma),
  a_bar ~ dnorm(0,1),
  bp ~ dnorm(0,1),
  sigma ~ dexp(1)
), data=dat3, chains=4, log_lik=T)
precis(mpred)

dat4 <- dat2
dat4$is_pred <- d$pred=="pred"
msizepred <- ulam(alist(
  S ~ dbinom(N, p),
  logit(p) <- a[tank] + bs*is_big +bp*is_pred,
  a[tank] ~ dnorm(a_bar, sigma),
  a_bar ~ dnorm(0,1),
  bs ~ dnorm(0,1),
  bp ~ dnorm(0,1),
  sigma ~ dexp(1)
), data=dat4, chains=4, log_lik=T)
precis(msizepred)

msizepredint <- ulam(alist(
  S ~ dbinom(N, p),
  logit(p) <- a[tank] + bs*is_big +bp*is_pred + bsp*is_big*is_pred,
  a[tank] ~ dnorm(a_bar, sigma),
  a_bar ~ dnorm(0,1),
  bs ~ dnorm(0,1),
  bp ~ dnorm(0,1),
  bsp ~ dnorm(0,1),
  sigma ~ dexp(1)
), data=dat4, chains=4, log_lik=T)
precis(msizepredint)

# 13M2
compare(mbase, msize, mpred, msizepred, msizepredint)
# None are big improvements over just base. Best is just pred.


# 13M3
mbase <- ulam(alist(
  S ~ dbinom(N, p),
  logit(p) <- a[tank],
  a[tank] ~ dnorm(a_bar, sigma),
  a_bar ~ dnorm(0,1),
  sigma ~ dexp(1)
), data=dat1, chains=4, log_lik=T)
precis(mbase)

mcau <- ulam(alist(
  S ~ dbinom(N, p),
  logit(p) <- a[tank],
  a[tank] ~ dcauchy(a_bar, sigma),
  a_bar ~ dnorm(0,1),
  sigma ~ dexp(1)
), data=dat1, chains=4, log_lik=T)
precis(mcau)

compare(mbase, mcau)

# plot(coeftab(mbase, mcau))

sampnorm <- extract.samples(mbase)
sampcau <- extract.samples(mcau)

anorm <- apply(sampnorm$a, 2, mean)
acau  <- apply(sampcau$a, 2, mean)

plot(anorm, acau, xlab='normal', ylab='cauchy')


# 13M4
mstu <- ulam(alist(
  S ~ dbinom(N, p),
  logit(p) <- a[tank],
  a[tank] ~ dstudent(2, a_bar, sigma),
  a_bar ~ dnorm(0,1),
  sigma ~ dexp(1)
), data=dat1, chains=4, log_lik=T)
precis(mstu)

compare(mbase, mcau, mstu)


# 13M5

# 13M6

# 13H1
data("bangladesh")
d <- bangladesh
str(d)
sort(unique(d$district))
d$district_id <- as.integer(as.factor(d$district))
sort(unique(d$district_id))
precis(d)
d$use_contraception <- d$use.contraception

# fixed model
mfix <- ulam(alist(
  use_contraception ~ dbinom(1, p),
  logit(p) <- a_district[district_id],
  a_district[district_id] ~ dnorm(0,5),
  sigma ~ dexp(1)
), data=d %>% select(use_contraception, district_id))
precis(mfix, depth=2)

mml <- ulam(alist(
  use_contraception ~ dbinom(1, p),
  logit(p) <- a_district[district_id],
  a_district[district_id] ~ dnorm(a,5),
  a ~ dnorm(0,5),
  sigma ~ dexp(1)
), data=d %>% select(use_contraception, district_id))
precis(mml, depth=2)
compare(mfix, mml)


# 13H2

# 13H3

# 13H4




