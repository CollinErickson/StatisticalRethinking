# Chapter 12
library(rethinking)
library(dplyr)
library(magrittr)
library(ggplot2)


# 12E1
# Ordered has a strict order...
# The distance between each category is unknown,
# but there is information gained from each category's
# relation to other categories.
# Unordered: Country
# Ordered: On a scale of 1 to 10, how do you feel about ...?

# 12E2
# Cumulative logit function.
# It's a logit on the cumulative probability.
# Since the categories are ordered, they are spaced out between 0 and 1
# on the cumulative scale."
# To find the effect for each category, you add the "probabilities"
# of all lower categories.

# 12E3
# It's a model misspecification.
# For Poisson, it'll underestimate lambda.

# 12E4
# Overdispersion: For Poisson, more zeros than otherwise expected. 
# Maybe something like number of apples on each tree, there could be more 0
# because of trees that die early.
# Underdispersion: For Poisson, fewer zeroes than expected.
# Sticking with tree example, maybe if the zeros were excluded from the data.

# 12M1
x <- c(12,36,7,41)
pcum <- cumsum(x)/ sum(x)
logit(pcum)
log(pcum/(1-pcum))

# 12M2
plot(1:4, pcum, xlab="rating", ylab='cumulative proportion')
lines(1:4, pcum)
for (i in 1:4) {
  lines(c(i,i), c(0,pcum[i]))
  lines(c(i,i), c(if (i==1) 0 else pcum[i-1],pcum[i]), col=2, lwd=5)
}

# 12M3
# Let a be probability  of extra 0 (not 0 from binomial),
# so 0 from coin flip to decide whether 0 or from binomial.
# Pr(0|n, p, a) = a + (1-a)*(1-p)^n
# For y=1,2,3,...,n
# Pr(y|n, p, a) = (1-a) * (n C p) p^y * (1-p)^(n-y)

# 12H1
data("Hurricanes")
d <- Hurricanes
str(d)
precis(d)
with(d, plot(female, femininity))

m1 <- quap(alist(
  deaths ~ dpois(lambda),
  log(lambda) ~ a,
  a ~ dnorm(0, 10)
), data=d)
precis(m1)

m2 <- quap(alist(
  deaths ~ dpois(lambda),
  log(lambda) ~ a + bf*femininity,
  a ~ dnorm(0, 10),
  bf ~ dnorm(0,10)
), data=d)
precis(m2)
plot(precis(m2))
# effect is .07, so exp(.07)
exp(.07)
postcheck(m2)

compare(m1, m2)

femseq <- seq(1,11, l=51)
l1 <- link(m1, data=data.frame(femininity=femseq))
l1.mean <- colMeans(l1)
l1.PI <- apply(l1, 2, PI, p=.89)
l2 <- link(m2, data=data.frame(femininity=femseq))
l2.mean <- colMeans(l2)
l2.PI <- apply(l2, 2, PI, p=.89)
with(d, plot(femininity, deaths, pch=19, col=3))
points(femseq, l1.mean)
points(femseq, l2.mean, col=2)
shade(l1.PI, femseq)
shade(l2.PI, femseq)
# Not a very good fit


# 12H2
# gamma poisson
mgp <- quap(alist(
  deaths ~ dgampois(lambda, theta),
  log(lambda) ~ a + bf*femininity,
  a ~ dnorm(0, 10),
  bf ~ dnorm(0,10),
  theta ~ dexp(1)
), data=d)
precis(mgp)
# bf now overlaps 0.

lgp <- link(mgp, data=data.frame(femininity=femseq))
lgp.mean <- colMeans(lgp)
lgp.PI <- apply(lgp, 2, PI, p=.89)

with(d, plot(femininity, deaths, pch=19, col=3))
points(femseq, l1.mean)
points(femseq, l2.mean, col=2)
shade(l1.PI, femseq)
shade(l2.PI, femseq)
points(femseq, lgp.mean, col=3)
shade(lgp.PI, femseq)
# It has larger uncertainty.

# 12H3
d$damage_norm_std <- standardize(d$damage_norm)
d$min_pressure_std <- standardize(d$min_pressure)
d$femininity_std <- standardize(d$femininity)
mgp2 <- quap(alist(
  deaths ~ dgampois(lambda, theta),
  log(lambda) ~ a + bf*femininity_std + bd*damage_norm_std + bp*min_pressure_std,
  a ~ dnorm(0, 10),
  c(bf, bd, bp) ~ dnorm(0,2),
  theta ~ dexp(1)
), data=d)
precis(mgp2)

# Add interaction
mgp3 <- quap(alist(
  deaths ~ dgampois(lambda, theta),
  log(lambda) ~ a + bf*femininity_std + bd*damage_norm_std + bp*min_pressure_std + bfd*femininity_std*damage_norm_std,
  a ~ dnorm(0, 10),
  c(bf, bd, bp, bfd) ~ dnorm(0,2),
  theta ~ dexp(1)
), data=d)
precis(mgp3)

compare(mgp, mgp2, mgp3)
plot(compare(mgp, mgp2, mgp3))
# adding interaction did almost nothing

# 12H4
d$log_damage_norm <- log(d$damage_norm)
mgp4 <- quap(alist(
  deaths ~ dgampois(lambda, theta),
  log(lambda) ~ a + bf*femininity_std + bd*log_damage_norm + bp*min_pressure_std,
  a ~ dnorm(0, 10),
  c(bf, bd, bp) ~ dnorm(0,2),
  theta ~ dexp(1)
), data=d)
precis(mgp4)

compare(mgp, mgp2, mgp3, mgp4)
# Helped a little bit


# 12H4
data(Trolley)
d <- Trolley
d %>% str

# from chapter
m1 <- quap(alist(
  response ~ dordlogit(phi, c(a1,a2,a3,a4,a5,a6)),
  phi <- bA*action + bI*intention + bC*contact + bAI*action*intention + bCI*contact*intention,
  c(bA, bI,bC, bAI, bCI) ~ dnorm(0,10),
  c(a1, a2, a3, a4,a5,a6) ~ dnorm(0,10)
), data=d, start=list(a1=-1.9,a2=-1.2,a3=-.7,a4=.2,a5=.9,a6=1.8))
precis(m1)
plot(precis(m1))
pairs(m1)

# Add gender and gender*contact
m2 <- quap(alist(
  response ~ dordlogit(phi, c(a1,a2,a3,a4,a5,a6)),
  phi <- bA*action + bI*intention + bC*contact + bAI*action*intention + bCI*contact*intention +
    bF*(1-male) + bFC*(1-male)*contact,
  c(bA, bI,bC, bAI, bCI, bF, bFC) ~ dnorm(0,10),
  c(a1, a2, a3, a4,a5,a6) ~ dnorm(0,10)
), data=d, start=list(a1=-1.9,a2=-1.2,a3=-.7,a4=.2,a5=.9,a6=1.8))
precis(m2)
plot(precis(m2))
pairs(m2)
compare(m1, m2)
plot(compare(m1, m2))
# gender has nonzero effect, but the interaction is near zero

# 12H6
data(Fish)
d <- Fish
d %>% str
d %>% precis

# Only use hours
f1 <- quap(alist(
  fish_caught ~ dzipois(p, lambda),
  logit(p) <- ap,
  log(lambda) <- log(hours) + al,
  ap ~ dnorm(0,10),
  al ~ dnorm(0,10)
), data=d)
precis(f1)
plot(precis(f1))
postcheck(f1)
logistic(-.74)
# 32% didn't fish
exp(-.14)
# avg fish caught per hour when fishing is .87

# Add in other factors
f2 <- quap(alist(
  fish_caught ~ dzipois(p, lambda),
  logit(p) <- ap + ac*camper + app*persons + ach*child,
  log(lambda) <- log(hours) + al + bl*livebait + bc*camper + bpp*persons + bch*child,
  ap ~ dnorm(0,10),
  al ~ dnorm(0,10),
  c(ac, app, ach) ~ dnorm(0,2),
  c(bl, bc, bpp, bch) ~ dnorm(0,2)
), data=d)
precis(f2)
plot(precis(f2))
compare(f1, f2)

# New model is much better




# 12H7

# 12H8


