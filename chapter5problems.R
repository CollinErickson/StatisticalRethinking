# Chapter 5
library(rethinking)
library(dplyr)
library(magrittr)
library(ggplot2)

# 5E1
# 2 and 4

# 5E2
# animaldiv ~ N(mu, sigma)
# mu ~ a + b1 * latitude + b2 * plantdiv

# 5E3
# time ~ N(mu, sigma)
# mu ~ a + b1 * funding + b2 * labsize
# b1, b2 > 0

# 5E4
# 1, 3, 4, 5

# 5H1
data(foxes)
precis(foxes)
str(foxes)

mh1a <- quap(
  alist(
    weight ~ dnorm(mu, sigma),
    mu ~ a + b*area,
    a ~ dnorm(4.5, 2),
    b ~ dnorm(0, 5),
    sigma ~ dunif(0,5)
  ), data=foxes
)
precis(mh1a)

mh1b <- quap(
  alist(
    weight ~ dnorm(mu, sigma),
    mu ~ a + b*groupsize,
    a ~ dnorm(4.5, 2),
    b ~ dnorm(0, 5),
    sigma ~ dunif(0,5)
  ), data=foxes
)
precis(mh1b)

areaseq <- seq(1,7,l=51)
mu_area <- link(mh1a, data=data.frame(area=areaseq))
PI_area <- apply(mu_area, 2, PI, .95)

groupseq <- seq(2,8,l=51)
mu_group <- link(mh1b, data=data.frame(groupsize=groupseq))
PI_group <- apply(mu_group, 2, PI, .95)

par(mfrow=c(2,1))
plot(weight ~ area, data=foxes)
abline(mh1a)
shade(PI_area, areaseq)

plot(weight ~ groupsize, data=foxes)
abline(mh1b)
shade(PI_group, groupseq)

# Area doesn't seem important, groupsize might be.

# 5H2
mh2 <- quap(
  alist(
    weight ~ dnorm(mu, sigma),
    mu ~ a + ba*area + bg*groupsize,
    a ~ dnorm(4.5, 2),
    ba ~ dnorm(0, 5),
    bg ~ dnorm(0, 5),
    sigma ~ dunif(0,5)
  ), data=foxes
)
precis(mh2)


areaseq <- seq(1,7,l=51)
mu_area <- link(mh2, data=data.frame(area=areaseq, groupsize=mean(foxes$groupsize)))
PI_area <- apply(mu_area, 2, PI, .95)
mean_area <- apply(mu_area, 2, mean)

groupseq <- seq(2,8,l=51)
mu_group <- link(mh2, data=data.frame(area=mean(foxes$area), groupsize=groupseq))
PI_group <- apply(mu_group, 2, PI, .95)
mean_group <- apply(mu_group, 2, mean)

par(mfrow=c(2,1))
plot(weight ~ area, data=foxes)
lines(areaseq, mean_area)
shade(PI_area, areaseq)

plot(weight ~ groupsize, data=foxes)
lines(groupseq, mean_group)
shade(PI_group, groupseq)

# Now both area important.
cor(foxes)
# area and groupsize were positively correlated