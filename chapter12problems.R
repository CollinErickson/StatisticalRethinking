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
