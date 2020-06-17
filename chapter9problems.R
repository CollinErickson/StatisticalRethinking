# Chapter 9
library(rethinking)
library(dplyr)
library(magrittr)
library(ggplot2)


# 9E1
# not 1, can be cts
# not 2, can be anything
# yes 3, must by sym. But metropolis-hastings doesn't require symmetric.

# 9E2
# Gibbs uses conjugate pairs.
# Limitations: you have to use conjugate priors,
# can't be very inefficient, samples can be highly correlated.'

# 9E3
# HMC can't handle discrete parameters.
# It works by moving along surfaces using a physics simulation,
# which doesn't work with discrete parameters since it can't take
# small steps.

# 9E4
# Samples can be correlated, so n_eff is often less than the
# actual number of samples.

# 9E5
# 1 from above. Even 1.01 can be too high.

# 9E6
# stationary, good mixing, convergence
# should cover entire region, not spend time in different areas.

# 9M1
m9.1 <- ulam(
  alist(
    log_gdp_std ~ dnorm( mu , sigma ) ,
    mu <- a[cid] + b[cid]*( rugged_std - 0.215 ) ,
    a[cid] ~ dnorm( 1 , 0.1 ) ,
    b[cid] ~ dnorm( 0 , 0.3 ) ,
    sigma ~ dexp( 1 )
  ) , data=dat_slim , chains=1 )
m9m1 <- ulam(
  alist(
    log_gdp_std ~ dnorm( mu , sigma ) ,
    mu <- a[cid] + b[cid]*( rugged_std - 0.215 ) ,
    a[cid] ~ dunif( 0 , 10 ) ,
    b[cid] ~ dnorm( 0 , 0.3 ) ,
    sigma ~ dexp( 1 )
  ) , data=dat_slim , chains=1 )
precis( m9.1 , depth=2 )
precis( m9m1 , depth=2 )
# no change in the estimates for a, but we got higher n_eff

# 9M2

# 9M3

# 9H1
mp <- ulam(
  alist(
    a ~ dnorm(0, 1),
    b ~ dcauchy(0,1)
  ),
  data=list(y=1),
  start=list(a=0,b=0),
  iter=1e4, warmup=100 #, WAIC=FALSE
)
precis(mp)
traceplot(mp)
# It isn't doing anything, just sampling from the prior,
# since there is no likelihood for y.
# a is just samples from a normal.
# b is samples from a cauchy. It has much lower n_eff since
# it is a much less informative prior.
# You can never really get good samples from a cauchy since
# the tail is thick.

# 9H2
data(WaffleDivorce)
d <- WaffleDivorce
d$A <- standardize( d$MedianAgeMarriage )
d$D <- standardize( d$Divorce )
d$M <- standardize( d$Marriage )
d <- d[, c('D', 'A', 'M')]

m5.1 <- ulam(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- a + bA * A ,
    a ~ dnorm( 0 , 0.2 ) ,
    bA ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data = d, log_lik = T )

m5.2 <- ulam(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- a + bM * M ,
    a ~ dnorm( 0 , 0.2 ) ,
    bM ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data = d , log_lik = T)

m5.3 <- ulam(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- a + bM*M + bA*A ,
    a ~ dnorm( 0 , 0.2 ) ,
    bM ~ dnorm( 0 , 0.5 ) ,
    bA ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data = d , log_lik=T)
compare(m5.1, m5.2, m5.3)
# model 1 is best, but 3 is close

# 9H3
## R code 6.2
N <- 100                          # number of individuals
set.seed(909)
height <- rnorm(N,10,2)           # sim total height of each
leg_prop <- runif(N,0.4,0.5)      # leg as proportion of height
leg_left <- leg_prop*height +     # sim left leg as proportion + error
  rnorm( N , 0 , 0.02 )
leg_right <- leg_prop*height +    # sim right leg as proportion + error
  rnorm( N , 0 , 0.02 )
# combine into data frame
d <- data.frame(height,leg_left,leg_right)

## R code 9.29
m5.8s <- ulam(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + bl*leg_left + br*leg_right ,
    a ~ dnorm( 10 , 100 ) ,
    bl ~ dnorm( 2 , 10 ) ,
    br ~ dnorm( 2 , 10 ) ,
    sigma ~ dexp( 1 )
  ) , data=d, chains=4,
  start=list(a=10,bl=0,br=0.1,sigma=1) , log_lik = T)

## R code 9.30
m5.8s2 <- ulam(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + bl*leg_left + br*leg_right ,
    a ~ dnorm( 10 , 100 ) ,
    bl ~ dnorm( 2 , 10 ) ,
    br ~ dnorm( 2 , 10 ) ,
    sigma ~ dexp( 1 )
  ) , data=d, chains=4,
  constraints=list(br="lower=0"),
  start=list(a=10,bl=0,br=0.1,sigma=1) , log_lik=T)
precis(m5.8s)
precis(m5.8s2)
# bl is much smaller now
# There are fewer n_eff
pairs(m5.8s)
pairs(m5.8s2)
# You can see the correlation in bl and br in the posterior
# This makes sense because of the physical model.

# 9H4
compare(m5.8s, m5.8s2)
# The updated model has lower WAIC, but it's close.
# The updated model has fewer effective parameters because
# there is less freedom/variability in bl and br

# 9H5
n <- 1e5
pos <- numeric(n)
pos[1] <- sample(1:10, 1)
populations <- 1 +runif(10)
for (i in 1:(n-1)) {
  seashells <- pos[i] + ifelse(runif(1)<.5, -1,1)
  seashells <- ifelse(seashells==0, 10, ifelse(seashells==11, 1, seashells))
  pos[i+1] <- ifelse(runif(1) < populations[seashells]/populations[pos[i]], seashells, pos[i])
}
table(pos) / n
(populations) / sum(populations)
# hist(pos, freq=T)
plot(table(pos))
points(1:10,
     n*(populations) / sum(populations)
)

# 9H6
