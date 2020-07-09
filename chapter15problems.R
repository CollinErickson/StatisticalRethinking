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
# Can't get proper initialization for dbeta.
# Why would it try anything outside 0-1?

# 15M3
data(WaffleDivorce)
d <- WaffleDivorce
# points
plot( d$Divorce ~ d$MedianAgeMarriage , ylim=c(4,15) ,
      xlab="Median age marriage" , ylab="Divorce rate" )
# standard errors
for ( i in 1:nrow(d) ) {
  ci <- d$Divorce[i] + c(-1,1)*d$Divorce.SE[i]
  x <- d$MedianAgeMarriage[i]
  lines( c(x,x) , ci )
}
dlist <- list(
  D_obs = standardize( d$Divorce ),
  D_sd = d$Divorce.SE / sd( d$Divorce ),
  M = standardize( d$Marriage ),
  A = standardize( d$MedianAgeMarriage ),
  N = nrow(d)
)
m15.1 <- ulam(
  alist(
    D_obs ~ dnorm( D_true , D_sd ),
    vector[N]:D_true ~ dnorm( mu , sigma ),
    mu <- a + bA*A + bM*M,
    a ~ dnorm(0,0.2),
    bA ~ dnorm(0,0.5),
    bM ~ dnorm(0,0.5),
    sigma ~ dexp(1)
  ) , data=dlist , chains=4 , cores=4 )
precis( m15.1 , depth=2 )


# D_sd is now 2x
dlistb <- list(
  D_obs = standardize( d$Divorce ),
  D_sd = 2*d$Divorce.SE / sd( d$Divorce ),
  M = standardize( d$Marriage ),
  A = standardize( d$MedianAgeMarriage ),
  N = nrow(d)
)
m15.1b <- ulam(
  alist(
    D_obs ~ dnorm( D_true , D_sd ),
    vector[N]:D_true ~ dnorm( mu , sigma ),
    mu <- a + bA*A + bM*M,
    a ~ dnorm(0,0.2),
    bA ~ dnorm(0,0.5),
    bM ~ dnorm(0,0.5),
    sigma ~ dexp(1)
  ) , data=dlistb, chains=4 , cores=4 )
precis( m15.1b , depth=2 )

plot(m15.1@coef, m15.1b@coef); abline(a=0,b=1,col=2)
# Parameter mean estimates are about the same.
plot(m15.1@vcov, m15.1b@vcov); abline(a=0,b=1,col=2)
# Variance estimates are generally smaller.


# 15M4
# Making up X -> Y -> Z
n <- 1e3
X <- rnorm(n)
Y <- 4 + 3*X + rnorm(n)
Z <- -5 + 2*Y + rnorm(n)
d <- data.frame(X, Y, Z)
pairs(d)
m1 <- quap(alist(
  Y ~ dnorm(a + b1*X + b2*Z, sigma),
  c(a,b1,b2) ~ dnorm(0,5),
  sigma ~ dexp(1)
), data=d)
precis(m1)

# It looks fine when just using X
m2 <- quap(alist(
  Y ~ dnorm(a + b1*X, sigma),
  c(a,b1) ~ dnorm(0,5),
  sigma ~ dexp(1)
), data=d)
precis(m2)

compare(m1, m2)

# 15M5

# 15M6

# 15H1
data(elephants)
d <- elephants
str(d)
plot(d)

m1 <- ulam(alist(
  MATINGS ~ dpois(lambda),
  log(lambda) <- a + b*AGE,
  a ~ dnorm(0,10),
  b ~ dnorm(0,1)
), data=d)
precis(m1)
# plot(m1)
traceplot(m1)

dl <- as.list(d)
dl[['N']] <- length(d$AGE)
m2 <- ulam(alist(
  MATINGS ~ dpois(lambda),
  log(lambda) <- a + b*AGE_est[i],
  AGE ~ dnorm(AGE_est, 5),
  vector[N]:AGE_est ~ dnorm(35, 10),
  a ~ dnorm(0,10),
  b ~ dnorm(0,1)
), data=dl, start=list(AGE_est=d$AGE))
precis(m2)

# 15H2
# Same as above, but with bigger std error
m3 <- ulam(alist(
  MATINGS ~ dpois(lambda),
  log(lambda) <- a + b*AGE_est[i],
  AGE ~ dnorm(AGE_est, 10),
  vector[N]:AGE_est ~ dnorm(35, 10),
  a ~ dnorm(0,10),
  b ~ dnorm(0,1)
), data=dl, start=list(AGE_est=d$AGE))
precis(m3)

m4 <- ulam(alist(
  MATINGS ~ dpois(lambda),
  log(lambda) <- a + b*AGE_est[i],
  AGE ~ dnorm(AGE_est, 20),
  vector[N]:AGE_est ~ dnorm(35, 10),
  a ~ dnorm(0,10),
  b ~ dnorm(0,1)
), data=dl, start=list(AGE_est=d$AGE))
precis(m4)

# 15H3
set.seed(100)
x <- c(rnorm(10), NA)
y <- c(rnorm(10,x), 100)
d <- list(x=x, y=y)
plot(x,y)
lm(y ~ x)

m0 <- ulam(alist(
  y ~ dnorm(mu, sigma),
  mu <- a + b*x,
  x ~ dnorm(0,1),
  a ~ dnorm(0,100),
  b ~ dnorm(0,100),
  sigma ~ dexp(1)
), data=list(x=x[1:10], y=y[1:10]), chains=3, cores=3)
precis(m0, depth=2)

m1 <- ulam(alist(
  y ~ dnorm(mu, sigma),
  mu <- a + b*x,
  x ~ dnorm(0,1),
  a ~ dnorm(0,100),
  b ~ dnorm(0,100),
  sigma ~ dexp(1)
), data=d, chains=3, cores=3)
precis(m1, depth=2)
traceplot(m1)
# b and x_impute have two modes corresponding to whether the imputed x 
# is bigger or smaller than other x. Then the slope is extremely
# negative or positive.

s1 <- extract.samples(m1)
str(s1)
par(mfrow=c(2,2))
for (i in 1:4) {
  dens(s1[[i]])
}
