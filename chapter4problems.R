# Chapter 4
library(rethinking)
library(dplyr)
library(magrittr)
library(ggplot2)


#4E1
# First line

# 4E2
# 2, mu and sigma

# 4E3
# Pr(u, sigma | y) = prod(dnorm(y|mu,sigma)*dnorm(mu|0,10)*dexp(sigma|1)) /
# integral over mu and sigma of above

#4E4
# Second line

# 4E5
# 3: alpha, beta, sigma

# 4M1
n <- 1e4
sigma <- rexp(n, 1)
mu <- rnorm(n, 0, 10)
y <- rnorm(n, mu, sigma)
dens(y)

# 4M2
flist <- alist(
  y ~ dnorm(mu, sigma),
  mu ~ dnorm(0, 10),
  sigma ~ dexp(1)
)
# quap(flist)

# 4M3
# y_i ~ normal(mu_i, sigma)
# mu_i = alpha + beta*x_i
# alpha ~ norm(0, 10)
# beta ~ unif(0,1)
# sigma ~ exp(1)

# 4M4

# y_i ~ normal(mu_i, sigma)
# mu_i = alpha + beta*x_i
# alpha ~ norm(140, 20)
# beta ~ unif(0,20)
# sigma ~ unif(0,50)

# 4M5
# alpha must be > 0.

# 4M6
# Variance shouldn't have units of cm?
# want sigma in (0,8)

# 4H1
data(Howell1)
d4h1 <- Howell1
m4h1 <- quap(
  alist(
    height ~ dnorm(a + b*weight, sigma),
    a ~ dnorm(178, 100),
    b ~ dnorm(0,10),
    sigma~dunif(0,50)
  ), data=d4h1
)
precis(m4h1)
weights <- c(46.95,43.72,64.78,32.59,54.63)
sim4h1 <- sim(m4h1,data=list(weight=weights), n=1e4)
str(sim4h1)
# expected height
apply(sim4h1, 2, mean)
# 89% interval
apply(sim4h1, 2, PI, prob=.89)

# 4H2
d4h2 <- Howell1 %>% filter(age<18)
str(d4h2)
precis(d4h2)
m4h2 <- quap(
  alist(
    height ~ dnorm(a + b*weight, sigma),
    a ~ dnorm(178, 100),
    b ~ dnorm(0,10),
    sigma~dunif(0,50)
  ), data=d4h2
)
m4h2 %>% precis
# 27 cm
ggplot(d4h2, aes(weight, height)) + geom_point()
wts <- seq(0,50,l=101)
post <- extract.samples(m4h2)
mus <- sapply(wts, function(wt) {post$a+post$b*wt})
mu.mean <- colMeans(mus)  
mu.hpdi <- apply(mus, 2, HPDI, prob=.89)  
sim.hts <- sim(m4h2, data=list(weight=wts))  
hts.pi <- apply(sim.hts, 2, PI, prob=.89)
plot(height ~ weight, data=d4h2)#, col=col.alpha(rangi2, .95))
shade(hts.pi, wts)
shade(mu.hpdi, wts, col=2)
lines(wts, mu.mean, col=7)

# c
# It's clearly not linear, use a spline or polynomial.

# 4H3
d4h3 <- Howell1
m4h3 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*log(weight),
    a ~ dnorm(178,20),
    b ~ dlnorm(0,1),
    sigma ~ dunif(0,50)
  ), data=d4h3
)
precis(m4h3)
# If weight increases by a factor of 2.7,
# height increases by 46.8

# b
wts <- seq(0,65,l=101)
post <- extract.samples(m4h3)
mus <- sapply(wts, function(wt) {post$a+post$b*log(wt)})
mu.mean <- colMeans(mus)  
mu.hpdi <- apply(mus, 2, HPDI, prob=.97)  
sim.hts <- sim(m4h3, data=list(weight=wts))  
hts.pi <- apply(sim.hts, 2, PI, prob=.97)
plot(height ~ weight, data=d4h3)#, col=col.alpha(rangi2, .95))
shade(hts.pi, wts)
shade(mu.hpdi, wts, col=2)
lines(wts, mu.mean, col=7)

# 4H4
d4h4 <- Howell1 %>% mutate(weight_s=(weight-mean(weight))/sd(weight),
                           weight_s2=weight_s^2)
d4h4 %>% precis
m4h4 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a+b1*weight_s + b2*weight_s2,
    a ~ dnorm(178,20),
    b1 ~ dlnorm(0,1),
    b2 ~ dnorm(0,1),
    sigma ~ dunif(0,50)
  ), data=d4h4
)
plot(height ~ weight_s, data=d4h4)
for (i in 1:10) {
  sigma <- runif(1, 0,50)
  a <-  rnorm(1, 140,10)
  b1 <-  rlnorm(1, 3,1)
  b2 <-  -rlnorm(1, 0,2)
  curve(a+b1*x+b2*x^2, add=T)
}
