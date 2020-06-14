# Chapter 8
library(rethinking)
library(dplyr)
library(magrittr)
library(ggplot2)

# Problems are mislabeled as 7xx instead of 8xx in my copy

# 8E1
# 1- temperature
# 2- country
# 3- working engine

# 8E2
# 1, 2, 4

# 8E3

# 8M1
# interaction with all 3
# The effect of shade and water depend on temperature.

# 8M2
# multiply bloomsize by temp_cold

# 8M3

# 8H1
data(tulips)
d <- tulips
str(d)
precis(d)
d$blooms_std <- d$blooms / max(d$blooms)
d$water_cent <- d$water - mean(d$water)
d$shade_cent <- d$shade - mean(d$shade)
d$bed_ind <- as.numeric(d$bed)
str(d)

m8.7 <- quap(
  alist(
    blooms_std ~ dnorm(mu, sigma),
    mu <- a + bw*water_cent + bs*shade_cent + bws*water_cent*shade_cent,
    a ~ dnorm(.5, .25),
    bw ~ dnorm(0, .25),
    bs ~ dnorm(0, .25),
    bws ~ dnorm(0, .25),
    sigma ~ dexp(1)
  ), data=d
)
plot(precis(m8.7))

m8h1 <- quap(
  alist(
    blooms_std ~ dnorm(mu, sigma),
    mu <- a + bw*water_cent + bs*shade_cent + bws*water_cent*shade_cent,
    # a ~ dnorm(.5, .25),
    a[bed_ind] ~ dnorm(.5, .25),
    bw ~ dnorm(0, .25),
    bs ~ dnorm(0, .25),
    bws ~ dnorm(0, .25),
    sigma ~ dexp(1)
  ), data=d
)
precis(m8h1, depth=2)

# 8H2
compare(m8.7, m8h1)

# They are about the same.
# All of the a[bed_ind] are about the same.

# 8H3
data(rugged)
d <- rugged
d$log_gdp <- log(d$rgdppc_2000)
dd <- d[complete.cases(d$rgdppc_2000), ]
dd$log_gdp_std <- dd$log_gdp / mean(dd$log_gdp)
dd$rugged_std <- with(dd, rugged/max(rugged))

m8h3_with <- quap(alist(
  log_gdp_std ~ dnorm(mu, sigma),
  mu <- a + ba*cont_africa + br*rugged_std + bar*cont_africa*rugged_std,
  a ~ dnorm(1,.3),
  ba ~ dnorm(0, .3),
  br ~ dnorm(0,.3),
  bar ~ dnorm(0,.3),
  sigma ~ dexp(1)
), data=dd)
precis(m8h3_with)

m8h3_wo <- quap(alist(
  log_gdp_std ~ dnorm(mu, sigma),
  mu <- a + ba*cont_africa + br*rugged_std + bar*cont_africa*rugged_std,
  a ~ dnorm(1,.3),
  ba ~ dnorm(0, .3),
  br ~ dnorm(0,.3),
  bar ~ dnorm(0,.3),
  sigma ~ dexp(1)
), data=dd[dd$country!="Seychelles", ] )
precis(m8h3_wo)


precis(m8h3_with)
precis(m8h3_wo)

# The interaction of continent and ruggedness changed the most,
# but it is still in the same ballpark.
# The 89% interval doesn't include 0.

# b)
rugged.seq <- seq(-.1,1.1, l=30)
mu.NotAfrica.with <- link(m8h3_with, data=data.frame(cid=2, rugged_std=rugged.seq, cont_africa=0))
mu.Africa.with <- link(m8h3_with, data=data.frame(cid=2, rugged_std=rugged.seq, cont_africa=1))
mu.NotAfrica.with_mu <- apply(mu.NotAfrica.with, 2, mean)
mu.NotAfrica.with_ci <- apply(mu.NotAfrica.with, 2, PI, prob=.97)
mu.Africa.with_mu <- apply(mu.Africa.with, 2, mean)
mu.Africa.with_ci <- apply(mu.Africa.with, 2, PI, prob=.97)
par(mfrow=c(1,2))
plot(log_gdp_std ~ rugged_std, data=dd, col=1+cont_africa, main="With Seychelles")
lines(rugged.seq, mu.NotAfrica.with_mu)
shade(mu.NotAfrica.with_ci, rugged.seq)
lines(rugged.seq, mu.Africa.with_mu, col=2)
shade(mu.Africa.with_ci, rugged.seq)
# plot for without Seychelles
mu.NotAfrica.wo <- link(m8h3_wo, data=data.frame(cid=2, rugged_std=rugged.seq, cont_africa=0))
mu.Africa.wo <- link(m8h3_wo, data=data.frame(cid=2, rugged_std=rugged.seq, cont_africa=1))
mu.NotAfrica.wo_mu <- apply(mu.NotAfrica.wo, 2, mean)
mu.NotAfrica.wo_ci <- apply(mu.NotAfrica.wo, 2, PI, prob=.97)
mu.Africa.wo_mu <- apply(mu.Africa.wo, 2, mean)
mu.Africa.wo_ci <- apply(mu.Africa.wo, 2, PI, prob=.97)
plot(log_gdp_std ~ rugged_std, data=dd[dd$country!="Seychelles",], col=1+cont_africa, main="Without Seychelles")
lines(rugged.seq, mu.NotAfrica.wo_mu)
shade(mu.NotAfrica.wo_ci, rugged.seq)
lines(rugged.seq, mu.Africa.wo_mu, col=2)
shade(mu.Africa.wo_ci, rugged.seq)

# There's not a clear interaction when looking at just the data.
# The posterior lines still have an interaction, but not as much.

# c

m8h3c_mod1 <- quap(alist(
  log_gdp_std ~ dnorm(mu, sigma),
  mu <- a + br*rugged_std,
  a ~ dnorm(1,.3),
  ba ~ dnorm(0, .3),
  br ~ dnorm(0,.3),
  bar ~ dnorm(0,.3),
  sigma ~ dexp(1)
), data=dd[dd$country!="Seychelles", ] )

m8h3c_mod2 <- quap(alist(
  log_gdp_std ~ dnorm(mu, sigma),
  mu <- a + ba*cont_africa + br*rugged_std,
  a ~ dnorm(1,.3),
  ba ~ dnorm(0, .3),
  br ~ dnorm(0,.3),
  bar ~ dnorm(0,.3),
  sigma ~ dexp(1)
), data=dd[dd$country!="Seychelles", ] )

m8h3c_mod3 <- quap(alist(
  log_gdp_std ~ dnorm(mu, sigma),
  mu <- a + ba*cont_africa + br*rugged_std + bar*cont_africa*rugged_std,
  a ~ dnorm(1,.3),
  ba ~ dnorm(0, .3),
  br ~ dnorm(0,.3),
  bar ~ dnorm(0,.3),
  sigma ~ dexp(1)
), data=dd[dd$country!="Seychelles", ] )

compare(m8h3c_mod1, m8h3c_mod2, m8h3c_mod3)

# Adding the interaction reduced the WAIC

# 8H4
data(nettle)
d <- nettle
str(nettle)
d$lang.per.cap <- d$num.lang / d$k.pop
d$log_lang.per.cap <- log(d$lang.per.cap)
d$log_area <- log(d$area)
precis(d)

m8h4 <- quap(
  alist(
    log_lang.per.cap ~ dnorm(mu, sigma),
    mu ~ a + barea * log_area + bmgs*mean.growing.season + bsgs*sd.growing.season,
    a ~ dnorm(-5, 2),
    barea ~ dnorm(0,1),
    bmgs ~ dnorm(0,1),
    bsgs ~ dnorm(0,1),
    sigma ~ dexp(1)
  ), data=d
)
precis(m8h4)

# mean growing season has a positive effect on log_lang.per.cap
# sd growing season has a negative effect, but is near zero

# c
m8h4c <- quap(
  alist(
    log_lang.per.cap ~ dnorm(mu, sigma),
    mu ~ a + barea * log_area + bmgs*mean.growing.season + bsgs*sd.growing.season + bint*mean.growing.season*sd.growing.season,
    a ~ dnorm(-5, 2),
    barea ~ dnorm(0,1),
    bmgs ~ dnorm(0,1),
    bsgs ~ dnorm(0,1),
    bint ~ dnorm(0,1),
    sigma ~ dexp(1)
  ), data=d
)
precis(m8h4c)
# There is a slight interaction.

# I should have standardized the data. It's hard to set priors this way.