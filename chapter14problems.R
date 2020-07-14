# Chapter 14
library(rethinking)
library(dplyr)
library(magrittr)
library(ggplot2)
library(MASS)

# 14E1

# Independent intercepts and slopes
# y ~ dnorm(mu, sigma)
# mu <- a[group] + b[group] * x
# a[group] ~ dnorm(a_bar, sigma_a)
# a_bar ~ dnorm(0,10)
# b[group] ~ dnorm(b_bar, sigma_b)
# b_bar ~ dnorm(0,10)
# sigma ~ dexp(1)
# sigma_a ~ dexp(1)
# sigma_b ~ dexp(1)

# Covarying intercepts and slopes
# y ~ dnorm(mu, sigma)
# mu <- a[group] + b[group] * x
# c(a[group], b[group]) ~ multi_normal(c(a_bar, b_bar), Rho, sigma_group)
# a_bar ~ dnorm(0,10)
# b_bar ~ dnorm(0,10)
# sigma ~ dexp(1)
# sigma_group ~ dexp(1)
# Rho ~ lkj_corr(2)


# 14E2

# 14E3
# When there is high correlation between slopes and intercepts.
# Or if the prior is restrictive.

# 14M1
# Cafe data
# As shown in book, rho=-.7
a <- 3.5
b <- -1
sigma_a <- 1
sigma_b <- .5
rho <- -.7
Mu <- c(a,b)
cov_ab <- sigma_a*sigma_b*rho
Sigma <- matrix(c(sigma_a^2, cov_ab, cov_ab, sigma_b^2), ncol=2)
N_cafes <- 20
library(MASS)
set.seed(5)
vary_effects <- mvrnorm(N_cafes, Mu, Sigma)
a_cafe <- vary_effects[,1]
b_cafe <- vary_effects[,2]
set.seed(22)
N_visits <- 10
afternoon <- rep(0:1, N_visits*N_cafes/2)
cafe_id <- rep(1:N_cafes, each=N_visits)
mu <- a_cafe[cafe_id] + b_cafe[cafe_id]*afternoon
sigma <- .5
wait <- rnorm(N_visits*N_cafes, mu, sigma)
d <- data.frame(cafe=cafe_id, afternoon=afternoon, wait=wait)

m1 <- ulam(
  alist(
    wait ~ normal(mu, sigma),
    mu <- a_cafe[cafe] + b_cafe[cafe]*afternoon,
    c(a_cafe, b_cafe)[cafe] ~ multi_normal(c(a,b), Rho, sigma_cafe),
    a ~ normal(5,2),
    b ~ normal(-1,.5),
    sigma_cafe ~ exponential(1),
    sigma ~ exponential(1),
    Rho ~ lkj_corr(2)
  ), data=d, chains=4, log_lik = T
)
precis(m1)

# Copy plot code from book, but put all four plots on same
par(mfrow=c(2,2))
# compute unpooled estimates directly from data
a1 <- sapply( 1:N_cafes ,
              function(i) mean(wait[cafe_id==i & afternoon==0]) )
b1 <- sapply( 1:N_cafes ,
              function(i) mean(wait[cafe_id==i & afternoon==1]) ) - a1
# extract posterior means of partially pooled estimates
post <- extract.samples(m1)
a2 <- apply( post$a_cafe , 2 , mean )
b2 <- apply( post$b_cafe , 2 , mean )
# plot both and connect with lines
plot( a1 , b1 , xlab="intercept" , ylab="slope" ,
      pch=16 , col=rangi2 , ylim=c( min(b1)-0.1 , max(b1)+0.1 ) ,
      xlim=c( min(a1)-0.1 , max(a1)+0.1 ) )
points( a2 , b2 , pch=1 )
for ( i in 1:N_cafes ) lines( c(a1[i],a2[i]) , c(b1[i],b2[i]) )

# compute posterior mean bivariate Gaussian
Mu_est <- c( mean(post$a) , mean(post$b) )
rho_est <- mean( post$Rho[,1,2] )
sa_est <- mean( post$sigma_cafe[,1] )
sb_est <- mean( post$sigma_cafe[,2] )
cov_ab <- sa_est*sb_est*rho_est
Sigma_est <- matrix( c(sa_est^2,cov_ab,cov_ab,sb_est^2) , ncol=2 )
# draw contours
library(ellipse)
for ( l in c(0.1,0.3,0.5,0.8,0.99) ) {
  lines(ellipse(Sigma_est,centre=Mu_est,level=l),
        col=col.alpha("black",0.2))
}

# convert varying effects to waiting times
wait_morning_1 <- (a1)
wait_afternoon_1 <- (a1 + b1)
wait_morning_2 <- (a2)
wait_afternoon_2 <- (a2 + b2)
# plot both and connect with lines
plot( wait_morning_1 , wait_afternoon_1 , xlab="morning wait" ,
      ylab="afternoon wait" , pch=16 , col=rangi2 ,
      ylim=c( min(wait_afternoon_1)-0.1 , max(wait_afternoon_1)+0.1 ) ,
      xlim=c( min(wait_morning_1)-0.1 , max(wait_morning_1)+0.1 ) )
points( wait_morning_2 , wait_afternoon_2 , pch=1 )
for ( i in 1:N_cafes )
  lines( c(wait_morning_1[i],wait_morning_2[i]) ,
         c(wait_afternoon_1[i],wait_afternoon_2[i]) )
abline( a=0 , b=1 , lty=2 )

# now shrinkage distribution by simulation
v <- mvrnorm( 1e4 , Mu_est , Sigma_est )
v[,2] <- v[,1] + v[,2] # calculate afternoon wait
Sigma_est2 <- cov(v)
Mu_est2 <- Mu_est
Mu_est2[2] <- Mu_est[1]+Mu_est[2]
# draw contours
library(ellipse)
for ( l in c(0.1,0.3,0.5,0.8,0.99) )
  lines(ellipse(Sigma_est2,centre=Mu_est2,level=l),
        col=col.alpha("black",0.5))



# Now with rho=0
rho <- 0
cov_ab <- sigma_a*sigma_b*rho
Sigma <- matrix(c(sigma_a^2, cov_ab, cov_ab, sigma_b^2), ncol=2)
set.seed(5)
vary_effects <- mvrnorm(N_cafes, Mu, Sigma)
a_cafe <- vary_effects[,1]
b_cafe <- vary_effects[,2]
set.seed(22)
N_visits <- 10
afternoon <- rep(0:1, N_visits*N_cafes/2)
cafe_id <- rep(1:N_cafes, each=N_visits)
mu <- a_cafe[cafe_id] + b_cafe[cafe_id]*afternoon
sigma <- .5
wait <- rnorm(N_visits*N_cafes, mu, sigma)
d <- data.frame(cafe=cafe_id, afternoon=afternoon, wait=wait)

mrho0 <- ulam(
  alist(
    wait ~ normal(mu, sigma),
    mu <- a_cafe[cafe] + b_cafe[cafe]*afternoon,
    c(a_cafe, b_cafe)[cafe] ~ multi_normal(c(a,b), Rho, sigma_cafe),
    a ~ normal(5,2),
    b ~ normal(-1,.5),
    sigma_cafe ~ exponential(1),
    sigma ~ exponential(1),
    Rho ~ lkj_corr(2)
  ), data=d, chains=4
)
precis(mrho0)

# compute unpooled estimates directly from data
a1 <- sapply( 1:N_cafes ,
              function(i) mean(wait[cafe_id==i & afternoon==0]) )
b1 <- sapply( 1:N_cafes ,
              function(i) mean(wait[cafe_id==i & afternoon==1]) ) - a1
# extract posterior means of partially pooled estimates
post <- extract.samples(mrho0)
a2 <- apply( post$a_cafe , 2 , mean )
b2 <- apply( post$b_cafe , 2 , mean )
# plot both and connect with lines
plot( a1 , b1 , xlab="intercept" , ylab="slope" ,
      pch=16 , col=rangi2 , ylim=c( min(b1)-0.1 , max(b1)+0.1 ) ,
      xlim=c( min(a1)-0.1 , max(a1)+0.1 ) )
points( a2 , b2 , pch=1 )
for ( i in 1:N_cafes ) lines( c(a1[i],a2[i]) , c(b1[i],b2[i]) )

# compute posterior mean bivariate Gaussian
Mu_est <- c( mean(post$a) , mean(post$b) )
rho_est <- mean( post$Rho[,1,2] )
sa_est <- mean( post$sigma_cafe[,1] )
sb_est <- mean( post$sigma_cafe[,2] )
cov_ab <- sa_est*sb_est*rho_est
Sigma_est <- matrix( c(sa_est^2,cov_ab,cov_ab,sb_est^2) , ncol=2 )
# draw contours
library(ellipse)
for ( l in c(0.1,0.3,0.5,0.8,0.99) ) {
  lines(ellipse(Sigma_est,centre=Mu_est,level=l),
        col=col.alpha("black",0.2))
}

# convert varying effects to waiting times
wait_morning_1 <- (a1)
wait_afternoon_1 <- (a1 + b1)
wait_morning_2 <- (a2)
wait_afternoon_2 <- (a2 + b2)
# plot both and connect with lines
plot( wait_morning_1 , wait_afternoon_1 , xlab="morning wait" ,
      ylab="afternoon wait" , pch=16 , col=rangi2 ,
      ylim=c( min(wait_afternoon_1)-0.1 , max(wait_afternoon_1)+0.1 ) ,
      xlim=c( min(wait_morning_1)-0.1 , max(wait_morning_1)+0.1 ) )
points( wait_morning_2 , wait_afternoon_2 , pch=1 )
for ( i in 1:N_cafes )
  lines( c(wait_morning_1[i],wait_morning_2[i]) ,
         c(wait_afternoon_1[i],wait_afternoon_2[i]) )
abline( a=0 , b=1 , lty=2 )

# now shrinkage distribution by simulation
v <- mvrnorm( 1e4 , Mu_est , Sigma_est )
v[,2] <- v[,1] + v[,2] # calculate afternoon wait
Sigma_est2 <- cov(v)
Mu_est2 <- Mu_est
Mu_est2[2] <- Mu_est[1]+Mu_est[2]
# draw contours
library(ellipse)
for ( l in c(0.1,0.3,0.5,0.8,0.99) )
  lines(ellipse(Sigma_est2,centre=Mu_est2,level=l),
        col=col.alpha("black",0.5))



samp1 <- extract.samples(m1)
str(samp1)
rhosamp1 <- samp1$Rho[,1,2]
dens(rhosamp1)
samp2 <- extract.samples(mrho0)
str(samp2)
rhosamp2 <- samp2$Rho[,1,2]
dens(rhosamp2)

# Estimate of rho correctly moves to be centered on 0.


# 14M2
# As shown in book, rho=-.7
a <- 3.5
b <- -1
sigma_a <- 1
sigma_b <- .5
rho <- -.7
Mu <- c(a,b)
cov_ab <- sigma_a*sigma_b*rho
Sigma <- matrix(c(sigma_a^2, cov_ab, cov_ab, sigma_b^2), ncol=2)
N_cafes <- 20
library(MASS)
set.seed(5)
vary_effects <- mvrnorm(N_cafes, Mu, Sigma)
a_cafe <- vary_effects[,1]
b_cafe <- vary_effects[,2]
set.seed(22)
N_visits <- 10
afternoon <- rep(0:1, N_visits*N_cafes/2)
cafe_id <- rep(1:N_cafes, each=N_visits)
mu <- a_cafe[cafe_id] + b_cafe[cafe_id]*afternoon
sigma <- .5
wait <- rnorm(N_visits*N_cafes, mu, sigma)
d <- data.frame(cafe=cafe_id, afternoon=afternoon, wait=wait)

m3 <- ulam(
  alist(
    wait ~ normal(mu, sigma),
    mu <- a_cafe[cafe] + b_cafe[cafe]*afternoon,
    # c(a_cafe, b_cafe)[cafe] ~ multi_normal(c(a,b), Rho, sigma_cafe),
    a_cafe[cafe] ~ dnorm(a, sigma_a),
    b_cafe[cafe] ~ dnorm(b, sigma_b),
    a ~ normal(0,10),
    b ~ normal(0,10),
    # sigma_cafe ~ exponential(1),
    sigma ~ exponential(1),
    sigma_a ~ exponential(1),
    sigma_b ~ exponential(1)
    # Rho ~ lkj_corr(2)
  ), data=d, chains=4, log_lik = T
)
precis(m3)

compare(m1, m3)
# Can't get WAIC even though I used log_lik???

# 14M3

# 14M4

# 14M5


# 14H1
data("bangladesh")
d <- bangladesh
d$district_id <- as.integer(as.factor(d$district))
d$use_contraception <- d$use.contraception
d$age_centered <- d$age.centered

m1 <- ulam(alist(
  use_contraception ~ dbinom(1, p),
  logit(p) <- a_district[district_id],
  a_district[district_id] ~ dnorm(a, sigma),
  a ~ dnorm(0, 10),
  sigma ~ dexp(1)
), data=d %>% dplyr::select(use_contraception, age_centered, district_id))
precis(m1)
# plot(precis(m1))

m2 <- ulam(alist(
  use_contraception ~ dbinom(1, p),
  logit(p) <- a_district[district_id] + b_district[district_id]*urban,
  c(a_district, b_district)[district_id] ~ multi_normal(c(a, b_urban), Rho, sigma),
  a ~ dnorm(0, 10),
  b_urban ~ dnorm(0, 1),
  sigma ~ dexp(1),
  Rho ~ dlkjcorr(2)
), data=d %>% dplyr::select(use_contraception, age_centered, district_id, urban), log_lik = T)
precis(m2)


# 14H2

# 14H3

# 14H4
data("Oxboys")
d <- Oxboys
str(d)
precis(d)
ggplot(d, aes(age, height)) + facet_wrap(. ~ Subject) + geom_point()
d$height_norm <- with(d, (height - mean(height)) / sd(height))
precis(d)

require(MASS)
m1 <- ulam(alist(
  height_norm ~ dnorm(mu, sigma),
  mu <- a + a_ind[Subject] + (b_age + b_age_ind[Subject])*age,
  c(a_ind, b_age_ind)[Subject] ~ multi_normal(0, Rho, sigma_ind),
  a ~ dnorm(0, 100),
  b_age ~ dnorm(0, 1),
  sigma_ind ~ dexp(1),
  Rho ~ dlkjcorr(2),
  sigma ~ dexp(1)
), data=d, chains=3, cores=3)
precis(m1, depth=2)

post <- extract.samples(m1)
a_ind_means <- colMeans(post$a_ind)
b_age_ind_means <- colMeans(post$b_age_ind)
plot(a_ind_means, b_age_ind_means)
abline(a=0,b=1, col=2)

