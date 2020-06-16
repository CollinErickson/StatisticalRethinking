# chapter 9 notes
# King Markov, not efficient, trying to do it before looking at code in book
n <- 1e4
pos <- numeric(n)
pos[1] <- sample(1:10, 1)
for (i in 1:(n-1)) {
  coin <- sample(1:2,1)
  if (coin==1) {
    seashells <- pos[i]-1
  } else {
    seashells <- pos[i]+1
  }
  if (seashells==0) {seashells <- 10}
  if (seashells==11) {seashells <- 1}
  stones <- pos[i]
  if (seashells >= stones) {pos[i+1] <- seashells}
  else if (runif(1) < seashells/stones) {pos[i+1] <- seashells}
  else {pos[i+1] <- pos[i]}
}
table(pos) / n * 55
plot(table(pos) / n * 55)
curve(1*x, col=2, add=T)
plot(pos)
plot(pos, type='l')

# shorter code
n <- 1e4
pos <- numeric(n)
pos[1] <- sample(1:10, 1)
for (i in 1:(n-1)) {
  seashells <- pos[i] + ifelse(runif(1)<.5, -1,1)
  seashells <- ifelse(seashells==0, 10, ifelse(seashells==11, 1, seashells))
  pos[i+1] <- ifelse(runif(1) < seashells/pos[i], seashells, pos[i])
}
table(pos) / n * 55




## HMC examples


## R code 9.11
library(rethinking)
data(rugged)
d <- rugged
d$log_gdp <- log(d$rgdppc_2000)
dd <- d[ complete.cases(d$rgdppc_2000) , ]
dd$log_gdp_std <- dd$log_gdp / mean(dd$log_gdp)
dd$rugged_std <- dd$rugged / max(dd$rugged)
dd$cid <- ifelse( dd$cont_africa==1 , 1 , 2 )

## R code 9.12
m8.3 <- quap(
  alist(
    log_gdp_std ~ dnorm( mu , sigma ) ,
    mu <- a[cid] + b[cid]*( rugged_std - 0.215 ) ,
    a[cid] ~ dnorm( 1 , 0.1 ) ,
    b[cid] ~ dnorm( 0 , 0.3 ) ,
    sigma ~ dexp( 1 )
  ) , data=dd )
precis( m8.3 , depth=2 )

## R code 9.13
dat_slim <- list(
  log_gdp_std = dd$log_gdp_std,
  rugged_std = dd$rugged_std,
  cid = as.integer( dd$cid )
)
str(dat_slim)

## R code 9.14
m9.1 <- ulam(
  alist(
    log_gdp_std ~ dnorm( mu , sigma ) ,
    mu <- a[cid] + b[cid]*( rugged_std - 0.215 ) ,
    a[cid] ~ dnorm( 1 , 0.1 ) ,
    b[cid] ~ dnorm( 0 , 0.3 ) ,
    sigma ~ dexp( 1 )
  ) , data=dat_slim , chains=1 )

stancode(m9.1)
show(m9.1)
## R code 9.15
precis( m9.1 , depth=2 )
