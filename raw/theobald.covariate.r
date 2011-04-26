# theobald.covariate.r
# Time-stamp: <08 Apr 2011 10:21:56 c:/x/rpack/agridat/raw/theobald.covariate.r>

library(asreml)
library(kw)
library(Hmisc)
library(lattice)
setwd("c:/x/rpack/agridat/")

dat <- import("raw/theobald.covariate.csv")
dm <- melt(dat[,1:12], id.var=c('year','env'))
names(dm) <- c('year','env','gen','yield')
dm$chu <- lookup(dm$year %&% dm$env, dat$year %&% dat$env, dat$chu)
dm <- subset(dm, !is.na(yield))
# dm$year <- factor(dm$year)

theobald.covariate <- dm
export(theobald.covariate, "data/theobald.covariate.txt")
promptData(theobald.covariate, "man/theobald.covariate.Rd")

# ----------------------------------------------------------------------------

theo <- theobald.covariate

# REML estimates (Means) in table 3 of Theobald 2002
require(lme4)
m0 <- lmer(yield ~ -1 + gen + (1|year/env) + (1|gen:year), data=theo)
round(fixef(m0),2)

# ----------------------------------------------------------------------------

\dontrun {

# Theobald model 3.2 with 'Expert' prior
require(rjags)

ymat <- acast(theo, year+env~gen, value_var='yield')
chu <- acast(theo, year+env~., mean, value_var='chu', na.rm=TRUE)
chu <- as.vector(chu - mean(chu))  # Center the covariate
theo$yr <- as.numeric(theo$year)
yridx <- as.vector(acast(theo, year+env~., mean, value_var='yr', na.rm=TRUE))
theo$loc <- as.numeric(theo$env)
locidx <- acast(theo, year+env~., mean, value_var='loc', na.rm=TRUE)
locidx <- as.vector(locidx)

dat <- list(nVar = 10, nYear = 5, nLoc = 7, nYL = 29, yield = ymat,
            chu = chu, year = yridx, loc = locidx) 
            
model.bug <- "
model {

# Data
# nVar = 10          number of varieties
# nYear = 5          number of years
# nLoc = 7           number of locs
# nYL = 29           number of year by loc combinations in the data
# yield[nYL, nVar]   yield in year-by-loc combination h for variety i
# chu[nYL]           covariate: corn heat units, centered
# year[nYL]          year for year-by-loc combination
# loc[nYL]           loc for year-by-loc combination

for (h in 1:nYL){
  for (i in 1:nVar){
    yield[h,i] ~ dnorm(mu[h, i], tau)
    # Model 3.2, no covariate
    mu[h,i] <- phi[i, year[h]] + xi[loc[h], year[h]]
    # Model 3.3, with covariate
#    mu[h,i] <- phi[i, year[h]] + beta[i] * chu[h] + xi[loc[h], year[h]]
}}
for (i in 1:nVar){
  alpha[i] ~ dnorm(mu.v, tau.v)       # effect of variety i
  beta[i]  ~ dnorm(mu.c, tau.c)       # slope for variety i
  for (k in 1:nYear){
  phi[i,k] ~ dnorm(alpha[i], tau.vy)  # effect of year-by-variety combination
#  vef[i] <- alpha[i] - mean(alpha[])
}}
for (k in 1:nYear){
  pi[k] ~ dnorm(mu.y, tau.y)          # effect of year k
  for (j in 1:nLoc){
    xi[j,k] ~ dnorm(pi[k], tau.ly)    # effect of year-by-loc combination
}}

# 'Expert' Priors
mu.c   ~ dnorm(5,4)          # N(5, 0.25) expected slope
mu.v   ~ dnorm(6, 20)        # N(6, 0.05) expected variety effect
mu.y   ~ dnorm(0, 4)         # N(0, 0.25) expected year effect
tau.v  ~ dgamma(25, 6.25)    # variance of 0.25 with 50 df
tau.y  ~ dgamma(5, 1.25)     #             0.25      10 df
tau.ly ~ dgamma(25, 25)      #             1         50 df
tau.vy ~ dgamma(12.5, 0.625) #             0.05      25 df
tau    ~ dgamma(12.5, 2.5)   #             0.20      25 df
tau.c  ~ dgamma(10, 40)      #             4         20 df

# Derived values, not used in model
# alpha.bar <- mean(alpha[]) # mean variety effect
# beta.bar <- mean(beta[])   # mean slope
# sigsq <- pow(tau, -1)
# sigsq.c <- pow(tau.c, -1)
# sigsq.v <- pow(tau.v, -1)
# sigsq.y <- pow(tau.y, -1)
# sigsq.ly <- pow(tau.ly, -1)
# sigsq.vy <- pow(tau.vy, -1)
}
"

tc <- textConnection(model.bug)
m1 <- jags.model(file=tc, data=dat, n.chains=2)
close(tc)

# Table 3, Variety deviations from means (Expert prior)
c1 <- coda.samples(m1, variable.names=(c('alpha')),
                   n.iter=10000, thin=10)
s1 <- summary(c1)
effs <- s1$statistics[,'Mean']
rev(sort(round(effs - mean(effs), 2))) # Perfect match

}

