# Alcroft2003.r
# Time-stamp: <06 Apr 2011 15:52:08 c:/x/rpack/agridat/raw/allcroft.lodging.r>

library(kw)
library(Hmisc)
library(lattice)
setwd("c:/x/rpack/agridat")
dat <- read.csv("raw/allcroft.lodging.csv")

allcroft.lodging <- dat
#export(allcroft.lodging, "data/allcroft.lodging.RData")
promptData(allcroft.lodging, "man/allcroft.lodging.Rd")

# ----------------------------------------------------------------------------

dat <- allcroft.lodging

# Transformation
dat$sy <- sqrt(dat$y)

hist(subset(dat, sy>0)$sy, breaks=20)

# Variety 4 has no lodging anywhere, so add a small amount
dat[dat$env==5 & dat$gen==4,]$sy <- .01

# Tobit model
require(survival)
m1 <- survreg(Surv(sy, sy>0, type='left')~ -1 + gen+env, data=dat,
              dist='gaussian')
summary(m1)

# Allcroft table 2.
# To get means, predict all combinations, then average across other levels
p1 <- predict(m1)
mean(p1)
round(tapply(p1, dat$env, mean),2) # trial/env means
round(tapply(p1, dat$gen, mean),2) # variety/gen means
# Residual variance
var(resid(m1))
# Residual plot.  Allcroft figure 3.
plot(resid(m1)~predict(m1), cex=.5)


# Same model using zelig
require(Zelig)
m2 <- zelig(sy~ gen+env, below=0, above=Inf, model="tobit", data=dat)
summary(m2)


# Bayesian tobit model using zelig
m3 <- zelig(sy ~ -1 + gen + env, below=0, above=100, model="tobit.bayes",
            data=dat)
s3 <- summary(m3)
# Average env effect is sum(0+e2+e3+...+e7)/7
avgenv <- apply(m3$coefficients[,33:38], 1, function(x) sum(x)/7)
# Add avgenv to each genotype
p3 <- m3$coefficients[,1:32] + avgenv
# Probability of observing 0.  Compare to Allcroft Table 2,.
round(apply(p3, 2, function(x) mean(x<0)),2)
# Expected squared value of non-zeros.  Somewhat different from Allcroft.
round(apply(p3, 2, function(x) { x <- x[x>0]; mean(x^2) }),2)

# Density plots
plot(p3[,1:4])


# ----------------------------------------------------------------------------

## require(MCMCpack)
## MCMCtobit
## m4 <- MCMCtobit(sy~gen+env, data=dat, below=0, mcmc=30000,
##                 verbose=1000)
## s4 <- summary(m4)
