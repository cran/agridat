# wheatrowcol.r
# Time-stamp: <06 Apr 2011 16:32:15 c:/x/rpack/agridat/raw/kempton.rowcol.r>

library(asreml)
library(kw)
library(Hmisc)
library(lattice)

setwd("c:/x/rpack/agdata/")
dat <- read.csv("raw/kempton.rowcol.csv")
d1 <- subset(dat, rep==1)
d2 <- subset(dat, rep==2)
d1$row <-  11 - d1$row
d2$row <- 6-d2$row
dat <- rbind(d1,d2)
dat <- transform(dat, rep=factor(rep), gen=factor(gen), row=factor(row),
                 col=factor(col))
str(dat)

kempton.rowcol <- dat
save(kempton.rowcol, file="data/kempton.rowcol.RData")
#promptData(kempton.rowcol, "man/kempton.rowcol.Rd")
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

lib(kw)
desplot(yield~col*row, dat, num=gen, outline1=rep)


# Model with rep, row, col as random (Kempton, bottom of page 62)
# Use "-1" so that the vcov matrix doesn't include intercept
library(lme4)
m1 <- lmer(yield ~ -1 + gen + rep + (1|rep:row) + (1|rep:col), data=dat)

# Variances
print(m1, corr=FALSE)

# SEDs

covs <- as.matrix(vcov(m1)[1:35, 1:35])
vars <- diag(covs)
vdiff <- outer(vars, vars, "+") - 2 * covs
sed <- sqrt(vdiff[upper.tri(vdiff)])
min(sed) # Minimum SED
mean(sed) # Average SED
max(sed) # Maximum SED
