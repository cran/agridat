# besag.met.r
# Time-stamp: <01 Mar 2011 14:01:21 c:/x/rpack/agdata/raw/besag.met.r>

library(asreml)
library(kw)
library(lattice)
library(nlme)

setwd("c:/x/rpack/agdata/")
dat <- import("data/besag.met.txt")

besag.met <- dat
#promptData(besag.met, "man/besag.met.Rd")

# ----------------------------------------------------------------------------

dat <- besag.met

desplot(yield~col*row|county, data=dat, outline1=rep, outline2=block)

# Use reps
dat <- dat[order(dat$gen),]
m1 <- asreml(yield ~ county, data=dat,
             random=~gen,
               rcov = ~ at(gen):units)
summary(m1)$varcomp
p1 <- predict(m1, classify="gen")$predictions$pvals

d2 <- transform(dat, xf=factor(col), yf=factor(row))
d2 <- sort(d2, by=~county+xf+yf)
m1 <- asreml(yield ~ county, data=d2,
             random=~idh(gen),
             rcov = ~ at(county):ar1(xf):ar1(yf))

# ----------------------------------------------------------------------------

# Heteroskedastic variance model
# asreml takes 1 second, lme 73 seconds, SAS 30 minutes

# Average reps
datm <- aggregate(yield ~ county + gen, data=dat, FUN=mean)

# asreml Using 'rcov' ALWAYS requires sorting the data
require(asreml)
datm <- datm[order(datm$gen),]
m2a <- asreml(yield ~ gen, data=datm,
              random = ~ county,
              rcov = ~ at(gen):units,
              predict=predict.asreml(classify="gen"))
summary(m2a)$varcomp

# lme
require(nlme)
m2l <- lme(yield ~ -1 + gen, data=datm, random=~1|county,
               weights = varIdent(form=~ 1|gen))
m2l$sigma^2 * c(1, coef(m2l$modelStruct$varStruct, unc = F))^2

# We get the same results from asreml & lme
plot(m2a$gammas[-1],
     m2l$sigma^2 * c(1, coef(m2l$modelStruct$varStruct, unc = F))^2)


