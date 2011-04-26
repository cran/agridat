# gomez.fractionalfactorial.r
# Time-stamp: <14 Feb 2011 09:35:53 c:/x/rpack/agdata/raw/gomez.fractionalfactorial.r>

library(asreml)
library(kw)
library(Hmisc)
library(lattice)


setwd("c:/x/rpack/agdata/")
dat <- import("raw/gomez.fractionalfactorial.csv")
gomez.fractionalfactorial <- dat

#export(gomez.fractionalfactorial, "data/gomez.fractionalfactorial.RData")
promptData(gomez.fractionalfactorial, "man/gomez.fractionalfactorial.Rd")

# ----------------------------------------------------------------------------

dat <- gomez.fractionalfactorial

# Gomez, Figure 4.8
with(dat, plot(y~x, type='n'))
with(dat, text(x,y,trt, col=as.numeric(block)))

# Split treatment into individual factors
dat <- transform(dat,
                 a = -1 + 2 * grepl('a',trt),
                  b = -1 + 2 * grepl('b',trt),
                 c = -1 + 2 * grepl('c',trt),
                 d = -1 + 2 * grepl('d',trt),
                 e = -1 + 2 * grepl('e',trt),
                 f = -1 + 2 * grepl('f',trt))

# Gomez table 4.24, trt SS totalled together.
# Why didn't Gomez nest block within rep?
m0 <- lm(yield~rep*block+trt, dat)
anova(m0)

# Gomez table 4.24, trt SS split apart
m1 <- lm(yield ~ rep * block + (a+b+c+d+e+f)^3, dat)
anova(m1)

require(FrF2)
aliases(m1)
MEPlot(m1)

levelplot(yield~x*y, dat) # trt abcdef has the highest yield

