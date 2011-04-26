# gilmour.wheat97.r
# Time-stamp: <01 Apr 2011 16:57:50 c:/x/rpack/agdata/raw/gilmour.wheat97.r>

library(asreml)
library(kw)
library(Hmisc)
library(lattice)
setwd("c:/x/rpack/agdata/")
dat <- import("data/gilmour.wheat97.txt")
gilmour.wheat97 <- dat

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

desplot(yield~ column*row, data=dat, num=gen, outline1=rep)

# ----------------------------------------------------------------------------

dat <- gilmour.wheat97


