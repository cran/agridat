# shafii.r
# Time-stamp: <25 Mar 2011 22:40:04 c:/x/rpack/agdata/raw/shafii.r>

library(asreml)
library(kw)
library(Hmisc)
library(lattice)
setwd("c:/x/rpack/agdata/raw/")
dat <- import("")
dat <- transform(dat, rep=factor(rep))
str(dat)
describe(dat)


setwd("c:/x/rpack/agdata")
dat <- import("raw/shafii.rapeseed.csv")
shafii.rapeseed <- dat
export(shafii.rapeseed, "c:/x/rpack/agdata/data/shafii.rapeseed.txt")
promptData(shafii.rapeseed, "c:/x/rpack/agdata/man/shafii.rapeseed.Rd")

# ----------------------------------------------------------------------------




