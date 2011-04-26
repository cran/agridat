# gomez.stripsplitplot.r
# Time-stamp: <14 Feb 2011 15:09:38 c:/x/rpack/agdata/raw/gomez.stripsplitplot.r>

library(asreml)
library(kw)
library(Hmisc)
library(lattice)
setwd("c:/x/rpack/agdata/raw/")
dat <- import("gomez.stripsplitplot.csv")

desplot(variety~x+y|rep, dat, outline1=nitro, num=planting)

gomez.stripsplitplot <- dat
promptData(gomez.stripsplitplot, "c:/x/rpack/agdata/man/gomez.stripsplitplot.Rd")

# ----------------------------------------------------------------------------

# Gomez table 4.19, ANOVA of strip-split-plot design
dat <- transform(dat, nf=factor(nitro))
m1 <- aov(yield ~ nf * variety * planting +
          Error(rep + rep:nf + rep:variety + rep:nf:variety), data=dat)
summary(m1)

# High nitro strips (columns 2 4 7) are easy to see
levelplot(yield~x+y, dat, col.regions=topo.colors(16))

# There is a noticeable linear trend along the y coordinate which may be
# an artifact that blocking will remove, or may need to be modeled.
# Note the outside values in the high-nitro boxplot.
require(HH)
interaction2wt(yield~nitro+variety+planting+y, dat, x.relation="free")
