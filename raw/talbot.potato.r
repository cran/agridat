# potato.r
# Time-stamp: <05 Apr 2011 14:32:05 c:/x/rpack/agridat/raw/talbot.potato.r>

# http://www.bioss.ac.uk/smart/unix/mplsgxe/example/t2.g
# http://www.bioss.ac.uk/smart/unix/mplsgxe/slides/

library(pls)

setwd("c:/x/rpack/agridat")

# Potato variety characteristics
char = read.csv("raw/talbot.potato_x.csv", row.names=1)
char = as.matrix(char)

# Potato variety yields
yield = read.csv("raw/talbot.potato_y.csv", row.names=1)
yield = as.matrix(yield)

talbot.potato <- list(yield=yield, char=char)
export(talbot.potato, "data/talbot.potato.rda")

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------



# How do I get table 5?  Table 6?
# Cross validation
plot(MSEP(m1))
plot(RMSEP(m1))

potato.cv <- crossval(m1, segments=5)

plot(m1, ncomp=3, asp=1, line=TRUE)

# Loadings plot
plot(m1,"loadings", comps=1:3, legendpos='topleft')
abline(h=0)
# Score plot
plot(m1,"score", comps=1:3)

plot(m1,"coefficients", comps=1:3)

# ----------------------------------------------------------------------------


coefplot(m1)

scoreplot(m1)
biplot(m1, 1:2, "x")
biplot(m1, 1:2, "y")
biplot(m1, 1:2, "scores")
biplot(m1, 1:2, "loadings")

predplot(m1, ncomp=2)
corrplot(m1, comps=1:2, labels="names")


# ----------------------------------------------------------------------------

# PLS
char <- talbot.potato$char
yield <- talbot.potato$yield

# Transform columns to zero mean and unit variance
char <- scale(char)
yield <- scale(yield)


#dat <- data.frame(I(yield),I(char))
#m1 <- plsr(yield~char, data=dat, validation="LOO", ncomp=5)
require(pls)
m1 <- plsr(yield~char, ncomp=3)
summary(m1)

# Loadings factor 1
lo <- loadings(m1)[,1,drop=FALSE]
round(-1*lo[order(-1*lo),1,drop=FALSE],2)
