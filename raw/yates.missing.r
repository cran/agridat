# yates.missing.r
# Time-stamp: <28 Mar 2011 13:19:04 c:/x/rpack/agdata/raw/yates.missing.r>

library(asreml)
library(kw)
library(Hmisc)
library(lattice)
setwd("c:/x/rpack/agdata/raw/")
dat <- import("")
dat <- transform(dat, rep=factor(rep))
str(dat)
describe(dat)

dmat0 <- matrix(c(3.55, 2.29, NA, 2.00, 3.34, 3.83, 3.86, 3.50, 2.23, 2.91,
2.30, 4.03, 2.54, 2.82, 3.29, 2.93, NA, 2.55, 2.20, 2.30,
3.96, 3.62, 3.46, 2.50, 2.94, 3.70, 3.82, 2.54, 3.18, 3.69,
2.99, 3.99, 2.90, 3.97, 4.49, 4.70, 3.86, NA, 3.50, 3.59,
NA, 3.07, 3.49, 1.07, 3.99, 3.48, 3.80, 3.68, 3.24, 2.70,
2.36, 3.47, 2.64, 3.17, 3.26, 3.28, NA, NA, 3.07, 3.12,
2.16, 2.34, 1.96, 2.60, 3.77, NA, 3.20, 3.47, 2.67, 3.33,
3.16, 2.52, 2.39, 3.68, NA, NA, 3.85, 3.36, 2.50, 4.13),
ncol=10, byrow=TRUE)
rownames(dmat0) <- c('O','N','K','P','NK','NP','KP','NKP')

dat <- melt(dmat0)
names(dat) <- c('trt','block','y')
dat$block <- paste('B',formatC(dat$block,width=2,flag='0'), sep="")
yates.missing <- dat

yates.missing <- transform(yates.missing, block=factor(block))
# ----------------------------------------------------------------------------
require(reshape2)
dat <- yates.missing
mat0 <- acast(dat[, c('trt','block','y')], trt~block,
               id.var=c('trt','block'))

# Use lm to estimate missing values.  The estimated missing values
# are the same as in Yates (1933)
m1 <- lm(y~trt+block, dat)
dat$pred <- predict(m1, new=dat[, c('trt','block')])
dat$filled <- ifelse(is.na(dat$y), dat$pred, dat$y)
mat1 <- acast(dat[, c('trt','block','pred')], trt~block,
               id.var=c('trt','block'))

# Another method to estimate missing values via PCA
require(pcaMethods)
m2 <- pca(mat0, method="nipals", center=FALSE, nPcs=3)
mat2 <- m2@scores %*% t(m2@loadings)

# Compare
ord <- c("O","N","K","P","NK","NP","KP","NKP")
print(mat0[ord,], na.print=".")
round(mat1[ord,] ,2)
round(mat2[ord,] ,2)

# SVD with 3 components recovers original data better
sum((mat0-mat1)^2, na.rm=TRUE)
sum((mat0-mat2)^2, na.rm=TRUE) # Smaller SS => better fit
