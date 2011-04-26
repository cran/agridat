# yates.oats.r
# Time-stamp: <10 Mar 2011 13:19:18 c:/x/rpack/agdata/raw/yates.oats.r>

library(asreml)
library(kw)
library(Hmisc)
library(lattice)
setwd("c:/x/rpack/agdata/raw/")

yates.oats <- within(expand.grid(y = 1:18, x = 1:4), {
  block <- factor(rep(c('B2', 'B4', 'B1', 'B2', 'B4', 'B1',
                        'B6', 'B3', 'B5', 'B6', 'B3', 'B5'),
                      each = 6))
  gen <- factor(c(1, 1, 3, 3, 2, 2, 2, 2, 1, 1, 3, 3, 2, 2, 1,
                      1, 3, 3, 1, 1, 3, 3, 2, 2, 2, 2, 1, 1, 3, 3, 2, 2,
                      1, 1, 3, 3, 3, 3, 2, 2, 1, 1, 3, 3, 1, 1, 2, 2, 1,
                      1, 2, 2, 3, 3, 3, 3, 2, 2, 1, 1, 3, 3, 1, 1, 2, 2,
                      1, 1, 2, 2, 3, 3))
  levels(gen) <- c("Victory", "Golden Rain", "Marvellous")
  nitro <- 0.2 * c(1, 0, 2, 3, 3, 1, 2, 1, 1, 3, 1, 2, 2, 0, 3, 0,
                   1, 3, 2, 3, 0, 1, 0, 2, 3, 0, 2, 0, 3, 0, 3, 1, 2, 1,
                   0, 2, 2, 0, 0, 3, 3, 2, 1, 2, 0, 2, 2, 3, 3, 1, 3, 0,
                   0, 2, 3, 1, 1, 2, 1, 0, 0, 3, 1, 3, 1, 0, 0, 2, 1, 2,
                   1, 3)
  yield <- c(91, 61, 121, 144, 149, 108, 132, 103, 89, 122, 89, 104,
             161, 117, 174, 111, 140, 156, 97, 100, 96, 124, 70, 126,
             133, 64, 81, 74, 117, 70, 141, 114, 157, 130, 105, 118,
             119, 97, 89, 104, 113, 118, 129, 132, 68, 112, 89, 96, 116,
             90, 126, 80, 63, 109, 121, 99, 82, 86, 74, 53, 89, 124, 64,
             86, 102, 60, 62, 100, 82, 94, 70, 99)
})

save(yates.oats, file="c:/x/rpack/agdata/data/yates.oats.rda")
dat <- yates.oats

# ----------------------------------------------------------------------------

require(lattice)
dotplot(yield~factor(x)|nitro, dat, type=c('p','smooth'), xlab='x')
xyplot(yield ~ x|factor(nitro), oats, type = c('p', 'smooth'),
       as.table = TRUE)
