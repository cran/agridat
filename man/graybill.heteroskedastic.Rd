\name{graybill.heteroskedastic}
\alias{graybill.heteroskedastic}
\docType{data}
\title{
  Wheat varieties with heteroskedastic yields
}
\description{
  Wheat varieties with heteroskedastic yields
}
\usage{data(graybill.heteroskedastic)}
\format{
  A data frame with 52 observations on the following 3 variables.
  \describe{
    \item{\code{env}}{Environment factor, 13 levels}
    \item{\code{gen}}{Genotype factor, 4 levels}
    \item{\code{yield}}{Yield}
  }
}
\details{
  Yield of 4 varieties of wheat at 13 locations in Oklahoma, USA.

  The data was used to explore variability between varieties.
}
\source{
  F. A. Graybill, 1954. Variance heterogeneity in a randomized
  block design, \emph{Biometrics}, 10, 516-520.
}
\references{
  Hans-Pieter Piepho, 1994.
  Missing observations in the analysis of stability, \emph{Heredity},
  72, 141--145.
}
\examples{
dat <- graybill.heteroskedastic

# Genotypes are obviously not homoscedastic
boxplot(yield ~ gen, dat)

# Shukla stability variance of each genotype, same as Grubbs' estimate
# Matches Piepho 1994 page 143.
# Do not do this!  Nowadays, use mixed models instead.
require("reshape2")
datm <- acast(dat, gen~env)
w <- datm
w <- sweep(w, 1, rowMeans(datm))
w <- sweep(w, 2, colMeans(datm))
w <- w + mean(datm)
w <- rowSums(w^2)
k=4; n=13
sig2 <- k*w/((k-2)*(n-1)) - sum(w)/((k-1)*(k-2)*(n-1))
}
