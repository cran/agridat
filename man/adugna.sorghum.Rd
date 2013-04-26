\name{adugna.sorghum}
\alias{adugna.sorghum}
\docType{data}
\title{
  Sorghum yields at 3 locations across 5 years
}
\description{
  Sorghum yields at 3 locations across 5 years
}
\usage{data(adugna.sorghum)}
\format{
  A data frame with 289 observations on the following 6 variables.
  \describe{
    \item{\code{gen}}{Genotype factor, 28 levels}
    \item{\code{trial}}{Trial factor, 2 levels}
    \item{\code{env}}{Environment factor, 13 levels}
    \item{\code{yield}}{Yield kg/ha}
    \item{\code{year}}{Year, 2001-2005}
    \item{\code{loc}}{Loc factor, 3 levels}
  }
}
\details{
  Sorghum yields at 3 locations across 5 years.
  The trials were carried out at three locations in dry, hot lowlands of
  Ethiopia:
  Melkassa (39 deg 21 min E, 8 deg 24 min N),
  Mieso (39 deg 22 min E, 8 deg 41 min N) and
  Kobo (39 deg 37 min E, 12 deg 09 min N).
  Trial 1 was 14 hybrids and one open-pollinated variety.  Trial 2 was
  12 experimental lines.
}
\source{
  Asfaw Adugna (2008), Assessment of yield stability in sorghum using
  univariate and multivariate statistical approaches, \emph{Hereditas},
  145, 28--37.

  Used with permission of Asfaw Adugna.
}

\examples{
data(adugna.sorghum)

dat <- adugna.sorghum

# Genotype means match Adugna
tapply(dat$yield, dat$gen, mean)

# CV for each genotype.  G1..G15 match, except for G2.
# The table in Adugna scrambles the means for G16..G28
require(reshape2)
mat <- acast(dat, gen~env,  value.var="yield")
round(sqrt(apply(mat, 1, var, na.rm=TRUE)) / apply(mat, 1, mean, na.rm=TRUE) * 100,2)

# Shukla stability.  G1..G15 match Adugna.  Can't match G16..G28.
dat1 <- droplevels(subset(dat, trial=="T1"))
mat1 <- acast(dat1, gen~env,  value.var="yield")
w <- mat1; k=15; n=8  # k=p gen, n=q env
w <- sweep(w, 1, rowMeans(mat1, na.rm=TRUE))
w <- sweep(w, 2, colMeans(mat1, na.rm=TRUE))
w <- w + mean(mat1, na.rm=TRUE)
w <- rowSums(w^2, na.rm=TRUE)
sig2 <- k*w/((k-2)*(n-1)) - sum(w)/((k-1)*(k-2)*(n-1))
round(sig2/10000,1) # Genotypes in T1 are divided by 10000

}
\keyword{datasets}