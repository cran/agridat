\name{gomez.seedrate}
\alias{gomez.seedrate}
\docType{data}
\title{
  Rice yield at six different densities
}
\description{
  Rice yield at six different densities
}
\usage{data(gomez.seedrate)}
\format{
  A data frame with 24 observations on the following 3 variables.
  \describe{
    \item{\code{rate}}{kg seeds per hectare}
    \item{\code{rep}}{rep, four levels}
    \item{\code{yield}}{yield, kg/ha}
  }
}
\details{
  Rice yield at six different densities.
}
\source{
  Gomez, K.A. and Gomez, A.A.. 1984,
  \emph{Statistical Procedures for Agricultural Research}.
  Wiley-Interscience. Page 26.

  Used with permission of Kwanchai Gomez.
}

\examples{
data(gomez.seedrate)

dat <- gomez.seedrate

require(lattice)
#xyplot(yield ~ rate, data=dat, group=rep)
bwplot(yield ~ rate, data=dat, horizontal=FALSE)
# Quadratic response
m1 <- lm(yield ~ poly(rate,2), dat)

}
\keyword{datasets}