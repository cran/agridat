\name{hessling.argentina}
\alias{hessling.argentina}
\docType{data}
\title{
  Relation between wheat yield and weather in Argentina
}
\description{
  Relation between wheat yield and weather in Argentina
}
\usage{data(hessling.argentina)}
\format{
  A data frame with 30 observations on the following 15 variables.
  \describe{
    \item{\code{yield}}{Average yield, kg/ha}
    \item{\code{year}}{Year}
    \item{\code{p05}}{Precipitation (mm) in May}
    \item{\code{p06}}{Precip in June}
    \item{\code{p07}}{Precip in July}
    \item{\code{p08}}{Precip in August}
    \item{\code{p09}}{Precip in Septempber}
    \item{\code{p10}}{Precip in October}
    \item{\code{p11}}{Precip in November}
    \item{\code{p12}}{Precip in December}
    \item{\code{t06}}{June temperature deviation from normal, deg Celsius}
    \item{\code{t07}}{July temp deviation}
    \item{\code{t08}}{August temp deviation}
    \item{\code{t09}}{September temp deviation}
    \item{\code{t10}}{October temp deviation}
    \item{\code{t11}}{November temp deviation}
  }
}
\details{
  In Argentina wheat is typically sown May to August.  Harvest begins in
  November or December.
}
\source{
  N. A. Hessling, 1922.
  Relations between the weather and the yield of wheat in the
  Argentine republic, \emph{Monthly Weather Review}, 50, 302-308.
  \url{http://dx.doi.org/10.1175/1520-0493(1922)50<302:RBTWAT>2.0.CO;2}
}
\examples{
dat <- hessling.argentina

# Fig 1 of Hessling.  Use avg Aug-Nov temp to predict yield
dat <- transform(dat, avetmp=(t08+t09+t10+t11)/4) # Avg temp
m0 <- lm(yield ~ avetmp, dat)
plot(yield~year, dat, ylim=c(200,1400), type='l',
main="Observed (black) and predicted yield (blue)")
lines(fitted(m0)~year, dat, col="blue")

# A modern, PLS approach
require(pls)
yld <- dat[,"yield",drop=FALSE]
yld <- as.matrix(sweep(yld, 2, colMeans(yld)))
cov <- dat[,c("p06","p07","p08","p09","p10","p11", "t08","t09","t10","t11")]
cov <- as.matrix(scale(cov))
m2 <- plsr(yld~cov)
biplot(m2, which="x", var.axes=TRUE)
}
\keyword{datasets}