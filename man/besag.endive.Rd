\name{besag.endive}
\alias{besag.endive}
\docType{data}
\title{
  Presence of footroot disease in an endive field
}
\description{
  Presence of footroot disease in an endive field
}

\format{
  A data frame with 2506 observations on the following 3 variables.
  \describe{
    \item{\code{col}}{column ordinate}
    \item{\code{row}}{row ordinate}
    \item{\code{disease}}{plant is diseased, Y=yes,N=no}
  }
}
\details{
  In a field of endives, does each plant have footrot, or not?

  Modeled as an autologistic distribution.

  We assume the endives are a single genotype.

  Besag (1978) may have had data taken at 4 time points.  This data
  was extracted from Friel and Pettitt.  It is not clear what, if any,
  time point was used.
}
\source{
  J Besag (1978). Some Methods of Statistical Analysis for Spatial Data.
  \emph{Bulletin of the International Statistical Institute}, 47, 77-92.
}
\references{
  N Friel & A. N Pettitt (2004). Likelihood Estimation and Inference
  for the Autologistic Model.
  \emph{Journal of Computational and Graphical Statistics}, 13:1,
  232-246. \url{http://dx.doi.org/10.1198/1061860043029}
}
\examples{

dat <- besag.endive

# Incidence map.  Figure 2 of Friel and Pettitt
desplot(disease~col*row, dat, col.regions=c('lightgray','black'),
        main="besag.endive")

\dontrun{

# Besag 2000 "An Introduction to Markov Chain Monte Carlo" suggested
# that the autologistic model is not a very good fit for this data.
# We try it anyway.  No idea if this is correct or how to interpret...

require(ngspatial)
A = adjacency.matrix(179,14)
X = cbind(x=dat$col, y=dat$row)
Z = as.numeric(dat$disease=="Y")
m1 <- autologistic(Z ~ 0+X, A=A, control=list(confint="none"))

summary(m1)
## Coefficients:
##      Estimate Lower Upper MCSE
## Xx  -0.007824    NA    NA   NA
## Xy  -0.144800    NA    NA   NA
## eta  0.806200    NA    NA   NA

# Now try an AR1xAR1 model.
dat2 <- transform(dat, xf=factor(col), yf=factor(row),
                  pres=as.numeric(disease=="Y"))
require(asreml)
m2 <- asreml(pres ~ 1, data=dat2, rcov= ~ar1(xf):ar1(yf))
# The 0/1 response is arbitrary, but there is some suggestion
# of auto-correlation in the x (.17) and y (.10) directions,
# suggesting the pattern is more 'patchy' than just random noise,
# but is it meaningful?

require(lucid)
vc(m2)
##     effect component std.error z.ratio constr
## R!variance   0.1301   0.003798    34      pos
##   R!xf.cor   0.1699   0.01942      8.7  uncon
##   R!yf.cor   0.09842  0.02038      4.8  uncon

}

}
\keyword{datasets}