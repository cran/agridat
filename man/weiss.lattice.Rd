\name{weiss.lattice}
\alias{weiss.lattice}
\docType{data}
\title{
  Lattice experiment in soybeans.
}
\description{
  Lattice experiment in soybeans.
}
\usage{data(weiss.lattice)}
\format{
  A data frame with 196 observations on the following 5 variables.
  \describe{
    \item{\code{yield}}{yield (bu/ac)}
    \item{\code{gen}}{genotype factor, 49 levels}
    \item{\code{rep}}{rep factor, 4 levels}
    \item{\code{col}}{column}
    \item{\code{row}}{row}
  }
}
\details{
Yield test of 49 soybean varieties, grown at Ames, IA, in 1938.
Plot dimensions were 3x16 feeet.
The varieties are compared to variety 26 (Mukden).

It is not clear how the reps were positioned in the field.  On the one
hand, the middle three columns of each rep/square are higher yielding,
giving the appearance of the reps being stacked on top of each other.
On the other hand, the analysis by Weiss uses 24 degrees of freedom
4*(7-1) to fit a separate effect for each column in each
rep (instead of across reps).
}
\source{
 Weiss, Martin G. and Cox, Gertrude M. (1939).
 Balanced Incomplete Block and Lattice Square Designs for Testing Yield
 Differences Among Large Numbers of Soybean Varieties.  Table 5.
 \emph{Agricultural Research Bulletins, Nos. 251-259}.
 \url{http://lib.dr.iastate.edu/ag_researchbulletins/24/}
}

\examples{

dat <- weiss.lattice

desplot(yield~col*row|rep,dat, text=gen, aspect=3/16,
        main="weiss.lattice", xlab="Soybean yields", shorten="none", cex=.8)
dat <- transform(dat, xf=factor(col), yf=factor(row))
m1 <- lm(terms(yield ~ rep + rep:xf + rep:yf + gen, keep.order=TRUE), data=dat)
anova(m1) # Matches Weiss table 7
## Response: yield
##           Df  Sum Sq Mean Sq F value    Pr(>F)
## rep        3   91.57  30.525  4.7414 0.0039709 **
## rep:xf    24 2913.43 121.393 18.8557 < 2.2e-16 ***
## rep:yf    24  390.21  16.259  2.5254 0.0007734 ***
## gen       48 1029.87  21.456  3.3327 2.652e-07 ***
## Residuals 96  618.05   6.438

\dontrun{
require(asreml)
m2 <- asreml(yield ~ rep + rep:xf + rep:yf + gen, data=dat)
anova(m2)
predict(m2, classify="gen")$predictions$pvals # Weiss table 6 means
##  gen pred.value std.error  est.stat
##  G01      27.74     1.461 Estimable
##  G02      24.95     1.461 Estimable
##  G03      24.38     1.461 Estimable
##  G04      28.05     1.461 Estimable
##  G05      19.6      1.461 Estimable
##  G06      23.79     1.461 Estimable
}

}
\keyword{datasets}