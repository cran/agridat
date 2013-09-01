\name{hayman.tobacco}
\alias{hayman.tobacco}
\docType{data}
\title{
  Full diallel cross of tobacco
}
\description{
  Full diallel cross of tobacco
}
\usage{data(hayman.tobacco)}
\format{
  A data frame with 128 observations on the following 4 variables.
  \describe{
    \item{\code{block}}{block factor, 2 levels}
    \item{\code{male}}{male inbred factor, 8 levels}
    \item{\code{female}}{female inbred factor}
    \item{\code{flower}}{mean flowering date}
  }
}
\details{
  The experiment used 8 varieties of Nicotiana rustica.  Data is
  recorded for each cross and reciprocal cross.

  Each cross/self was represented by 10 progeny, in two plots of 5
  plants each.  The data are the mean flowering time per plot.
}
\source{
  Hayman, B.I. (1954). The theory and analysis of diallel crosses.
  \emph{Genetics}, 39, 789-809.  Table 3, page 805.
  \url{http://www.genetics.org/content/39/6/789.full.pdf}
}
\examples{

dat <- hayman.tobacco

# Reproduce table 3 and figure 2 of Hayman 1954.

d1 <- subset(dat, block=='B1')
d2 <- subset(dat, block=='B2')

m1 <- acast(d1, male~female, value.var='flower')
m2 <- acast(d2, male~female, value.var='flower')

mn1 <- (m1+t(m1))/2
mn2 <- (m2+t(m2))/2

# Variance and covariance of 'rth' offspring
vr1 <- apply(mn1, 1, var)
vr2 <- apply(mn2, 1, var)
wr1 <- apply(mn1, 1, cov, diag(mn1))
wr2 <- apply(mn2, 1, cov, diag(mn2))

summ <- data.frame(rbind(mn1,mn2))
summ$block <- rep(c('B1','B2'), each=8)
summ$vr <- c(vr1,vr2)
summ$wr <- c(wr1,wr2)
summ$male <- rep(1:8,2) # Vr and Wr match Hayman table 3

with(summ, plot(wr~vr, type='n'))
with(summ, text(vr, wr, male)) # Match Hayman figure 2
abline(0,1,col="gray")
# Hayman notes that 1 and 3 do not lie along the line, so modifies them
# and re-analyzes.

}
\keyword{datasets}