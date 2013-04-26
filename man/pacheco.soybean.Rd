\name{pacheco.soybean}
\alias{pacheco.soybean}
\docType{data}
\title{
  Soybean yields in Brazil.
}
\description{
  Yields of 18 soybean genotypes at 11 environments in Brazil.
}
\usage{data(pacheco.soybean)}
\format{
  \describe{ 
    \item{\code{gen}}{Genotype factor, 18 levels}
    \item{\code{env}}{Environment factor, 11 levels}
    \item{\code{yield}}{Yield, kg/ha}
  }
}
\details{
  In each environment was used an RCB design with 3 reps.  The means of
  the reps are shown here.
}
\source{
  R M Pacheco, J B Duarte, R Vencovsky, J B Pinheiro, A B Oliveira,
  (2005). Use of supplementary genotypes in AMMI analysis.
  \emph{Theor Appl Genet}, 110, 812-818.

  Used with permission of Robert Pacheco.
}
\examples{
data(pacheco.soybean)

dat <- pacheco.soybean

# AMMI biplot similar to Fig 2 of Pacheco et al.
require(agricolae)
m1 <- with(dat, AMMI(env, gen, REP=1, yield))
bip <- m1$biplot[,1:3]
# Fig 1 of Pacheco et al.
with(bip, plot(yield, PC1, cex=0.0,
               text(yield,PC1,labels=row.names(bip), col="blue"),
               xlim=c(1000,3000),main="AMMI BIPLOT",frame=TRUE))
}
\keyword{datasets}