\name{crossa.wheat}
\alias{crossa.wheat}
\docType{data}
\title{
  Wheat yields for 18 genotypes at 25 locations
}
\description{
  Wheat yields for 18 genotypes at 25 locations
}

\format{
  A data frame with 450 observations on the following 3 variables.
  \describe{
    \item{\code{loc}}{location factor}
    \item{\code{gen}}{genotype factor}
    \item{\code{yield}}{grain yield, tons/ha}
  }
}
\details{
  Grain yield from the 8th Elite Selection Wheat Yield Trial to evaluate
  18 bread wheat genotypes at 25 locations in 15 countries.

  Locations
  \tabular{lllrr}{
    Code \tab Country \tab Location \tab Latitude (N) \tab Elevation (m) \cr
    AK \tab Algeria \tab El Khroub \tab 36 \tab 640 \cr
    AL \tab Algeria \tab Setif \tab 36 \tab 1,023 \cr
    BJ \tab Bangladesh \tab Joydebpur \tab 24 \tab 8 \cr
    CA \tab Cyprus \tab Athalassa \tab 35 \tab 142 \cr
    EG \tab Egypt \tab E1 Gemmeiza \tab 31 \tab 8 \cr
    ES \tab Egypt \tab Sakha \tab 31 \tab 6 \cr
    EB \tab Egypt \tab Beni-Suef \tab 29 \tab 28 \cr
    IL \tab India \tab Ludhiana \tab 31 \tab 247 \cr
    ID \tab India \tab Delhi \tab 29 \tab 228 \cr
    JM \tab Jordan \tab Madaba \tab 36 \tab 785 \cr
    KN \tab Kenya \tab Njoro \tab 0 \tab 2,165 \cr
    MG \tab Mexico \tab Guanajuato \tab 21 \tab 1,765 \cr
    MS \tab Mexico \tab Sonora \tab 27 \tab 38 \cr
    MM \tab Mexico \tab Michoacfin \tab 20 \tab 1,517 \cr
    NB \tab Nepal \tab Bhairahwa \tab 27 \tab 105 \cr
    PI \tab Pakistan \tab Islamabad \tab 34 \tab 683 \cr
    PA \tab Pakistan \tab Ayub \tab 32 \tab 213 \cr
    SR \tab Saudi Arabia \tab Riyadh \tab 24 \tab 600 \cr
    SG \tab Sudan \tab Gezira \tab 14 \tab 411 \cr
    SE \tab Spain \tab Encinar \tab 38 \tab 20 \cr
    SJ \tab Spain \tab Jerez \tab 37 \tab 180 \cr
    SC \tab Spain \tab Cordoba \tab 38 \tab 110 \cr
    SS \tab Spain \tab Sevilla \tab 38 \tab 20 \cr
    TB \tab Tunisia \tab Beja \tab 37 \tab 150 \cr
    TC \tab Thailand \tab Chiang Mai \tab 18 820 \cr
  }

}
\source{
  Crossa, J and Fox, PN and Pfeiffer, WH and Rajaram, S and Gauch Jr,
  HG. (1991). AMMI adjustment for statistical analysis of an
  international wheat yield trial.
  \emph{Theoretical and Applied Genetics}, 81, 27--37.

  Used with permission of Jose' Crossa.
}
\references{
  Jean-Louis Laffont, Kevin Wright and Mohamed Hanafi. 2013.
  Genotype + Genotype x Block of Environments (GGB) Biplots.
  \emph{Crop Science}. In press.
}
\examples{

dat <- crossa.wheat

# AMMI biplot.  Fig 3 of Crossa at al
library(agricolae)
m1 <- with(dat, AMMI(E=loc, G=gen, R=1, Y=yield, graph=""))
b1 <- m1$biplot[,1:4]
b1$PC1 <- -1 * b1$PC1 # Flip vertical
plot(b1$yield, b1$PC1, cex=0.0,
     text(b1$yield, b1$PC1, cex=.5, labels=row.names(b1),col="brown"),
     main="AMMI BIPLOT", xlab="Average yield", ylab="PC1", frame=TRUE)
mn <- mean(b1$yield)
abline(h=0, v=mn, col='wheat')

g1 <- subset(b1,type=="GEN")
text(g1$yield, g1$PC1, rownames(g1), col="darkgreen", cex=.5)

e1 <- subset(b1,type=="ENV")
arrows(mn, 0,
       0.95*(e1$yield - mn) + mn, 0.95*e1$PC1,
       col= "brown", lwd=1.8,length=0.1)

}
\keyword{datasets}