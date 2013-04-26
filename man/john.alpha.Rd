\name{john.alpha}
\alias{john.alpha}
\docType{data}
\title{
  Alpha lattice design of spring oats
}
\description{
  Alpha lattice design of spring oats
}
\usage{data(john.alpha)}
\format{
  A data frame with 72 observations on the following 5 variables.
  \describe{
    \item{\code{plot}}{Plot number}
    \item{\code{rep}}{Replicate factor}
    \item{\code{block}}{Incomplete block factor}
    \item{\code{gen}}{Genotype (variety) factor}
    \item{\code{yield}}{Dry matter yield (tonnes/ha)}
  }
}
\details{
A spring oats trial grown in Craibstone, near Aberdeen.
There were 24 varities in 3 replicates, each consisting
of 6 incomplete blocks of 4 plots.  Planted in a resolvable
alpha design.  The plots were laid out in a single line.
}
\source{
  John JA, Williams ER. (1995)
  \emph{Cyclic and computer generated designs},
  Chapman and Hall, London.  Page 146.
}
\references{
  Piepho, H.P. and M{\"o}hring, J. (2007),
  Computing heritability and selection response from unbalanced
  plant breeding trials, \emph{Genetics}, 177, 1881--1888.
}
\examples{
dat <- john.alpha

# RCB (no incomplete block)
m0 <- lm(yield ~ 0 + gen + rep, data=dat)

# Block fixed (intra-block analysis) (bottom of table 7.4 in John)
m1 <- lm(yield ~ 0 + gen + rep + rep:block, dat)
anova(m1)

# Block random (combined inter-intra block analysis)
require(lme4)
m2 <- lmer(yield ~ 0 + gen + rep + (1|rep:block), dat)
anova(m2)
# summary(m2)@REmat

# Variety means.  John and Williams table 7.5.  Slight, constant
# difference for each method as compared to John and Williams.
means <- data.frame(rcb=coef(m0)[1:24],
                    ib=coef(m1)[1:24],
                    intra=fixef(m2)[1:24])

\dontrun{
# Heritability calculation of Piepho & Mohring, Example 1
require(asreml)
m3 <- asreml(yield ~ 1 + rep, data=dat, random=~ gen + rep:block)
sg2 <- summary(m3)$varcomp[1,'component']
vblup <- predict(m3, classify="gen")$pred$avsed ^ 2
m3 <- asreml(yield ~ 1 + gen + rep, data=dat, random = ~ rep:block)
vblue <- predict(m3, classify="gen")$pred$avsed ^ 2
# H^2 = .803
sg2 / (sg2 + vblue/2)
# H^2c = .809
1-(vblup / 2 / sg2) 
}
}
\keyword{datasets}