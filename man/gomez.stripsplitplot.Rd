\name{gomez.stripsplitplot}
\alias{gomez.stripsplitplot}
\docType{data}
\title{
Rice strip-split-plot experiment
}
\description{
  A strip-split-plot experiment with three reps, variety as the horizontal
  strip, nitrogen fertilizer as the vertical strip, and planting method
  as the subplot factor.
}
\usage{gomez.stripsplitplot}
\format{
  \describe{
    \item{\code{yield}}{Grain yield in kg/ha}
    \item{\code{planting}}{Planting factor, P1=broadcast, P2=transplanted}
    \item{\code{rep}}{Rep factor, 3 levels}
    \item{\code{nitro}}{Nitrogen fertilizer in kg/ha}
    \item{\code{gen}}{Genotype factor, G1 to G6}
    \item{\code{x}}{Ordinate in the field}
    \item{\code{y}}{Ordinate in the field}
  }
}
\source{
  Gomez, K.A. and Gomez, A.A.. 1984,
  \emph{Statistical Procedures for Agricultural Research}.
  Wiley-Interscience. Page 155.
  
  Used with permission of Kwanchai Gomez.
}
\examples{
\dontrun{
dat <- gomez.stripsplitplot

# Gomez table 4.19, ANOVA of strip-split-plot design
dat <- transform(dat, nf=factor(nitro))
m1 <- aov(yield ~ nf * gen * planting +
          Error(rep + rep:nf + rep:gen + rep:nf:gen), data=dat)
summary(m1)

# High nitro strips (columns 2 4 7) are easy to see
levelplot(yield~x+y, dat)

# There is a noticeable linear trend along the y coordinate which may be
# an artifact that blocking will remove, or may need to be modeled.
# Note the outside values in the high-nitro boxplot.
require(HH)
interaction2wt(yield ~ nitro + gen + planting + y, dat, x.relation="free")
}
}

