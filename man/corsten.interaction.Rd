\name{corsten.interaction}
\alias{corsten.interaction}
\docType{data}
\title{Corn yield in multi-environment trial}
\description{
  The data is the average yield (kg/acre) of 20 genotypes of corn at 7
  locations.
}
\usage{corsten.interaction}
\format{
  A data frame with 140 observations on the following 3 variables.
  \describe{
    \item{\code{gen}}{Genotype factor with 20 levels }
    \item{\code{loc}}{Location factor with 7 levels }
    \item{\code{yield}}{yield in kg/acre}
  }
}
\details{ 
  The data is used by Corsten & Denis (1990) to illustrate two-way
  clustering by minimizing the interaction sum of squares.
  Note that one of the dendrograms in the paper might be wrong.
}
\source{
  L C A Corsten and J B Denis, Structuring Interaction in Two-Way Tables
  By Clustering, \emph{Biometrics}, 1990, 46, 207--215. Table 1.

  Used with permission.
}

\examples{

dat <- corsten.interaction

require(reshape2)
m1 <- melt(dat, measure.var='yield')
dmat <- acast(m1, loc~gen)

# Compare figure 1 of Corsten 1990 (using clustering of _interactions_)
# with the simple one-way clusterings from 'heatmap'
heatmap(dmat)
}

