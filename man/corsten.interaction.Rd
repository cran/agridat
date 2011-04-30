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
  The labels on the location dendrogram are possibly in the wrong
  order.
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

# Corsten (1990) uses this data to illustrate simultaneous row and column clustering
# based on interaction sums-of-squares.  There is no (known) function in R to
# reproduce this analysis (please contact the package maintainer if this is not true),
# For comparison, the 'heatmap' function clusters the rows and columns independently
# of each other.
heatmap(dmat)
}

