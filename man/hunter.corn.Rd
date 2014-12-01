\name{hunter.corn}
\alias{hunter.corn}
\docType{data}
\title{
  Corn yield response to nitrogen
}
\description{
  Corn yield response to nitrogen
}

\format{
  A data frame with 54 observations on the following 4 variables.
  \describe{
    \item{\code{nitro}}{nitrogen fertilizer, pound/acre}
    \item{\code{year}}{year}
    \item{\code{loc}}{location factor}
    \item{\code{yield}}{yield, bu/ac}
  }
}
\details{
  Experiments were conducted in eastern Oregon during the years
  1950-1952.

  Planting rates varied from 15,000 to 21,000 planter per acre.
}
\source{
  Albert S. Hunter, John A. Yungen (1955).  The Influence of
  Variations in Fertility Levels Upon the Yield and Protein
  Content of Field Corn in Eastern Oregon.
  \emph{Soil Science Society of America Journal}, 19, 214-218.
}
\references{
  James Leo Paschal, Burton Leroy French (1956).
  A method of economic analysis applied to nitrogen fertilizer rate
  experiments on irrigated corn.
  Tech Bull 1141. United States Dept of Agriculture.
  \url{books.google.com/books?id=gAdZtsEziCcC&pg=PP1}
}
\examples{

dat <- hunter.corn

dat <- transform(dat, env=factor(paste(loc,year)))

require(lattice)
xyplot(yield~nitro|env, dat, type='b',
main="hunter.corn - nitrogen response curves")

}
\keyword{datasets}