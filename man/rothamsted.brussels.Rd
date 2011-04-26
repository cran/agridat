\name{rothamsted.brussels}
\alias{rothamsted.brussels}
\docType{data}
\title{
  Yield of brussels sprouts, RCB with 9 fertilizer treatments
}
\description{
  Yield of brussels sprouts, RCB with 9 fertilizer treatments
}
\usage{rothamsted.brussels}
\format{
  A data frame with 48 observations on the following 5 variables.
  \describe{
    \item{\code{row}}{Row}
    \item{\code{col}}{Column}
    \item{\code{yield}}{Yield of saleable sprouts, pounds}
    \item{\code{trt}}{Treatment factor, 9 levels}
    \item{\code{block}}{Block factor, 4 levels}
  }
}
\details{
  The block numbers are arbitrary, and may not match the orignal source.
}
\source{
  Batchelor, LD and Reed, HS. 1918.
  Relation of the variability of yields of fruit trees to the accuracy
  of field trials. \emph{J. Agric. Res}, 12, 245--283.
  \url{http://books.google.com/books?id=Lil6AAAAMAAJ&lr&pg=PA245}.
}
\references{
  McCullagh, P. and Clifford, D., (2006).
  Evidence for conformal invariance of crop yields,
  \emph{Proceedings of the Royal Society A: Mathematical, Physical and
  Engineering Science}, 462, 2119--2143.
}
\examples{
dat <- rothamsted.brussels

bwplot(yield~trt, dat)
desplot(yield~col*row, data=dat, num=trt, outline1=block, cex=1)

}

