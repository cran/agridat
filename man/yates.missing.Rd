\name{yates.missing}
\alias{yates.missing}
\docType{data}
\title{
  Factorial experiment with missing values
}
\description{
  Potato factorial experiment with missing values
}
\usage{yates.missing}
\format{
  A data frame with 80 observations on the following 3 variables.
  \describe{
    \item{\code{trt}}{Treatment factor with levels \code{0} \code{K}
      \code{N} \code{P} \code{NK} \code{KP} \code{NP} \code{NKP} }
    \item{\code{block}}{Block factor, 10 levels}
    \item{\code{y}}{Numeric, infection intensity}
  }
}
\details{
  The response variable \code{y} is 'intensity of infection of potato
  tubers innoculated with \emph{Phytophthora Erythroseptica}'.
  
  Yates (1933) presents an iterative algorithm to estimate missing
  values in a matrix, using this data as an example.
}
\source{
  F. Yates, 1933. The analysis of replicated experiments when the field
  results are incomplete. \emph{Emp. J. Exp. Agric.}, 1, 129--142.
}
\references{
  Steel & Torrie, 1980, Principles and Procedures of Statistics,
  2nd Edition, page 212.
}
\examples{
require(reshape2)
dat <- yates.missing
mat0 <- acast(dat[, c('trt','block','y')], trt~block,
               id.var=c('trt','block'))

# Use lm to estimate missing values.  The estimated missing values
# are the same as in Yates (1933)
m1 <- lm(y~trt+block, dat)
dat$pred <- predict(m1, new=dat[, c('trt','block')])
dat$filled <- ifelse(is.na(dat$y), dat$pred, dat$y)
mat1 <- acast(dat[, c('trt','block','pred')], trt~block,
               id.var=c('trt','block'))

\dontrun{
# Another method to estimate missing values via PCA
require(pcaMethods)
m2 <- pca(mat0, method="nipals", center=FALSE, nPcs=3)
mat2 <- m2@scores %*% t(m2@loadings)

# Compare
ord <- c("O","N","K","P","NK","NP","KP","NKP")
print(mat0[ord,], na.print=".")
round(mat1[ord,] ,2)
round(mat2[ord,] ,2)

# SVD with 3 components recovers original data better
sum((mat0-mat1)^2, na.rm=TRUE)
sum((mat0-mat2)^2, na.rm=TRUE) # Smaller SS => better fit
}
}
