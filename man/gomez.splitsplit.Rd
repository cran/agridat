\name{gomez.splitsplit}
\alias{gomez.splitsplit}
\docType{data}
\title{
Rice split-split plot experiment
}

\description{
  Grain yield of three varieties of rice grown in a split-split plot
  arrangement with 3 reps, nitrogen level as the main plot, management
  practice as the sub-plot, and rice variety as the sub-sub plot.
}

\usage{gomez.splitsplit}

\format{
  A data frame with 135 observations on the following 7 variables.
  \describe{
    \item{\code{rep}}{Replicate factor, 3 levels}
    \item{\code{nitro}}{Nitrogen fertilizer, in kilograms/hectare}
    \item{\code{management}}{Plot management}
    \item{\code{gen}}{Variety of rice}
    \item{\code{yield}}{Yield of rice}
    \item{\code{x}}{Column position in the field}
    \item{\code{y}}{Row position in the field}
  }
}

\source{
  Gomez, K.A. and Gomez, A.A.. 1984,
  \emph{Statistical Procedures for Agricultural Research}.
  Wiley-Interscience. Page 143.

  Used with permission of Kwanchai Gomez.
}

\examples{
dat <- gomez.splitsplit
dat$nf <- factor(dat$nitro)

desplot(yield~x*y, dat)
desplot(nf~x*y, dat, outline1=rep, col=management, num=gen, cex=1)

if(require(HH)){
position(dat$nf) <- c(0,50,80,110,140)
interaction2wt(yield~rep+nf+management+gen, data=dat,
               relation=list(x="free", y="same"),
               rot=c(90,0), xlab="",
               par.strip.text.input=list(cex=.7))
}

# AOV.  Gomez page 144-153
m0 <- aov(yield~ nf * management * gen + Error(rep/nf/management),
         data=dat)
summary(m0) # Compare to Gomez, p. 153.

}
