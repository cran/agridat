\name{mercer.wheat.uniformity}
\alias{mercer.wheat.uniformity}
\docType{data}
\title{
  Mercer and Hall's uniformity trial of wheat in 1910
}
\description{
  Uniformity trial of wheat in 1910
}

\format{
  A data frame with 500 observations on the following 4 variables.
  \describe{
    \item{\code{row}}{Row}
    \item{\code{col}}{Column}
    \item{\code{grain}}{Grain yield, pounds}
    \item{\code{straw}}{Straw yield, pounds}
  }
}
\details{  
The wheat crop was grown in 1910 at Rothamsted Experiment Station
(Harpenden, Herts, England).  In one field, a seemingly uniform area of
1 acre was harvested in separate plots, each 1/500th acre in size.  The
grain and straw from each plot was weighed separately.  Row 1 is the
south edge, row 20 is the north.  Column 1 is the west edge.  The
dimensions of each plot were approximately 3.2 meters by 2.5 meters.
}
\source{
  Mercer, WB and Hall, AD, 1911. The experimental error of field trials
  \emph{The Journal of Agricultural Science}, 4, 107-132. Table 5.
}
\references{
The 'spdep' package includes the grain yields
(only) and spatial positions of plot centres in its example dataset 'wheat'.
}
\examples{
dat <- mercer.wheat.uniformity

require('lattice')
desplot(grain ~ col*row, dat, main="mercer.wheat.uniformity")

densityplot(~grain, data=dat)

xyplot(straw ~ grain, data=dat, type=c('p','r'), main="mercer.wheat.uniformity - regression")

# require(hexbin)
# hexbinplot(straw ~ grain, data=dat)

require(sp)
require(gstat)
plot.wid <- 2.5
plot.len <- 3.2
nr <- length(unique(dat$row))
nc <- length(unique(dat$col))

xy <- expand.grid(x = seq(plot.wid/2, by=plot.wid, length=nc),
y = seq(plot.len/2, by=plot.len, length=nr))
dat.sp <- dat
coordinates(dat.sp) <- xy

# heatmap
spplot(dat.sp, zcol = "grain", cuts=8,
       cex = 1.6,
       col.regions =  bpy.colors(8),
       main = "Grain yield", key.space = "right")

# variogram
vg <- variogram(grain ~ 1, dat.sp, cutoff = plot.wid * 10, width = plot.wid)
plot(vg, plot.numbers = TRUE)
}
