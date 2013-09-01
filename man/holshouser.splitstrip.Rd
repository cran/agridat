\name{holshouser.splitstrip}
\alias{holshouser.splitstrip}
\docType{data}
\title{
  Split strip plot on soybeans
}
\description{
  Split strip plot on soybeans
}

\format{
  A data frame with 160 observations on the following 8 variables.
  \describe{
    \item{\code{block}}{block factor, 4 levels}
    \item{\code{plot}}{plot number}
    \item{\code{cultivar}}{cultivar factor, 4 levels}
    \item{\code{spacing}}{row spacing, numeric}
    \item{\code{pop}}{population, numeric (thousand per acre)}
    \item{\code{yield}}{yield, a numeric vector}
    \item{\code{row}}{row number}
    \item{\code{col}}{column number}
  }
}
\details{
  Within each block, cultivars were whole plots.  Withing whole plots,
  spacing was applied in strips vertically, and population was applied
  in strips horizontally.
}

\source{
  Schabenberger, Oliver and Francis J. Pierce. 2002.
  \emph{Contemporary Statistical Models for the Plant and Soil
    Sciences}. CRC Press, Boca Raton, FL.  Page 493.

  Used with permission of David Holshouser at Virginia Polytechnic.
}

\examples{

dat <- holshouser.splitstrip
dat$spacing <- factor(dat$spacing)
dat$pop <- factor(dat$pop)

# Experiment layout and field trends
desplot(spacing ~ col*row, data=dat, out1=block, out2=cultivar,
        col=cultivar, text=pop, cex=.8, shorten='none', col.regions=c('wheat','white'),
        main="holshouser.splitstrip experiment design")
desplot(yield ~ col*row, data=dat, out1=block,
        main="holshouser.splitstrip")

# Overall main effects and interactions
require(HH)
interaction2wt(yield~cultivar*spacing*pop, dat)

## Schabenberger's SAS model, page 497
## proc mixed data=splitstripplot;
##   class block cultivar pop spacing;
##   model yield = cultivar spacing spacing*cultivar pop pop*cultivar
##                 spacing*pop spacing*pop*cultivar / ddfm=satterth;
##   random block block*cultivar block*cultivar*spacing block*cultivar*pop;
## run;

## Now lme4. This design has five error terms--four are explicitly given.
require(lme4)
m1 <- lmer(yield ~ cultivar * spacing * pop +
           (1|block) + (1|block:cultivar) + (1|block:cultivar:spacing) +
           (1|block:cultivar:pop), data=dat)
## Variances match Schabenberger, page 498.
print(summary(m1)@REmat, quote=FALSE)
## Groups                 Name        Variance Std.Dev.
## block:cultivar:pop     (Intercept) 2.42143  1.55609
## block:cultivar:spacing (Intercept) 1.24437  1.11551
## block:cultivar         (Intercept) 0.45232  0.67255
## block                  (Intercept) 3.03666  1.74260
## Residual                           3.92755  1.98180

}
\keyword{datasets}
