\name{desplot}

\alias{desplot}

\title{Plot the layout/data of a field experiment}

\usage{
  desplot(form=formula(NULL ~ x + y), data, num, col, lab,
  outline1, outline2,
  levels.fill, levels.col, levels.lab, col.regions=RedGrayBlue,
  outline1.gpar=gpar(col = "black", lwd = 3),
  outline2.gpar=gpar(col = "yellow", lwd = 1, lty = 1),
  main, at, ticks=FALSE,
  shorten="abb", show.key=TRUE, key.cex, cex=0.4, strip.cex=0.75, ...)
}

\description{Plot the layout/data of a field experiment}

\details{
  A tool for plotting the layout of an experiment design and also the
  observed data.

  If the response variable is a factor, the cells are filled with
  light-colored backgrounds and a key is placed on the left.  If the
  response variable is numeric, the cells are colored according to a
  palette and a ribbon key is placed on the right.

  The default \code{shorten='abb'} will shorten labels using the
  \code{abbreviate} function.
  Other choices include \code{shorten='sub'} to use a 3-character
  substring, and \code{shorten='no'} for no shortening.

  Note that two sub-plots with identical levels of the split-plot factor
  can be adjacent to each other by virtue of appearing in different
  whole-plots.  To correctly outline the split-plot factor, simply
  concatenate the whole-plot factor and sub-plot factor together.
}

\author{Kevin Wright}

\arguments{\item{form}{A formula like \code{yield~x*y|location}}
\item{data}{A data frame}
\item{num}{The column of the data to use for plotting numbers}
\item{col}{Column of the data for the color of the number}
\item{lab}{Column to use for text labels}
\item{outline1}{Column to use for outlining}
\item{outline2}{Column to use for outlining}
\item{levels.fill}{Text strings to use for fill}
\item{levels.col}{Text strings for color factor}
\item{levels.lab}{Text strings}
\item{col.regions}{Colors for regions}
\item{outline1.gpar}{Graphics parameters for outlining}
\item{outline2.gpar}{Graphics parameters for outlining}
\item{main}{Main title}
\item{at}{Breakpoints for the color ribbon}
\item{ticks}{If TRUE, show tick marks row/column}
\item{shorten}{Method for shortening text in the key}
\item{show.key}{If TRUE, show the key}
\item{cex}{Expansion factor for text/number in each cell}
\item{key.cex}{Left legend cex}
\item{strip.cex}{Strip cex}
\item{...}{Other}
}
\examples{
data("Wheat2", package="nlme")
# Convert lat/long to integers
Wheat2$y <- as.numeric(as.factor(Wheat2$latitude))
Wheat2$x <- as.numeric(as.factor(Wheat2$longitude))
desplot(yield ~ x*y, data=Wheat2, outline1=Block, aspect=2)

desplot(Block ~ x*y, data=Wheat2, num=variety, outline1=Block)

desplot(yield~x+y, yates.oats, outline1=block, outline2=gen)

desplot(gen~x+y, yates.oats, col=block, num=nitro, cex=1,
outline1=block)

desplot(block~x+y, yates.oats, col=nitro, lab=gen, cex=1, outline1=block)
}

