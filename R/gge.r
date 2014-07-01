# gge.r
# Time-stamp: c:/x/rpack/agridat/R/gge.r

# TODO: Add biplot example to denis.ryegrass

if(0){
  require("reshape2")
  # matrix data
  mat1 <- matrix(c(50, 55, 65, 50, 60, 65, 75,
                   67, 71, 76, 80, 82, 89, 95,
                   90, 93, 95, 102, 97, 106, 117,
                   98, 102, 105, 130, 135, 137, 133,
                   120, 129, 134, 138, 151, 153, 155),
                 ncol=5, byrow=FALSE)
  colnames(mat1) <- c("E1","E2","E3","E4","E5")
  rownames(mat1) <- c("G1","G2","G3","G4","G5","G6","G7")

  m11 = gge(mat1)
  plot(m11)
  biplot(m11) # default title is 'm11'
  biplot(m11, title="Example biplot")
  biplot(m11, title="Example biplot", cex.gen=2)
  biplot(m11, title="Example biplot", cex.env=2)
  biplot(m11, title="Example biplot", col.gen="blue")
  biplot(m11, title="Example biplot", col.gen=c("blue","red")) # not working
  biplot(m11, title="Example biplot", comps=2:3)
  #biplot(m11, title="Example biplot", alpha=1)
  #biplot(m11, title="Example biplot", lab.gen=FALSE) # not supported
  #biplot(m11, title="Example biplot", col.gen=1:2, lab.gen=0, cex.gen=2)
  biplot(m11, title="Example biplot", flip="") # no flipping
  biplot(m11, title="Example biplot", flip=c(TRUE,FALSE))
  biplot(m11, title="Example biplot", flip=c(FALSE,TRUE))
  biplot(m11, title="Example biplot", flip=c(FALSE,FALSE))
  biplot(m11, title="Example biplot", flip=c(TRUE,TRUE))
  biplot(m11, title="Example biplot", flip=TRUE)

  # One missing value in a matrix
  mat2 <- mat1 ; mat2[1,1] <- NA
  m2 <- gge(mat2) # should switch to 'nipals'
  plot(m2)
  biplot(m2)
  m3 <- gge(mat2, method="svdImpute") # svdImpute
  biplot(m3)

  bar <- transform(lattice::barley, env=paste0(site,year))
  m21 <- gge(yield~variety*site, bar, env.group=year) # errs, as it should
  m22 <- gge(yield~variety*env, bar)
  biplot(m22)
  biplot(m22, lab.env=FALSE) # label locs

  # Need an option to disable residual vectors

  # env.group with data.frame
  m23 <- gge(yield~variety*env, bar, env.group=year)
  plot(m23)
  biplot(m23)
  biplot(m23, lab.env=TRUE) # default is to label locs
  biplot(m23, lab.env=FALSE) # label locs

  # Scaling options
  m31 <- gge(yield ~ variety * env, data=bar, scale="none")
  biplot(m31)
  m32 <- gge(yield ~ variety * env, data=bar, scale=FALSE)
  biplot(m32)
  m33 <- gge(yield ~ variety * env, data=bar, scale=TRUE)
  biplot(m33)
  m34 <- gge(yield ~ variety * env, data=bar, scale='uv')
  biplot(m34)
  m35 <- gge(yield ~ variety * env, data=bar, scale='vector')
  biplot(m35)
  m36 <- gge(yield ~ variety * env, data=bar, scale='pareto')
  biplot(m36)
  m37 <- gge(yield ~ variety * env, data=bar,
             scale=runif(12, 1, 2)) # fails due to bug in pcaMethods
  biplot(m37)


  # Method options: "svd","nipals","bpca", "ppca", "svdImpute"
  m41 <- gge(yield ~ variety * env, data=bar, scale="uv", method="svd")
  biplot(m41)
  m42 <- gge(yield ~ variety * env, data=bar, scale="uv", method="nipals")
  biplot(m42)
  #m43 <- gge(yield ~ variety * env, data=bar, scale="uv", method="bpca")
  #biplot(m43) # FIXME.  Seems to be a problem with bpca
  m44 <- gge(yield ~ variety * env, data=bar, scale="uv", method="ppca")
  biplot(m44)
  m45 <- gge(yield ~ variety * env, data=bar, scale="uv", method="svdImpute")
  biplot(m45)


#  biplot(m36, cex.gen=1, col.gen=c("black", "orange","purple")) # black since no gen.group


# Example 7.  Custom colors for gen/env

  # Example matrix data from Laffont
  mat3 <- read.csv("c:/x/rpack/ggb/example1.csv")
  rownames(mat3) <- mat3[,1]
  mat3 <- as.matrix(mat3[, -1])
  # specify 'gen.group' and 'env.group' as a vector with matrix data
  m31 <- gge(mat3, scale="none",
             env.group=c(rep("Blk1",3), rep("Blk2",5),rep("Blk3", 5), rep("Blk4", 7)),
             gen.group=rep(letters[1:3], each=5))
  biplot(m31, flip=c(1,1))






  lib(agridat)
  dat1 <- crossa.wheat
  mat1 <- acast(dat1, gen~loc)
  mat1 <- mat1[, c("SR","SG","CA","AK","TB","SE","ES","EB","EG",
                   "KN","NB","PA","BJ","IL","TC","JM","PI","AS","ID",
                   "SC","SS","SJ","MS","MG","MM")]
  tit1 <- "CYMMIT wheat"
  m3 <- gge(mat1, env.group=c(rep("Grp2",9), rep("Grp1", 16)), lab="Y",
            scale=FALSE, title=tit1)
  biplot(m3, lab.env=TRUE, cex.gen=1, cex.env=1)
  plot(m3)

  # Specify env.group as column in data frame
  dat2 <- crossa.wheat
  dat2$eg <- ifelse(dat2$loc %in% c("KN","NB","PA","BJ","IL","TC","JM","PI","AS","ID",
                                   "SC","SS","SJ","MS","MG","MM"), "Grp1", "Grp2")
  m4 <- gge(yield~gen*loc, dat2, env.group=eg, scale=FALSE)
  biplot(m4, lab.env=TRUE)

  # No env.group
  m5 <- gge(yield~gen*loc, dat2, scale=FALSE)
  biplot(m5, lab.env=TRUE, title="Test")

  # Nipals method, no missing data
  m6 <- gge(yield~gen*loc, dat2, scale=FALSE, method="nipals")
  biplot(m6, lab.env=TRUE, title="Test")



}

# ----------------------------------------------------------------------------

gge <- function(x, ...) UseMethod("gge")

gge.formula <- function(formula, data=NULL,
                        gen.group=NULL, env.group=NULL, ...) {
  # Author: Kevin Wright

  if(is.null(data))
    stop("This usage of gge requires a formula AND data frame.")

  # Get character representations of all necessary variables.
  # There is probably a more R-like (obscure) way to do this, but this works.
  vars <- all.vars(formula)
  # Check for valid names (in the data)
  if(!all(is.element(vars,names(data))))
    stop("Some of the terms in the formula are not found in the data.")
  .y <- vars[1]
  .gen <- vars[2] # Note that 'gen' may already a variable in the data
  .env <- vars[3]

  # Make gen.group & env.group either NULL or quoted name in the data
  gen.group <- substitute(gen.group)
  env.group <- substitute(env.group)
  if(!is.null(gen.group)) {
    gen.group <- deparse(gen.group) # convert to text
    if(!is.element(gen.group, names(data)))
      stop("The argument 'gen.group' refers to non-existant column of data.")

    if(any(colSums(table(data[[gen.group]], data[[.gen]])>0)>1)){
      warning("Some values of '", .gen, "' have multiple gen.group.  Ignoring gen.group.")
      gen.group <- NULL
    }
  }
  if(!is.null(env.group)) {
    env.group <- deparse(env.group)
    if(!is.element(env.group, names(data)))
      stop("The argument 'env.group' refers to non-existant column of data.")

    if(any(colSums(table(data[[env.group]], data[[.env]])>0)>1)){
      warning("Some values of '", .env, "' have multiple env.group.  Ignoring env.group.")
      env.group <- NULL
    }
  }

  # Finally, reshape data into a matrix, average values in each cell
  # require(reshape2) # Now in 'Depends'
  datm <- acast(data, formula(paste(.gen, "~", .env)), fun.aggregate=mean, value.var=.y)
  datm[is.nan(datm)] <- NA # Use NA instead of NaN

  # Make gen.group and env.group to be vectors corresponding to rows/cols of datm
  if(!is.null(gen.group)) {
    ix1 <- match(rownames(datm), data[[.gen]])
    gen.group <- data[[gen.group]][ix1]
  }
  if(!is.null(env.group)) {
    ix2 <- match(colnames(datm), data[[.env]])
    env.group <- data[[env.group]][ix2]
  }

  # Now call the matrix method and return the results
  invisible(gge.matrix(datm, gen.group=gen.group, env.group=env.group, ...))

}

gge.matrix <- function(x,
                       center=TRUE,
                       scale=TRUE,
                       gen.group=NULL,
                       env.group = NULL,
                       comps=c(1,2),
                       method="svd",
                       ...) {
  # Author: Kevin Wright, based on S-Plus code by Jean-Louis Laffont.

  # x: matrix of rows=genotypes, cols=environments
  # env.group: vector having the group class for each loc

  if(!is.null(env.group) && (length(env.group) != ncol(x)))
     stop("'env.group' is the wrong length.")
  if(!is.null(gen.group) && (length(gen.group) != nrow(x)))
     stop("'gen.group' is the wrong length.")

  x.orig <- x

  # Check for missing values
  pctMiss <- sum(is.na(x))/(nrow(x)*ncol(x))
  if(pctMiss > 0)
    cat("Missing values detected: (", round(100*pctMiss,0), "%)\n", sep="")
  if(pctMiss > 0 & method=="svd") {
    cat("Switching to 'nipals' method.\n")
    method <- "nipals"
  }
  if(pctMiss > .10)
    warning("Biplots deteriorate for more than 10-15% missing values.")
  genPct <- apply(x, 1, function(xx) length(na.omit(xx)))/ncol(x)
  envPct <- apply(x, 2, function(xx) length(na.omit(xx)))/nrow(x)
  if(any(genPct<.2) || any(envPct<.2))
    warning("Missing data may be structured.")

  # Maximum number of PCs
  #maxPCs <- min(nrow(x), ncol(x)-1)

  # SVD / PCA of x
  if(!require(pcaMethods)){ # use base::svd
    if(method!="svd")
      stop("Only method='svd' available.  Install 'pcaMethods' package for more options.")
    if(!is.logical(center) | !is.logical(scale))
      stop("With svd from base R, center/scale can only be TRUE/FALSE")

    x.pca <- NULL
    x <- scale(x, center=center, scale=scale)  # Center / scale each environment
    x.svd <- svd(x) # Singular Value Decomposition of x
    R2 <- x.svd$d^2/sum(x.svd$d^2)

  } else { # use 'pcaMethods'
    # svd is the same sas base R svd
    # nipals can handle small amount of missing values
    # bpca does NOT require orthogonal PCs
    # ppca allows NA
    # svdImpute allows NA

    if(!is.element(method, c("svd","nipals","bpca", "ppca", "svdImpute")))
      stop("Unsupported method.  Try 'svd' or 'svdImpute'")
    if(is.logical(scale) && scale) {
      cat("Changing scale from TRUE to 'uv' for pcaMethods.\n")
      scale <- "uv"
    }
    if(is.logical(scale) && !scale) {
      cat("Changing scale from FALSE to 'none' for pcaMethods.\n")
      scale <- "none"
    }

    x.svd <- NULL
    x.pca <- pca(x, nPcs=min(nrow(x), ncol(x)-1), completeObs=TRUE,
                 center=center, scale=scale, verbose=TRUE, method=method)
    x <- x.pca@completeObs # missing values are replaced with estimates
    x <- prep(x, center=center, scale=scale)
    R2 <- x.pca@R2
  }

	if(!is.null(x.svd) && length(x.svd$d) == 1)
		stop("Only one principal component.  Biplot not available.")

  # The matrices G,W,R of Laffont et al are here x.g, x.gb, x.r
  # x.g is a matrix of identical columns of genotype means

  genMeans <- rowMeans(x, na.rm=TRUE)
  x.g <-  genMeans %*% t(rep(1, ncol(x)))

  # x.gb is the same size as x, but has the G*B effect
  # First remove gen effects, then average by group, then expand back to size of x
  x.cc <- x - x.g # x.cc = x.orig - envmeans - genmeans
  x.grp <- NULL

  if(is.null(env.group)){
    x.gb <- x.cc # No groups (each loc is its own group)
  } else {
    groupNames <- names(table(env.group))
    for(i in groupNames) {
      # Need 'drop' so that a single-column is not converted to vector
      x.grp <- cbind(x.grp, rowMeans(x.cc[, env.group==i, drop=FALSE]))
    }
    colnames(x.grp) <- groupNames
    x.gb <- x.grp[,match(env.group, colnames(x.grp))]
  }
  # x.r is a matrix of residuals = x.orig - colmeans - rowmeans - G*B
  x.r <- x - x.g - x.gb

  # Numerical measures for mosaic plot
  # TSS <- sum(x^2, na.rm=TRUE) # Total sum of squares
  # SSG <- sum(x.g^ 2)          # Sum of squares due to genotype
  # SSGB <- sum(x.gb^2)         # Sum of squares due GxB
  # SSR <- sum(x.r^2)           # Residual SS

  # Orthogonal rotation matrix U
  if(!is.null(x.svd))
    U <- x.svd$u
  else {
    eval <- apply(x.pca@scores^2, 2, sum) # eigen values
    U <- x.pca@scores %*% diag(1/sqrt(eval))
  }

  # Partition SSG, SSGB, SSR along each axis
  # Ex: SSGk = diag(u'x.g * x.g'u)
  #          = diag(crossprod(crossprod(x.g, u)))
  #          = colSums(crossprod(x.g, u)^2)
  SSGk <- colSums(crossprod(x.g, U)^2)
  SSGBk <- colSums(crossprod(x.gb, U)^2)
  SSRk <- colSums(crossprod(x.r, U)^2)

  # Data for mosaic plot
  mosdat <- data.frame(G = SSGk, GB = SSGBk, R = SSRk)
  mosdat <- as.matrix(mosdat)
  rownames(mosdat) <- 1:nrow(mosdat)
  names(dimnames(mosdat)) <- c("PC","")

  # Calculate coordinates (along all kept components) for genotypes
  maxcomp <- 5
  maxcomp <- min(maxcomp, nrow(x), ncol(x)-1)
  ROT <- U[ , 1:maxcomp]
  n.gen <- nrow(x)
  genCoord <- ROT * sqrt(n.gen - 1)

  # Block coordinates
  blockCoord <- t(x.g + x.gb) %*% ROT / sqrt(n.gen - 1)

  # Loc coordinates = Block + Residual
  resCoord <- t(x.r) %*% ROT * (1/sqrt(n.gen - 1))
  locCoord <- blockCoord + resCoord

  # completeObs matrix lacks rownames ?
  rownames(genCoord) <- rownames(x.orig)
  rownames(locCoord) <- colnames(x.orig)
  rownames(blockCoord) <- env.group

  ret <- list(x=x,
              genCoord=genCoord, locCoord=locCoord, blockCoord=blockCoord,
              gen.group=gen.group, env.group=env.group,
              genMeans=genMeans, mosdat=mosdat, R2=R2,
              center=center, scale=scale, method=method,
              pctMiss=pctMiss)
  class(ret) <- "gge"

  return(ret)
}

extend <- function(x,y,xlim,ylim){
  # Given a vector of points (x,y) this function extends the points outward
  # along a vector from (0,0) to the border of the box defined by (xlim,ylim).
  # This box has four 'quadrants' bottom,right,top,left.
  # The 'right' quadrant is a triangle bound by:
  # (0, bottom-right corner, top-right corner)

  xmin <- xlim[1]; xmax <- xlim[2]
  ymin <- ylim[1]; ymax <- ylim[2]

  tr <- atan2(ymax, xmax) # Angle to top-right corner
  tl <- atan2(ymax, xmin) #   top-left
  bl <- atan2(ymin, xmin) #   bottom-left
  br <- atan2(ymin, xmax) #   bottom-right
  phi <- atan2(y, x)      # Angle to each point

  # Instead of many "if-else" terms, just sum(quadrant_indicator * ordinate)
  xb <- (bl < phi & phi <= br) * (ymin*x/y) + # bottom edge
        (br < phi & phi <= tr) * (xmax) +     # right
        (tr < phi & phi <= tl) * (ymax*x/y) + # top
        (phi <= bl | phi > tl) * (xmin)       # left

  yb <- (bl < phi & phi <= br) * (ymin) +
        (br < phi & phi <= tr) * (xmax*y/x) +
        (tr < phi & phi <= tl) * (ymax) +
        (phi <= bl | phi > tl) * (xmin*y/x)

  return(cbind(xb, yb))
}

plot.gge <- function(x, title=substitute(x), ...) {

  # For now, only a mosaic plot.
  # Todo: Scree, heatmap

  #browser()
  op1 <- par(mfrow=c(2,2))
  R2 <- x$R2

  # Scree plot
  op2 <- par(pty='s', mar=c(3,5,2,1))
  plot(1:length(R2), R2, type="b", axes=FALSE,
       main="Scree plot", xlab="", ylab="Pct SS")
  axis(1, at=pretty(1:length(R2)), cex.axis=0.75)
  axis(2, at=pretty(c(0,max(R2))), cex.axis=0.75)

  # Mosaic
  par(pty='s', mar=c(2,1,2,1))
  mosaicplot(x$mosdat, main="",
             col=c("darkgreen","lightgreen","gray70"), off=c(0,0))
  mtext(title, line=.5, cex=1)

  par(op2)
  par(op1)

  invisible()
}


biplot.gge <- function(x, title=substitute(x), subtitle="",
                       cex.gen=0.7, cex.env=.6,
                       col.gen="#005000", col.env="darkorange4",
                       #pch.gen=1,
                       lab.env = TRUE,
                       comps=1:2,
                       flip="auto", ...){

  # x: A model object of class 'gge'
  # Must include ... because the generic 'biplot' does

  gen.group <- x$gen.group
  env.group <- x$env.group
  genCoord <- x$genCoord
  locCoord <- x$locCoord
  blockCoord <- x$blockCoord
  genMeans <- x$genMeans
  #tab <- x$tab
  R2 <- x$R2
  pctMiss <- x$pctMiss
  #maxPCs <- x$maxPCs

  groupNames <- names(table(env.group))
  n.gen.grp <- length(unique(gen.group)) # 0 for NULL
  n.env.grp <- length(unique(env.group))

  expand.range <- function(xx) {
    if(xx[1] > 0) xx[1] <-  - xx[1]
    else if(xx[2] < 0) xx[2] <-  - xx[2]
    return(xx)
  }

  # Subtitle
  subtitle <- ""
  subtitle <- paste0(subtitle, "method=", x$method)
  subtitle <- paste0(subtitle, ", center=", x$center)
  subtitle <- paste0(subtitle, ", scale=", x$scale)
  subtitle <- paste0(subtitle, ", missing: ", round(pctMiss*100,1), "%")

  # Environment (group) colors (first one is used for environments)
  # Replicate colors if not enough have been specified
  col.env <- c(col.env, "blue","black","purple","darkgreen", "red",
               "dark orange", "deep pink", "#999999", "#a6761d")
  if(n.env.grp < 2) {
    col.env <- col.env[1]
  } else {
    col.env <- rep(col.env, length=n.env.grp)
  }


  # Flip. If 'auto', flip the axis so that genotype ordinate is positively
  # correlated with genotype means.
  if(length(flip)<length(comps)) flip <- rep(flip,length=length(comps))
  for(i in 1:length(comps)){
    ix <- comps[i]
    if(flip[i]==TRUE | (flip[i]=="auto" & cor(genMeans, genCoord[,ix]) < 0)){
      locCoord[, ix] <-  - locCoord[, ix]
      genCoord[, ix] <-  - genCoord[, ix]
      blockCoord[, ix] <- - blockCoord[, ix]
    }
  }

  # Set up plot
  par(pty='s')

  xcomp <- comps[1] # Component for x axis
  ycomp <- comps[2] # Component for y axis

  # Axis labels
  labs <- paste("PC ", c(xcomp, ycomp),
                  " (", round(100*R2[c(xcomp,ycomp)],0), "% TSS)", sep="")
  xlab <- labs[1] ; ylab <- labs[2]

  # Determine the range (for genotypes).
  rg1 <- expand.range(range(genCoord[, xcomp]))
  rg2 <- expand.range(range(genCoord[, ycomp]))
  #xlimg <- ylimg <- range(rg1, rg2)
  xmid <- mean(range(rg1))
  ymid <- mean(range(rg2))
  half <- 1.05 * max(diff(rg1), diff(rg2))/2 # Add 5% on each side
  xlimg <- c(xmid-half, xmid+half)
  ylimg <- c(ymid-half, ymid+half)
  # Range for loc and block.  Block coord are always 'inside' loc coord box
  re1 <- expand.range(range(c(locCoord[, xcomp])))
  re2 <- expand.range(range(c(locCoord[, ycomp])))
  ratio <- max(c(re1, re2)/c(xlimg, ylimg)) * 1.1
  xlime <- xlimg*ratio ; ylime <- ylimg*ratio

  # Set up plot for environment vectors
  plot(NULL, type = "n", xaxt="n", yaxt="n", xlab="", ylab="", xlim=xlime, ylim=ylime)
  # Add the margin axis labels and titles
  mtext(xlab, side=1, line=.5, cex=.8)
  mtext(ylab, side=2, line=.5, cex=.8)
  mtext(title, side=3, line=2.5)
  mtext(subtitle, side=3, line=0.9, cex=.7)
  abline(v = 0, h=0, lty = 3, col="gray80") # dashed lines through the 'origin'

#  browser()
  # I'm no longer sure what the radius should be...
  ## # Add a circle of average radius to standardized biplots.
  ## # Do this first so we don't overwrite group/genotype labels and so that
  ## # the unit circle is on the locCoord scale, not the genCoord scale.
  ## if(((is.logical(x$scale) && x$scale) | is.numeric(x$scale) |
  ##     x$scale=="uv")){
  ##   angles <- seq(from=0,to=2*pi,length=100)
  ##   radius <- sqrt(nrow(genCoord)) * ratio
  ##   xc <- radius * sin(angles)
  ##   yc <- radius * cos(angles)
  ##   lines(xc,yc,col="cornflowerblue")
  ## }

  # Plot locs (points OR labels, but not both) colored by group
  if(is.null(env.group))
    eix <- rep(1, nrow(locCoord))
  else eix <- as.numeric(factor(env.group))
  if(lab.env == TRUE)
    text(locCoord[ , c(xcomp, ycomp), drop = FALSE],
         rownames(locCoord), cex=cex.env, col = col.env[eix])
  else
    points(locCoord[ , c(xcomp, ycomp), drop = FALSE],
           cex = cex.env, col = col.env[eix]) # pch = (1:n.env.grp)[eix])

  # Draw vectors.  Shorten by 5% to reduce over-plotting the label
  if(n.env.grp < 2){
    # Draw vector to each loc
    segments(0, 0, .95*locCoord[,xcomp], .95*locCoord[,ycomp], col = col.env[1])
  } else {
    # Short residual vectors from group mean to each loc
    segments(blockCoord[ , xcomp], blockCoord[ , ycomp],
             locCoord[ , xcomp], locCoord[ , ycomp],
             col = col.env[eix], lwd = .5)

    # Draw solid-line part of the group vector
    ubc <- blockCoord[groupNames,] # Get unique row for each group
    segments(0, 0, ubc[ , xcomp], ubc[ , ycomp], lwd = 3, col=col.env) # no 'eix'
    # End point
    points(ubc[ , c(xcomp,ycomp)], pch = 16, col=col.env) # no 'eix'
    # The 'xy' variable extends the vector to the edge of plot
    xy <- extend(ubc[ , xcomp], ubc[ , ycomp], xlime, ylime)
    # Now the extended dashed-line part of the group vector
    #segments(ubc[ , xcomp], ubc[ , ycomp],
    #         .95*xy[,1], .95*xy[,2], lty = 3, col=col.env)
    # Group label
    text(xy[,1], xy[,2], rownames(ubc), cex = 1, col=col.env)
  }

  # pch.gen <- c(pch.gen, setdiff(1:20, pch.gen))
  if(n.gen.grp < 2) {
    col.gen <- col.gen[1] # in case there are multiple colors
    #pch.gen <- pch.gen[1]
  } else {
    col.gen <- rep(col.gen, length=n.gen.grp)
    #pch.gen <- rep(pch.gen, length=n.gen.grp)
  }

  # New coordinate system for genotypes
  par(new = TRUE)
  plot(NULL, type = "n", xaxt="n", yaxt="n", xlab="", ylab="",
       xlim=xlimg, ylim=ylimg)
  # Now overlay genotype labels and/or points
  if(n.gen.grp < 2) {
    text(genCoord[, c(xcomp, ycomp)], rownames(genCoord), cex=cex.gen, col=col.gen)
  } else {
    gix <- as.numeric(as.factor(gen.group))
    points(genCoord[, c(xcomp, ycomp)], cex=cex.gen, col=col.gen) # pch=pch.gen[gix])
    text(genCoord[, c(xcomp, ycomp)], paste(" ",rownames(genCoord)),
         cex=cex.gen, col=col.gen[gix], adj=c(0,.5))
  }

  invisible()

}
