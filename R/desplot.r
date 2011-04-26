# desplot.r
# Time-stamp: <26 Apr 2011 12:20:17 c:/x/rpack/agridat/R/desplot.r>

# Needs grid, lattice, reshape2

RedGrayBlue <- colorRampPalette(c("firebrick", "lightgray", "#305a7f"))

desplot <- function(form=formula(NULL ~ x + y),
                    data,
                    num=NULL, col=NULL, lab=NULL, outline1=NULL, outline2=NULL,
                    levels.fill=NULL, levels.col=NULL, levels.lab=NULL,
                    col.regions=RedGrayBlue, 
                    outline1.gpar=gpar(col="black", lwd=3),
                    outline2.gpar=gpar(col="yellow", lwd=1, lty=1),
                    main=NULL, at,
                    ticks=FALSE,
                    shorten='abb',
                    show.key=TRUE,
                    key.cex, # left legend cex
                    cex=.4, # cell cex
                    strip.cex=.75, ...){

  if(missing(main)) main <- deparse(substitute(data)) # Use data name by default
  
  # Force character, in case we forgot to quote the argument
  numvar <- substitute(num)
  colvar <- substitute(col)
  labvar <- substitute(lab)
  out1var <- substitute(outline1)
  out2var <- substitute(outline2)

  if(!is.null(numvar) & !is.character(numvar))
    numvar <- deparse(numvar)
  if(!is.null(colvar) & !is.character(colvar))
    colvar <- deparse(colvar)
  if(!is.null(labvar) & !is.character(labvar))
    labvar <- deparse(labvar)
  if(!is.null(out1var) & !is.character(out1var))
    out1var <- deparse(out1var)
  if(!is.null(out2var) & !is.character(out2var))
    out2var <- deparse(out2var)
  
  hasNum <- !is.null(numvar)
  hasCol <- !is.null(colvar)
  hasLab <- !is.null(labvar)
  hasOut1 <- !is.null(out1var)
  hasOut2 <- !is.null(out2var)
  
  data <- droplevels(data) # In case the user called with subset(obj, ...)

  # Split a formula like: resp~x*y|cond into a list of text strings called
  # resp, xy (vector like 'x' '*' 'y') , cond ('cond' could be a vector)
  ff <- latticeParseFormula(form, data)
  ff <- list(resp = ff$left.name,
             xy = strsplit(ff$right.name, " ")[[1]],
             cond = names(ff$cond))
  if(length(ff$resp)==0L) ff$resp <- NULL

  fillvar <- ff$resp
  if(is.null(fillvar)) {
    fillType <- "none"
    form <- as.formula(paste(".const", form[[1]], deparse(form[[2]]), sep=""))
    data[['.const']] <- rep(1, nrow(data)) # Hack.  We need something to plot.
  } else {
    fillType <- ifelse(is.factor(data[[fillvar]]), "factor", "num")
  }
  xvar <- ff$xy[1]
  yvar <- ff$xy[3]
  panelvar <- ff$cond[1]

  "%!in%" <- function (a, b) !(a %in% b)
  # Error checks
  dn <- names(data)
  if(!is.null(fillvar) && fillvar %!in% dn)
    stop("Couldn't find '", fillvar,"' in the data frame.")
  if(!is.null(panelvar) && panelvar %!in% dn)
    stop("Couldn't find '", panelvar,"' in the data frame.")
  if(!is.null(colvar) && colvar %!in% dn)
    stop("Couldn't find '", colvar,"' in the data frame.")
  if(!is.null(numvar) && numvar %!in% dn)
    stop("Couldn't find '", numvar,"' in the data frame.")
  if(!is.null(labvar) && labvar %!in% dn)
    stop("Couldn't find '", labvar,"' in the data frame.")
  if(!is.null(out1var) && out1var %!in% dn)
    stop("Couldn't find '", out1var,"' in the data frame.")
  if(!is.null(out2var) && out2var %!in% dn)
    stop("Couldn't find '", out2var,"' in the data frame.")

  xlab <- ifelse(ticks, xvar, "")
  ylab <- ifelse(ticks, yvar, "")
  
  if(is.null(levels.fill))
    levels.fill <- c("#E6E6E6","#FFD9D9","#FFB2B2","#FFD7B2","#FDFFB2",
                     "#D9FFB2","#B2D6FF","#C2B2FF","#F0B2FF","#A6FFC9",
                     "#FF8C8C","#B2B2B2","#FFBD80","#BFFF80","#80BAFF",
                     "#9980FF","#E680FF","#D0D192","#59FF9C","#FFA24D",
                     "#FBFF4D","#4D9FFF","#704DFF","#DB4DFF","#808080",
                     "#9FFF40","#C9CC3D")

  if(is.null(levels.col))
    levels.col <- c("black", "red3", "darkorange2", "chartreuse4",
                  "deepskyblue4", "blue", "purple4", "darkviolet", "maroon")


  # Change x/y from factor to numeric if needed.  Add missing levels.
  fac2num <- function(x) as.numeric(levels(x))[x]
  if(is.factor(data[[xvar]])) data[[xvar]] <- fac2num(data[[xvar]])
  if(is.factor(data[[yvar]])) data[[yvar]] <- fac2num(data[[yvar]])
  data <- .addLevels(data, xvar, yvar, panelvar)  

  x <- data[[xvar]]
  y <- data[[yvar]]
  outline1 <- if(hasOut1) data[[out1var]] else NULL
  outline2 <- if(hasOut2) data[[out2var]] else NULL
  

  nr <- 0 # cumulative rows in the key
  levs <- NULL

  # Panel
  if(!is.null(panelvar)) panel <- data[[panelvar]]

  # Outlines
  if(hasOut1){
    nr <- nr + 1
    levs <- c(levs, out1var)
  }
  if(hasOut2){
    nr <- nr + 1
    levs <- c(levs, out2var)
  }
  nr <- nr + (hasOut1 | hasOut2)
  
  # Check for multiple values
  if(is.null(panelvar)){
    tt <- table(x,y)
  } else {
    tt <- table(x,y,panel)
  }
  if(any(tt>1))
    warning("There are multiple data for each x/y/panel combination")
  
  if(fillType=="num"){
    fill <- data[[fillvar]]
  } else if(fillType=="factor") { # fill
    fill <- data[[fillvar]] 
    levsFill <- levels(fill)
    nFill <- length(levsFill)
    nr <- nr + 2 + nFill
    levs <- c(levs, levsFill)
    if(length(levels.fill) < nFill) levels.fill <-
      rep(levels.fill, length=nFill)
  }

  if(hasNum) { # number
    num <- factor(data[[numvar]])
    levsNum <- levels(num)
    nNum <- length(levsNum)
    nr <- nr + 2 + nNum
    levs <- c(levs, levsNum)
  }

  if(hasCol) { # color
    col <- data[[colvar]]
    if(!is.factor(col)) col <- factor(col) # In case col is numeric
    levsCol <- levels(col)
    nCol <- length(levsCol)
    nr <- nr + 2 + nCol
    levs <- c(levs, levsCol)
    if(length(levels.col) < nCol) levels.col <- rep(levels.col, length=nCol)
  } else {
    col <- rep(1, nrow(data)) # Use black by default
  }

  if(hasLab) { # label
    lab <- data[[labvar]]
    levsLab <- levels(lab)
    nLab <- length(levsLab)
    nr <- nr + 2 + nLab
    levs <- c(levs, levsLab)
    if(length(levels.col) < nLab) levels.col <- rep(levels.col, length=nLab)
  }

  # Set up short version of labels
  if(hasLab & is.null(levels.lab)){
    if(shorten=='no' | shorten=='none')
      levels.lab <- levsLab
    else if (shorten=='abb')
      levels.lab <- abbreviate(levsLab, 2, method='both')
    else if (shorten=='sub')
      levels.lab <- substring(levsLab, 1, 3)
  } else {
    # Nothing.  Why is this here?
  }

  # We might not have a key, even though it was requested
  if (nr==0) show.key <- FALSE
  
  # Set up key grobs
  if(show.key) {
    longstring <- levs[which.max(nchar(levs))]
    if(missing(key.cex)) {
      if(nr < 30) key.cex <- 1
      else if(nr < 40) key.cex <- .75
      else key.cex <- 0.5
    }

    foo <- frameGrob(layout = grid.layout(nrow = nr, ncol = 2,
                       heights = unit(rep(key.cex, nr), "lines"),
                       widths = unit(c(1,1), c("cm","strwidth"),
                         data=list(NULL, longstring))))
    
    offset <- 1

    # Outline
    if(hasOut1){
      foo <- placeGrob(foo, linesGrob(x = unit(c(.2, .8), "npc"),
                                      y = unit(.5, "npc"),
                                      gp=outline1.gpar),
                       row = offset, col = 1)
      foo <- placeGrob(foo, textGrob(lab = out1var, gp=gpar(cex=key.cex)),
                       row = offset, col = 2)
      offset <- offset + 1
    }
    if(hasOut2){
      foo <- placeGrob(foo, linesGrob(x=c(.2,.8), y=.5, gp=outline2.gpar),
                       row = offset, col = 1)
      foo <- placeGrob(foo, textGrob(lab = out2var, gp=gpar(cex=key.cex)),
                       row = offset, col = 2)
      offset <- offset + 1
    }
    if(hasOut1 | hasOut2) offset <- offset + 1 # blank line
    
    # Fill
    if(fillType=='factor') {  
      foo <- placeGrob(foo, textGrob(lab = fillvar, gp=gpar(cex=key.cex)),
                       row = offset, col = 2)
      for(kk in 1:nFill){
        foo <- placeGrob(foo, rectGrob(width = 0.6,
                                       gp = gpar(col="#FFFFCC",
                                         fill=levels.fill[kk], cex=key.cex)), 
                         row = offset + kk, col = 1)
        foo <- placeGrob(foo, textGrob(lab = levsFill[kk],
                                       gp=gpar(cex=key.cex)),
                         row = offset+kk, col = 2)
      }
      offset <- offset + 1 + nFill + 1
    } else if(fillType=="none"){
      # Create a transparent 'fill' variable
      fill <- rep(1, nrow(data))
      nFill <- 1
      levels.fill <- "transparent" 
    }

    # Number
    if(hasNum) {  
      foo <- placeGrob(foo, textGrob(lab = numvar, gp=gpar(cex=key.cex)),
                       row = offset, col = 2)
      for(kk in 1:nNum){
        foo <- placeGrob(foo, textGrob(lab = kk, gp=gpar(cex=key.cex)),
                         row = offset + kk, col = 1)
        foo <- placeGrob(foo, textGrob(lab = levsNum[kk], gp=gpar(cex=key.cex)),
                         row = offset + kk, col = 2)
      }
      offset <- offset + 1 + nNum + 1
    }  
    # Color
    if(hasCol) {
      foo <- placeGrob(foo, textGrob(lab = colvar, gp=gpar(cex=key.cex)),
                       row = offset, col = 2)
      for(kk in 1:nCol){
        foo <- placeGrob(foo, pointsGrob(.5,.5, pch=19,
                                         gp=gpar(col=levels.col[kk],
                                           cex=key.cex)),
                         row = offset + kk, col = 1)
        foo <- placeGrob(foo, textGrob(lab = levsCol[kk], gp=gpar(cex=key.cex)),
                         row = offset + kk, col = 2)
      }
      offset <- offset + 1 + nCol + 1
    }
    # Label
    if(hasLab) {  
      foo <- placeGrob(foo, textGrob(lab = labvar, gp=gpar(cex=key.cex)),
                       row = offset, col = 2)
      for(kk in 1:nLab){
        foo <- placeGrob(foo, textGrob(lab=levels.lab[kk],
                                       gp=gpar(cex=key.cex)),
                         row = offset + kk, col = 1)
        foo <- placeGrob(foo, textGrob(lab = levsLab[kk], gp=gpar(cex=key.cex)),
                         row = offset + kk, col = 2)
      }
      offset <- offset + 1 + nLab + 1
    }  

  } else foo <- NULL

  # Panel text
  if(hasLab) {
    ptext <- levels.lab[as.numeric(lab)]
  } else if(hasNum) {
    ptext <- as.numeric(num)
  } else if(hasCol) {
    ptext <- rep("x", length=nrow(data))
  }
    
  
  # at = # cut points for region colors
  if(fillType=="num") {
    zrng <- lattice:::extend.limits(range(as.numeric(fill), finite = TRUE))
    if(is.function(col.regions)) {
      if(missing(at)) at <- seq(zrng[1], zrng[2], length.out = 15 + 1)
      colreg <- col.regions(length(at)-1)
    } else { # col.regions is a vector
      colreg <- col.regions
      nbins <- length(col.regions)
      if(missing(at)) {
        at <- seq(zrng[1], zrng[2], length.out = nbins + 1)
      } else {
        if(nbins != length(at)-1) stop("Length of 'at' must be 1 more than length of 'col.regions'\n")
      }
    }
  } else { # fill with a factor
    at <- c((0:nFill+.5))
    colreg <- levels.fill[1:nFill]
  }

  out <-
    levelplot(form,
              data=data
              , out1f=outline1, out1g=outline1.gpar
              , out2f=outline2, out2g=outline2.gpar
              , col.regions=colreg
              , colorkey = if(fillType=="num") TRUE else FALSE
              , as.table=TRUE
              , at=at
              , legend=if(show.key) list(left=list(fun=foo)) else list()
              , main=main
              , xlab=xlab
              , ylab=ylab
              , scales=list(relation='free' # Different scales for each panel
                  , draw=ticks # Don't draw panel axes
                  )
              , panel=function(x, y, z, subscripts, groups, ...,
                  out1f, out1g, out2f, out2g){
                # First fill the cells and outline
                panel.outlinelevelplot(x, y, z, subscripts, at, ...,
                                       out1f=out1f, out1g=out1g, 
                                       out2f=out2f, out2g=out2g)
                # Then, if we have numbers, colors, or labels
                if(hasNum|hasLab|hasCol)
                  panel.text(x[subscripts], y[subscripts],
                             ptext[subscripts],
                             cex=cex,
                             col=levels.col[as.numeric(col[subscripts])])
              },
    strip=strip.custom(par.strip.text=list(cex=strip.cex)))

  # Use 'update' for any other modifications
  #if(!show.key) out <- update(out, legend=list(left=NULL))
                  
  return(out)
}

panel.outlinelevelplot <- 
  function(x, y, z, subscripts, at, ...,
           alpha.regions = 1,
           out1f, out1g, out2f, out2g) {
    dots=list(...)
    col.regions=dots$col.regions

    # Based on panel.levelplot
    if (length(subscripts) == 0) 
        return()
    x.is.factor <- is.factor(x)
    y.is.factor <- is.factor(y)
    x <- as.numeric(x)
    y <- as.numeric(y)
    z <- as.numeric(z)
    zcol <- level.colors(z, at, col.regions, colors = TRUE)
    x <- x[subscripts]
    y <- y[subscripts]
    
    minXwid <- if (length(unique(x)) > 1) 
        min(diff(sort(unique(x))))
    else 1
    minYwid <- if (length(unique(x)) > 1) 
        min(diff(sort(unique(y))))
    else 1
    fullZrange <- range(as.numeric(z), finite = TRUE)
    z <- z[subscripts]
    zcol <- zcol[subscripts]
    scaleWidth <- function(z, min = 0.8, max = 0.8, zl = range(z, 
        finite = TRUE)) {
        if (diff(zl) == 0) 
            rep(0.5 * (min + max), length(z))
        else min + (max - min) * (z - zl[1])/diff(zl)
    }
    if (x.is.factor) {
        ux <- sort(unique(x[!is.na(x)]))
        lx <- rep(1, length(ux))
        cx <- ux
    }
    else {
        ux <- sort(unique(x[!is.na(x)]))
        bx <- if (length(ux) > 1) 
            c(3 * ux[1] - ux[2], ux[-length(ux)] + ux[-1], 3 * 
                ux[length(ux)] - ux[length(ux) - 1])/2
        else ux + c(-0.5, 0.5) * minXwid
        lx <- diff(bx)
        cx <- (bx[-1] + bx[-length(bx)])/2
    }
    if (y.is.factor) {
        uy <- sort(unique(y[!is.na(y)]))
        ly <- rep(1, length(uy))
        cy <- uy
    }
    else {
        uy <- sort(unique(y[!is.na(y)]))
        by <- if (length(uy) > 1) 
            c(3 * uy[1] - uy[2], uy[-length(uy)] + uy[-1], 3 * 
                uy[length(uy)] - uy[length(uy) - 1])/2
        else uy + c(-0.5, 0.5) * minYwid
        ly <- diff(by)
        cy <- (by[-1] + by[-length(by)])/2
    }
    idx <- match(x, ux)
    idy <- match(y, uy)

    # Fill the cells
    grid.rect(x = cx[idx], y = cy[idy],
              width=lx[idx] * scaleWidth(z, 1, 1, fullZrange), 
              height = ly[idy] * scaleWidth(z, 1, 1, fullZrange),
              default.units = "native",
              gp = gpar(fill = zcol, lwd = 1e-05, col="transparent",
                alpha = alpha.regions))

    draw.outline <- function(x, y, lab, gp) {
      out1 <- data.frame(x=x, y=y, lab=lab, stringsAsFactors = FALSE)
      out1 <- melt(out1, id.var=c('x','y'))
      # reshape melts char vector to char, reshape 2 melts to factor!
      # both packages could be attached, hack to fix this...
      out1$value <- as.character(out1$value)
      out1 <- acast(out1, y~x)

      # Horizontal lines above boxes
      # Careful.  The matrix is upside down from the levelplot 
      hor <- out1[2:nrow(out1)-1, ] != out1[2:nrow(out1), ]
      hor <- melt(hor)
      hor <- hor[!(hor$value==FALSE | is.na(hor$value)),]
      if(nrow(hor)>0) {
        hx <- hor[,2] # reshape uses X2, reshape2 uses Var2
        hy <- hor[,1]
        grid.polyline(x=c(hx-.5, hx+.5), y=c(hy+.5, hy+.5),
                      id=rep(1:length(hx), 2), default.units="native", gp=gp)
      }
      # Vertical lines along right side of boxes
      vert <- out1[ , 2:ncol(out1)-1] != out1[ , 2:ncol(out1)]
      vert <- melt(vert)
      vert <- vert[!(vert$value==FALSE | is.na(vert$value)),]
      if(nrow(vert)>0) {
        vx <- vert[,2]
        vy <- vert[,1]
        grid.polyline(x=c(vx+.5, vx+.5), y=c(vy-.5, vy+.5),
                      id=rep(1:length(vx), 2), default.units="native", gp=gp)
      }

    }
    
    # Outline factor 1
    if(!is.null(out1f))
      draw.outline(x, y, as.character(out1f[subscripts]), out1g)

    # Outline factor 2
    if(!is.null(out2f))
      draw.outline(x, y, as.character(out2f[subscripts]), out2g)
    
    return()
}

.addLevels <- function(dat, xvar='x', yvar='y', locvar=NULL){
  # For each loc, we want x/y coords to be complete.
  # NO: 1,2,4.  YES: 1,2,3,4.
  # Add one NA datum for each missing x and each missing y
  # This does NOT completely fill in the grid (as needed by asreml)

  # Original values
  ox <- dat[[xvar]]
  oy <- dat[[yvar]]

  x.is.factor <- is.factor(ox)
  y.is.factor <- is.factor(oy)
  if(x.is.factor | y.is.factor) stop("FIXME: x or y are factors.")
  
  if(is.null(locvar)) {
    loclevs <- factor("1") # hack alert
  } else {
    oloc <- factor(dat[[locvar]]) # In case loc is character
    loclevs <- levels(oloc)
  }

  for(loc.i in loclevs){

    if(is.null(locvar)){
      ux <- sort(unique(ox))
      uy <- sort(unique(oy))
    } else {
      ux <- sort(unique(ox[oloc==loc.i]))
      uy <- sort(unique(oy[oloc==loc.i]))
    }
    # Add new rows and columns. Fill with missing data
    xnew <- setdiff(seq(from=min(ux), to=max(ux), by=1), ux)
    ynew <- setdiff(seq(from=min(uy), to=max(uy), by=1), uy)
    if(length(xnew) > 0){
      newrows <- nrow(dat) + 1:length(xnew)
      dat[newrows, xvar] <- xnew # R creates these rows
      if(!is.null(locvar))
        dat[newrows, locvar] <- rep(loc.i, length(xnew))
    }
    if(length(ynew) > 0){
      newrows <- nrow(dat) + 1:length(ynew)
      dat[newrows, yvar] <- ynew
      if(!is.null(locvar))
        dat[newrows, locvar] <- rep(loc.i, length(ynew))
    }
  }
  return(dat)
}

# ----------------------------------------------------------------------------
if(FALSE){
  data(yates.oats, package="agridat")
  oats35 <- yates.oats

  desplot(yield~x+y, oats35)
  desplot(yield~x+y|block, oats35)

  # Text over continuous colors
  desplot(yield~x+y, oats35, outline1=block, lab=gen, cex=1)

  desplot(yield~x+y, oats35, outline2=block)
  desplot(yield~x+y, oats35, outline1=block, outline2=gen)
  desplot(gen~x+y, oats35, col=block, num=nitro, cex=1, outline1=block)

  # Test 'at' and 'col.regions' for the ribbon
  RedYellowBlue <-
    colorRampPalette(c("#D73027", "#F46D43", "#FDAE61", "#FEE090", "#FFFFBF",
                       "#E0F3F8", "#ABD9E9", "#74ADD1", "#4575B4"))
  eightnum <- function(x) {
    x <- x[!is.na(x)]
    st <- boxplot.stats(x)$stats
    # eps <- .Machine$double.eps
    eps <- 10^(log10(127.4)-6)
    c(min(x)-eps, st[1:2], st[3]-eps, st[3]+eps, st[4:5], max(x)+eps)
  }
  desplot(yield~x+y, oats35, col.regions=RedYellowBlue(7))
  desplot(yield~x+y, oats35, at=eightnum(oats35$yield))
  desplot(yield~x+y, oats35, col.regions=RedYellowBlue(7), at=eightnum(oats35$yield))


  # Test abbreviations  
  desplot(block~x+y, oats35, col=nitro, lab=gen, cex=1, shorten='abb') # def
  desplot(block~x+y, oats35, col=nitro, lab=gen, cex=1, shorten='sub')
  desplot(block~x+y, oats35, col=nitro, lab=gen, cex=1, shorten='no')

  
  # Would be nice if 'character' worked....
  oats35$yt <- factor(as.character(oats35$yield))
  desplot(block~x+y, oats35, lab=yt, shorten='no')
  
  desplot(block~x+y, oats35, col=nitro, lab=gen, cex=1, outline1=block)
  desplot(block~x+y, oats35, col=nitro, lab=gen, cex=1, outline1=block, outline2=gen)
  desplot(block~x+y, oats35, num="gen", col="nitro", cex=1)

  # Test custom labels
  desplot(block~x+y, oats35, lab="gen", col="nitro", cex=1, levels.lab=c('V','G','M'))
  desplot(nitro~x+y, oats35, lab="gen", cex=.9)
  desplot(nitro~x+y, oats35)
  desplot(nitro~x+y|block, oats35, lab="gen", cex=.9)
  
  # No fill color at all
  desplot(~x+y|block, oats35, lab="gen", cex=1)
  desplot(~x+y, oats35, col="gen", cex=1)

  desplot(nitro~x+y|block, oats35, lab="gen", cex=1)
  desplot(block~x+y|block, oats35, col="nitro", lab="gen", cex=1)

}
