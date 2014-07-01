
# vc.r
# Time-stamp: <30 Apr 2014 21:08:40 c:/x/rpack/agridat/R/vc.r>

# ----- lucid -----

lucid <- function(x, dig=4, ...) {
  # Kevin Wright
  # Don't export this--it is also in my personal package.
  # This is the workhorse that does the formatting, but no printing
  # Use 4 significant digits, drop trailing zero, align decimals
  if(class(x)=="numeric" | class(x)=="integer")
    format(format(signif(zapsmall(x), dig), scientific=FALSE,
                  drop0trailing=TRUE))
  else x
}

# ----- generic -----

vc <- function(object, ...) UseMethod("vc")

# ----- default -----

vc.default <- function(object, ...) {
  stop("No default method exists for 'vc'.")
}

# ----- asreml -----

vc.asreml <- function (object, gamma=FALSE, ...) {
  # Kevin Wright

  vv <- summary(object)$varcomp
  if(gamma==FALSE)
    vv$gamma <- NULL

  nm <- rownames(vv)
  nm <- factor(nm, levels=nm) # prevent alphanum sorting
  vv <- cbind(effect=nm, vv)
  rownames(vv) <- NULL

  class(vv) <- c("vc.asreml", class(vv))
  return(vv)
}
print.vc.asreml  <- function(x, dig=4, ...){
  # Kevin Wright
  class(x) <- class(x)[-1] # remove vc.asreml

  # Use 2 signif decimals for z.ratio
  x$z.ratio <- signif(x$z.ratio, 2)

  x[] <- lapply(x, lucid, dig)

  # Rename for printing.  Not all columns are always present--don't use reName
  cn <- colnames(x)
  cn[cn=="constraint"] <- "constr"
  colnames(x) <- cn

  # Shorten constraint to 3-letter code
  levels(x$constr)[levels(x$constr)=="Fixed"] <- "fix"
  levels(x$constr)[levels(x$constr)=="Boundary"] <- "bound"
  levels(x$constr)[levels(x$constr)=="Positive"] <- "pos"
  levels(x$constr)[levels(x$constr)=="Unconstrained"] <- "uncon"

  ## # Clean up random coefficient names
  ## rn <- as.character(x$Effect)
  ## # us(2)
  ## introws <- which(grepl("!us\\(2\\).1:1", rn))
  ## if(length(introws) > 0){
  ##   pluspos <- regexpr("\\+",rn[introws])
  ##   bangpos <- regexpr("\\!",rn[introws])
  ##   int <- substring(rn[introws], 1, pluspos-1)
  ##   slp <- substring(rn[introws], pluspos+1, bangpos-1)
  ##   rn[introws] <- paste(int, " us(2)")
  ##   rn[introws+1] <- "(corr)  us(2)"
  ##   rn[introws+2] <- paste(slp, " us(2)")
  ##   x$Effect <- rn
  ## }
  ## # diag(2)
  ## introws <- which(grepl("!diag\\(2\\).1.var", rn))
  ## if(length(introws) > 0){
  ##   pluspos <- regexpr("\\+",rn[introws])
  ##   bangpos <- regexpr("\\!",rn[introws])
  ##   int <- substring(rn[introws], 1, pluspos-1)
  ##   slp <- substring(rn[introws], pluspos+1, bangpos-1)
  ##   rn[introws] <- paste(int, " diag(2)")
  ##   rn[introws+1] <- paste(slp, " diag(2)")
  ##   x$Effect <- rn
  ## }

  print(x, row.names=FALSE) # Do not print row numbers
  invisible(x)
}

# ----- lme -----

vc.lme <- function(object, ...) {
  # Kevin Wright
  vv <- nlme::VarCorr(object)
  vv <- as.matrix(vv)

  # Convert from text to numeric matrix, then to data.frame
  nm <- rownames(vv)
  nm <- factor(nm, levels=nm) # prevent alphanum sorting
  # vv[] <- apply(vv,) is not working
  v2 <- apply(vv, 2, function(x) suppressWarnings(as.numeric(x)))
  v2 <- as.data.frame(v2)
  v2 <- cbind(effect=nm, v2)
  rownames(v2) <- NULL

  names(v2) <- tolower(names(v2))

  class(v2) <- c("vc.lme", class(v2))
  return(v2)
}
print.vc.lme <- function(x, dig=4, ...) {
  class(x) <- class(x)[-1] # remove vc.lme
  x[] <- lapply(x, lucid, dig)
  print(x, quote=FALSE, row.names=FALSE)
  invisible()
}

# ----- lme4 -----

## This no longer needed.  as.data.frame.VarCorr.lmerMod
## vc.lmerMod <- function(object, ...) {
##   # Kevin Wright
##   oo <- options(scipen=15) # turn off scientific notation
##   tt <- capture.output(print(VarCorr(object), comp=c("Variance","Std.Dev.")))
##   options(oo) # restore settings
##   tt1 <- tt[1] # header row
##   c1 <- regexpr("Groups",tt1)
##   c2 <- regexpr("Name",tt1)
##   c3 <- regexpr("Variance",tt1)
##   c4 <- regexpr("Std.Dev.",tt1)
##   c5 <- regexpr("Corr",tt1)

##   # What about random correlations???
##   dd <- data.frame(groups=substring(tt[-1], c1, c2-2),
##                    name=substring(tt[-1], c2, c3-2),
##                    variance=as.numeric(substring(tt[-1], c3, c4-2)),
##                    stddev=as.numeric(substring(tt[-1], c4)))
##   if(c5 > 0) {
##     warning("Correlations exist, use VarCorr()\n")
##     ## cc <- substring(tt[-1], c5)
##     ## # Replace leading/trailing zeros
##     ## cc <- gsub("^ +","",cc)
##     ## cc <- gsub(" +$","",cc)
##     ## cc <- lapply(strsplit(cc, " +"), as.numeric)
##     ## Need to unroll the correlation matrix into a vector
##     ## unlist(cc)
##     ## outer(dd$name[1:4], dd$name[1:4], paste)[lower.tri(diag(4))]
##   }

##   class(dd) <- c("vc.lmerMod", class(dd))
##   return(dd)
## }

vc.glmerMod <- function(object, ...) {
  dd <- as.data.frame(VarCorr(object))
  class(dd) <- c("vc.lmerMod", class(dd))
  return(dd)

}

vc.lmerMod <- function(object, ...) {
  dd <- as.data.frame(VarCorr(object))
  class(dd) <- c("vc.lmerMod", class(dd))
  return(dd)
}

print.vc.lmerMod <- function(x, dig=4, ...){
  class(x) <- class(x)[-1] # remove vc.lmerMod
  x[] <- lapply(x, lucid, dig)
  print(x, row.names=FALSE)
  invisible(x)
}

# ----- tests -----

if(FALSE) {

  require("nlme")
  #data(Rail)
  m1n <- lme(travel~1, random=~1|Rail, data=Rail)
  vc(m1n)

  require("lme4")
  m1l <- lmer(travel~1 + (1|Rail), data=Rail)
  vc(m1l)

  require("asreml")
  m1a <- asreml(travel~1, random=~Rail, data=Rail)
  vc(m1a)

}


