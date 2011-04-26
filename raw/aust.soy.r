# aust.soy.r
# Time-stamp: <08 Apr 2011 13:59:24 c:/x/rpack/agridat/raw/aust.soy.r>

setwd("c:/x/rpack/agridat")

dat <- read.csv("raw/australia_soybean.csv")
dat <- transform(dat, env=factor(paste(substring(loc,1,1), substring(year,3),
                        sep="")),
                 gen=factor(gen))
dat <- dat[ , c("env","loc","year","gen","yield","height","lodging","size","protein","oil")]
australia.soybean <- dat
export(australia.soybean, file="data/australia.soybean.txt")
promptData(australia.soybean, "man/australia.soybean.Rd")

# ----------------------------------------------------------------------------

dat <- australia.soybean
dm <- melt(dat, id.var=c('env', 'year','loc','line'))

# Joint plot of genotypes & traits. Similar to Figure 1 of Kroonenberg 1989
dmat <- acast(dm, line~variable, fun=mean)
dmat <- scale(dmat)
biplot(princomp(dmat))

# Figure 1 of Kozak 2010, lines 44-58
dmat2 <- dmat[44:58,]
parallel(dmat2[,c(2:6,1)], horiz=FALSE)





soy.a <- aggregate(cbind(yield, height, lodging, size, protein, oil) ~ line,
                   soy, mean)
library(kw)
rownames(soy.a) <- soy.a$line
soy.a$line <- NULL
biplot(gge(soy.a))
heatmap(as.matrix(soy.a))
windows()
pairs(soy[,5:10])
with(soy, plot(size~as.factor(line)))
with(soy, plot(height~as.factor(line)))

soy2 <- melt(soy, measure.var='yield', id.var=c('line', 'env'))
soy2 <- acast(soy2, line~env)
soy2 <- as.data.frame(soy2)
rownames(soy2) <- soy2$line
soy2$line <- NULL
colnames(soy2) <- paste("E",1:8,sep="")
biplot(gge(soy2))


# Multidimensional scaling (see Basford1982)
library(MASS)
ord = isoMDS(dist(scale(soy.a)))
plot(ord$points, asp = 1, type="n")
text(ord$points, rownames(ord$points))

# Another MDS
soy.mds = cmdscale(dist(scale(soy.a)))
plot(soy.mds,type='n')
text(soy.mds,rownames(soy.mds))

# ----------------------------------------------------------------------------


# ----------------------------------------------------------------------------
