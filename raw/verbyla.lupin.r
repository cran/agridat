# verbyla.r
# Time-stamp: <08 Apr 2011 10:22:08 c:/x/rpack/agridat/raw/verbyla.lupin.r>

library(asreml)
library(kw)
library(Hmisc)
library(lattice)

setwd("c:/x/rpack/agridat/")

lupin <- read.table("raw/verbyla_lupin.txt", header=TRUE)
lupin$rancol <- lupin$ranrow <- NULL
lupin <- reName(lupin, rate="seedrate", col="column",
                serp='fixrow1', linrate='x')
lupin$loc <- lookup(lupin$site, 1:11,
                    c("Badgingerra","Corrigin","MtBarker","Newdegate",
                      "WonganHills","Corrigin","EastChapman","MtBarker",
                      "Mullewa","SouthCarrabin","WonganHills"))
lupin$year <- lookup(lupin$site, 1:11,
                     c(91,91,91,91,91,92,92,92,92,92,92))
levels(lupin$gen)[1] <- "Myallie" # Name changed after data collected
# pad with zero because variogram re-orders levels to 1, 10, 11, 2, 3, ...
lupin <- transform(lupin, site=paste("S",formatC(site, 1, flag='0'),sep="") )
lupin <- transform(lupin, serp=paste("SE",serp,sep=""))
lupin <- lupin[,c("gen", "site", "rate", "row", "col", "serp", "linrow", "lincol", 
                  "linrate", "yield", "year","loc")]
str(lupin)

verbyla.lupin <- lupin
export(verbyla.lupin, "data/verbyla.lupin.txt")
#promptData(verbyla.lupin, file="man/verbyla.lupin.Rd")
# ----------------------------------------------------------------------------

dat <- verbyla.lupin
dat <- transform(dat, site=factor(site),
                 year=factor(year)
                 colf=factor(col), rowf=factor(row))

# Check layout of lupin...it IS RCB
if(require(kw)) {
  levelplot(gen~colf+rowf|site, data=lupin, col.regions=terrain.colors)
  levelplot(rate~colf+rowf|site, data=lupin, col.regions=terrain.colors)
  levelplot(serp~colf+rowf|site, data=lupin, col.regions=terrain.colors)
  desplot(gen~colf+rowf|site, data=lupin)
  desplot(serp~colf+rowf|site, data=lupin)
  desplot(rate~colf+rowf|site, data=lupin, col=gen)
  desplot(~colf+rowf|site, data=lupin, num=rate, col=gen)
}


# There are 9 varieties
length(with(lupin, table(gen)))      # 9 varieties for RC1
length(with(lupin, table(gen,site))) # 99 site:gen combinations for RC2

# ----------------------------------------------------------------------------

lupin <- transform(lupin, yrloc = factor(paste(year, loc, sep=" ")))
lupin <- sort(lupin, by=~site+rowf+colf)

# Initial AR1xAR1 spatial model.  No spline terms.
m0 <- asreml(yield ~ 1 + site
             + x
             + site:x
             # Random part
             , random = ~
             + dev(rate)
             + site:dev(rate)
             + gen:dev(rate)
             + str(~gen+gen:x, ~us(2):id(9)) # RC1
             + str(~site:gen+site:gen:x, ~us(2):id(99)) # RC2
             + site:gen:dev(rate)
             # Random spatial
             + at(site):ar1v(rowf):ar1(colf) # AR1
             , data=lupin)
m0 <- update(m0)
# Figure 7.  Doesn't quite match Verbyla
# head(coef(m0, pattern="site:rowf:colf"))
v0 <- variogram.asreml(m0, site:rowf:colf ~ rowf+colf | site)
plot(v0, xlim=c(0,20), zlim=c(0,2)) # screen=list(z=30,x=-60, y=0))

# ----------------------------------------------------------------------------

# Base model
m1 <- update(m0, fixed= ~ .
             + at(site, c(2,5,6,8,9,10)):lincol
             + at(site, c(3,5,7,8)):linrow
             + at(site, c(2,3,5,7,8,9,11)):serp
             , random = ~ .
             + spl(rate)
             + site:spl(rate)
             + gen:spl(rate)
             + site:gen:spl(rate)
             + at(site, c(3,6,7,9)):rowf
             + at(site, c(1,2,3,9,10)):colf
             + at(site, c(5,7,8,10)):units)
m1 <- update(m1)
m1 <- update(m1)
m1 <- update(m1)

m2 <- update(m1,
             random = ~ .
             - site:gen:spl(rate) - site:gen:dev(rate)
             )

m3 <- update(m2,
             random = ~ .
             - site:dev(rate) - gen:dev(rate))
             
m4 <- update(m3,
             random = ~ .
             - dev(rate))

m5 <- update(m4,
             random = ~ .
             - at(site, c(5,7,8,10)):units + at(site, c(5,7,8)):units)

summary(m5)$varcomp[1:9, 1:3]
anova(m5)
# Figure 8

# Figure 9
p5 <- predict(m5, classify="gen:rate")$predictions$pvals
head(p5)
xyplot(predicted.value ~ rate, data=p5, group=gen, type='smooth',
       auto.key=TRUE)

# Figure 10. Not right
c5 <- coef(m5, pattern="site:gen")
c5site <- unlist(lapply(strsplit(rownames(c5), ":"), function(x) {x[1]}))
c5site <- gsub("site_","",c5site)
c5var <- unlist(lapply(strsplit(rownames(c5), ":"), function(x) {x[2]}))
c5var <- gsub("gen_","",c5var)
c5par <- unlist(lapply(strsplit(rownames(c5), ":"), function(x) {x[3]}))
int <- as.vector(c5)[is.na(c5par)]
slp <- as.vector(c5)[!is.na(c5par)]
dat <- data.frame(site = c5site[is.na(c5par)],
                  var = c5var[is.na(c5par)],
                  int, slp)
xyplot(slp~int, data=dat, group=var, auto.key=TRUE)
