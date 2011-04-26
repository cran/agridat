# gomez.groupsplit.r
# Time-stamp: <05 Apr 2011 11:33:00 c:/x/rpack/agdata/raw/gomez.groupsplit.r>

library(asreml)
library(kw)
library(Hmisc)
library(lattice)
setwd("c:/x/rpack/agdata/")

dat <- import("raw/gomez.groupsplit.xls", sheet='yield')

gdat <- import("raw/gomez.groupsplit.xls", sheet='gen')
gdat <- as.matrix(gdat)
colnames(gdat) <- 1:9
gdat <- melt(gdat)
names(gdat) <- c('y','x','gen')

rdat <- import("raw/gomez.groupsplit.xls", sheet='rep')
rdat <- as.matrix(rdat)
colnames(rdat) <- 1:9
rdat <- melt(rdat)
names(rdat) <- c('y','x','rep')


fdat <- import("raw/gomez.groupsplit.xls", sheet='fert')
fdat <- as.matrix(fdat)
colnames(fdat) <- 1:9
fdat <- melt(fdat)
names(fdat) <- c('y','x','fert')

all <- merge(gdat, rdat, by=c('x','y'))
all <- merge(all, fdat, by=c('x','y'))
all$gen <- paste("G",formatC(all$gen, width=2, flag='0'),sep="")

all2 <- merge(dat, all, by=c('gen','rep','fert'))
all2$group <- all2$gen
levels(all2$group) <- rep(c('S1','S2','S3'), each=15)

dat <- all2[,c('x','y','rep','fert','gen','group','yield')]

dat$y <- 31-dat$y

with(dat, table(group,gen))
desplot(yield~x*y, dat, outline1=rep, outline2=group)

gomez.groupsplit <- dat
# ----------------------------------------------------------------------------


zeros <- rep(0,15)
ones <- rep(1,15)
cm.gen <- rbind("S1"=c(ones,zeros,zeros),
                "S2"=c(zeros,ones,zeros),
                "S3"=c(zeros,zeros,ones))

