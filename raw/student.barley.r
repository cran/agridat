# student.barley.r
# Time-stamp: <07 Apr 2011 21:43:13 c:/x/rpack/agridat/raw/student.barley.r>

library(asreml)
library(kw)
library(Hmisc)
library(lattice)
setwd("c:/x/rpack/agridat/")

dat <- import("raw/student.barley.xls","large barley")

summary(dat$ArcherShillings)
sd(dat$ArcherShillings)
summary(dat$GoldthorpeShillings)
sd(dat$GoldthorpeShillings)
sqrt(var(dat$ArcherShillings)/51+var(dat$GoldthorpeShillings)/51)

sqrt(var(dat$ArcherShillings-dat$GoldthorpeShillings)/51)

# Make it tall

dat <- dat[,c('year','farmer','place','district','ArcherStones','ArcherShillings','GoldthorpeStones','GoldthorpeShillings')]

d1 <- dat[,c(1:6)]
d2 <- dat[,c(1:4,7:8)]
names(d1) <- names(d2) <- c("year","farmer","place","district","yield","income")
d1$gen <- "Archer"
d2$gen <- "Goldthorpe"

dat <- rbind(d1,d2)
dat <- transform(dat, gen=factor(gen))
dat <- dat[,c("year","farmer","place","district","gen","yield","income")]

student.barley <- dat
export(student.barley, "data/student.barley.txt")
#promptData(student.barley, file="../man/student.barley.Rd")
