dd <- c(1,14, 2,12,0,12,0,13,3,8,0,9,
        7,8, 0,10,1,14,0,10,0,17,0,10,
        9,14, 1,11,0,10,1,14,0,10,0,21,
        10,17, 0,9,1,12,0,11,0,12,0,10,
        8,11, 1,10,0,9,2,12,0,10,1,11,
        7,9, 2,10,0,10,0,14,1,12,0,13,
        6,12, 0,11,1,13,0,9,2,11,0,10,
        6,7, 1,12,0,9,1,10,0,14,2,12,
        7,13, 0,10,0,10,1,12,0,9,1,8,
        11,15, 1,13,0,14,1,14,0,11,0,11,
        5,11, 5,11,0,15,1,15,0,8,1,10,
        4,9, 1,15,0,11,0,13,1,12,0,12,
        9,12, 2,14,0,12,0,12,0,10,0,13,
        10,10, 0,10,0,8,0,10,1,14,3,11,
        9,11, 0,11,1,11,1,14,0,9,0,9, # block 2
        0,12, 0,12,0,14,0,12,0,10,1,13,
        8,9, 1,9,0,12,0,10,0,12,1,10,
        11,14, 1,12,1,11,0,10,0,9,3,11,
        12,15, 0,15,0,10,1,9,1,10,0,16,
        10,14, 1,9,0,11,0,11,0,11,0,11,
        1,9, 0,9,0,12,1,10,1,12,0,17,
        9,12, 0,12,0,11,2,14,0,11,0,10,
        7,13, 0,16,1,12,0,10,0,10,0,11,
        7,13, 1,18,0,10,0,11,0,11,3,13,
        5,10, 0,10,0,10,0,10,0,9,1,12,
        6,13, 0,10,1,11,3,11,0,12,1,19,
        12,13, 0,11,0,8,0,9,0,17,0,12,
        8,11, 4,12,0,11,0,10,0,15,3,13,
        5,14, 1,9,0,12,1,12,0,10,2,14,
        10,14, 0,14,1,10,1,13,1,15,4,10,
        8,12, 1,14,0,12,0,20,1,18,7,15, # block 3
        9,16, 1,12,0,13,0,15,0,17,1,18,
        7,12, 0,14,1,13,0,18,0,14,0,14,
        10,14, 2,17,0,10,1,19,0,17,0,16,
        9,10, 1,14,1,11,0,18,0,15,1,11,
        10,10, 1,11,0,12,1,15,4,20,0,14,
        9,12, 1,10,1,12,3,18,0,16,0,12,
        10,11, 1,16,1,14,1,17,2,15,1,16,
        9,11, 2,14,0,10,0,18,0,17,0,12,
        11,12, 2,12,0,11,0,13,0,18,0,12,
        7,9, 0,13,0,9,0,18,0,18,0,13,
        6,14, 3,16,1,15,0,17,1,17,3,14,
        10,11, 0,10,1,16,1,18,0,16,0,11,
        10,10, 1,14,0,10,1,19,2,19,2,14,
        8,10, 0,12,0,12,0,18,0,14,0,12)

dat <- NULL
ix <- 1
for(block in c('B1','B2','B3')){
  for(vine in c('V1','V2','V3')){
    for(shoot in c('S1','S2','S3','S4','S5')){
      for(trt in c('T1','T2','T3','T4','T5','T6')){
        m=dd[ix]
        n=dd[ix+1]
        dat <- rbind(dat, c(block,vine,shoot,trt,m,n))
        ix <- ix + 2
      }}}}
dat <- as.data.frame(dat)
names(dat) <- c('block','vine','shoot','trt','diseased','total')
dat <- transform(dat, diseased=fac2num(diseased), total=fac2num(total))
dat <- dat[,c('block','trt','vine','shoot','diseased','total')]

hughes.grapes <- dat
# export(hughes.grapes, "c:/x/rpack/agdata/data/hughes.grapes.txt")
promptData(hughes.grapes, "c:/x/rpack/agdata/man/hughes.grapes.Rd")





# ----------------------------------------------------------------------------

dat <- hughes.grapes

dat <- transform(dat, rate = diseased/total, plot=trt:block)

# Trt 1 has higher rate, more variable, Trt 3 lower rate, less variable
require(latticeExtra)
useOuterStrips(bwplot(rate ~ vine|block*trt, dat))

# Table 1 of Piepho
tapply(dat$rate, dat$trt, mean) # trt 1 does not match Piepho
tapply(dat$rate, dat$trt, max)

# Model 3.  Binomial data

# Switch from counts to bernoulli data
lib(aod)
bdat <- splitbin(cbind(diseased, total-diseased) ~ block+trt+plot+vine+shoot,
                 data=dat)$tab
names(bdat)[2] <- 'y'


# 
m1 <- lmer(y ~ trt + block +
            (1|plot/vine), data=bdat,
            family=binomial)
m1

m2 <- glmmPQL(y ~ trt + block, data=bdat,
              random=~1|plot/vine, family=binomial)
m2


# Below the code is incoherent rambling attempts...
library(asreml)
m2 <- asreml(rate ~ -1 + trt + block, data=dat,
             random = ~ plot/vine,
             family=asreml.binomial(link='logit'))
m2 <- asreml(y ~ -1 + trt + block, data=bdat,
             random = ~ plot + vine + at(trt):shoot,
             family=asreml.binomial(link='logit'))
anova(m2)
vc(m2)

# ----------------------------------------------------------------------------

m2h <- glmmPQL(y ~ trt + block, data=bdat,
               random=~1|plot, family=binomial,
               weights = varIdent(form=~ 1|trt))
