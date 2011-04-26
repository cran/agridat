# verbyla.cow.r
# Time-stamp: <18 Jan 2011 13:44:04 c:/x/rpack/agdata/raw/verbyla.cow.r>

library(asreml)
library(kw)
library(Hmisc)
library(lattice)
setwd("c:/x/rpack/agdata/raw/")
dat <- import("verbyla.cow.csv")
dat <- transform(dat, rep=factor(rep))
dat$days <- dat$ctime*10 + 122
str(dat)
describe(dat)

# Figure 1 of Verbyla 1999
require(latticeExtra)
foo <- xyplot(y~days|iron*infect, dat, type=c('b'),group=animal)
print(useOuterStrips(foo))


# Table 4 & 6 -- doesn't work, I don't think this is quite right
require(asreml)
m1 <- asreml(y ~ 1 + lin(time) + infect + infect:lin(time)
             # + noninfected:iron:lin(time3)
             # + infected:iron:lin(time3)
             , data = dat,
             , random = ~ spl(time) + dev(time)
             + str(~animal + animal:time, ~us(2):id(26))
             # + noninfected:iron:spl(time3) + noninfected:iron:dev(time3)
             # + infected:iron:spl(time3) + infected:iron:dev(time3)
             + animal:spl(time)
             )
#m1 <- update(m1)
vc(m1)
# ----------------------------------------------------------------------------

y live weights (kilograms)
trt the 2x2 factorial treatments 1,2,3,4
time time as a factor with 23 levels
animal cow as a factor with 26 levels
ctime (actual time-122)/10
x centred (zero mean) and scaled (divided by 100) times
infect infection factor with 2 levels
iron iron factor with 2 levels
time3 actual time - 155, with first two values set to zero
i3 indicator for iron application (0 for times 1 and 2, 1 otherwise)
rtime3 as for time3 BUT with NA or missing for times 1 and 2

There are 598 lines of data and NA stand for missing data.

Source:

Diggle, P. J., Liang, K.-Y., & Zeger, S. L. (1994).
Analysisof Longitudinal Data. Page 100-101.

@article{lepper1989effects,
  title={{Effects of altered dietary iron intake in Mycobacterium paratuberculosis-mfecteA dairy cattle: sequential observations on growth, iron and copper metabolism and development of paratuberculosis}},
  author={LEPPER, AWD and LEWIS, VM},
  journal={Research in veterinary science},
  volume={46},
  pages={289--296},
  year={1989},
  publisher={British Veterinary Association}
}

Twenty calves were orally inoculated with Mycobacterium paratuberculosis at six weeks old. At six months old, 10 of these, plus four uninfected controls were maintained on limited dietary copper and supplemented iron intake for a further 27 months. During this time all these animals, together with a further four untreated controls, were bred before being killed and examined for evidence of paratuberculosis. Despite significant reduction in weight gain, attributable to both iron supplementation and infection, no significant difference was found in the numbers of iron-supplemented and unsupplemented animals that developed clinical signs nor in the extent and severity of intestinal lesions between groups. Accumulation of iron in paratuberculosis lesions was not affected by iron supplementation but was positively correlated with the frequency of shedding of M paratuberculosis in faeces (P less than 0.05). Dietary iron supplementation alone resulted in serum hyperferraemia, hepatic siderosis and slight hypocuprosis, whereas, in infected animals, this resulted in marked hypocuprosis and anaemia within groups (P less than 0.05). Infection alone resulted in serum hypoferraemia and intestinal and hepatic siderosis which was positively correlated with the severity of infection within groups (P less than 0.05). Susceptibility to paratuberculosis may result from failure ultimately to limit monokine-mediated iron sequestration in intestinal tissue.

Diggle 1994
Diggle 1988
Cullis 1990
Verbyla 1992

Verbyla 1997
Diggle et al., 1994, pp. 100-101, consider an experiment giving rise to longitudinal data.
The aim of the experiment was to study how iron dosing, a factor at two levels (no dosing
and a standard amount) and how presence or absence of the organism M.paratuberculosis,
influence the liveweights of cows.  Twenty-eight cows were allocated to treatments in
the 2 by 2 factorial design implied by these factors, with 4, 4, 10 and 10 cows for the 4
treatment combinations; in fact one cow died during the study and data on another was
removed on advice from the researcher, so that the final sizes were 4, 3, 9 and 10 in the
data analysed. The liveweights of the cows were measured at 23 unequally spaced time
int

SAS/STAT(R) 9.2 User's Guide, Second Edition
