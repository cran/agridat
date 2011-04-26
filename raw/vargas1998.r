# Vargas1998.r
# Time-stamp: <09 Apr 2011 21:24:52 c:/x/rpack/agridat/raw/vargas1998.r>

setwd("c:/x/rpack/agridat")
yield <- read.csv("raw/vargas1998b_y.csv", row.names=1)
yield <- as.matrix(yield)
covs <- read.csv("raw/vargas1998b_x.csv", row.names=1)
covs <- as.matrix(covs)
vargas.wheat2 <- list(yield=yield, covs=covs)

export(vargas.wheat2, file="data/vargas.wheat2.rda")
#promptData(vargas.wheat2, "man/vargas.wheat2.Rd")
# ----------------------------------------------------------------------------

dat <- vargas.wheat2
yield <- dat$yield
covs <- dat$covs

# The pls package centers, but does not (by default) use scaled covariates
# Vargas says you should
# yield <- scale(yield)
covs <- scale(covs)

require(pls)
m2 <- plsr(yield ~ covs)

# Plot predicted vs observed for each genotype using all components
plot(m2)

# Loadings
plot(m2, "loadings", xaxt='n')
axis(1, at=1:ncol(covs), labels=colnames(covs), las=2)

biplot(m2, cex=.5, which="y", var.axes=TRUE) # Vargas figure 2a
biplot(m2, cex=.5, which="x", var.axes=TRUE) # Vectors form figure 2 b
biplot(m2, cex=.5, which="scores", var.axes=TRUE)
biplot(m2, cex=.5, which="loadings", var.axes=TRUE)

# ----------------------------------------------------------------------------
# Wheat 1

setwd("c:/x/rpack/agridat")
dgen <- import("raw/vargas.wheat1.xls",sheet="gen")
denv <- import("raw/vargas.wheat1.xls",sheet="env")
names(denv) <- gsub("1","",names(denv))
dgen$rep <- paste("R", dgen$rep, sep="")
dgen$gen <- paste("G", dgen$gen, sep="")

vargas.wheat1 <- list(genvals=dgen, envvals=denv)
export(vargas.wheat1, "data/vargas.wheat1.rda")
#promptData(vargas.wheat1, "data/vargas.wheat1.Rd")
