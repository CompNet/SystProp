########################################################
# Retrieves the data, fits the power laws
# and alternative distributions, compares
# them. All of that using Clauset et al.'s
# source code from from 
# http://tuvalu.santafe.edu/~aaronc/powerlaws/
# 
# setwd("~/eclipse/workspaces/Networks")
# setwd("c:/eclipse/workspaces/Networks")
#
# source("SystProp/pli/main.R")
########################################################
library("poweRlaw")	# implements partially the functions of Clauset et al.

source("SystProp/pli/discexp.R.R")
source("SystProp/pli/disclnorm.R")
source("SystProp/pli/discpowerexp.R")
source("SystProp/pli/discweib.R")
source("SystProp/pli/exp.R")							# continuous distribution >> not needed
source("SystProp/pli/lnorm.R")							# continuous distribution >> not needed
source("SystProp/pli/pareto.R")							# continuous distribution >> not needed
source("SystProp/pli/poisson.R")
source("SystProp/pli/powerexp.R")						# continuous distribution >> not needed
source("SystProp/pli/powerexp-exponential-integral.R")	# continuous distribution >> not needed
source("SystProp/pli/power-law-test.R")
source("SystProp/pli/weibull.R")						# continuous distribution >> not needed
source("SystProp/pli/yule.R")
source("SystProp/pli/zeta.R")


########################################################
# retrieving data
########################################################
folder <- "temp/"
file.name <- "degree-all"
file.ext <- ".txt"
in.file <- paste(folder,file.name,file.ext,sep="") 
data <- read.table(in.file)

########################################################
# estimating the power law and lower cut-off value
########################################################
# estimate the lower cut-off value
	m <- displ$new(data)
	x.min <- estimate_xmin(m)$xmin
	m$setXmin(x.min)
	alpha = estimate_pars(m)$pars
	m$setPars(alpha)
# estimate the power law exponent
# evaluate the estimated law
	nb.cores <- parallel::detectCores()
	sig <- bootstrap_p(m, no_of_sims=1000, threads=nb.cores)
# init result matrix
	r.names <- c("n","<x>","sd","x_max","^x_min^","^alpha^","n_tail","p")
	pl.results <- matrix(NA, nrow=length(r.names), ncol=1)
	rownames(pl.results) <- r.names
	pl.results["n",1] <- length(data)
	pl.results["<x>",1] <- mean(data)
	pl.results["sd",1] <- sd(data)
	pl.results["x_max",1] <- max(data)
	pl.results["^x_min^",1] <- x.min
	pl.results["^alpha^",1] <- alpha
	pl.results["n_tail",1] <- length(which(data>=x.min))
	pl.results["p",1] <- sig$p
# record those results
	out.file <- paste(folder,file.name,".powerlaw",file.ext,sep="") 
	write.table(x=comp.results, file=out.file, row.names=TRUE, col.names=FALSE)
	

########################################################
# fitting distributions
########################################################
# pure discrete Power law a.k.a. Zipf or Zeta (zeta)
	power.d <- zeta.fit(x=data, threshold=x.min, method="ml.direct") # ml.approx
	# out: type, exponent, method, loglike, threshold, samples.over.threshold
# discrete Power law with exponential cutoff (discpowerexp)
	powerexp.d <- discpowerexp.fit(x=data,threshold=x.min)
	# out: type, exponent, rate, loglike, threshold, samples.over.threshold
# discrete Log-normal distribution (disclnorm)
	lnorm.d <- fit.lnorm.disc(x=data, threshold=x.min)
	# out: type, meanlog, sdlog, loglike, threshold, datapoints.over.threshold
# discrete Exponential distribution (discexp)
	exp.d <- discexp.fit(x=data, threshold=x.min)
	# out: type, lambda, method, loglike, samples.over.threshold, threshold
# discrete Stretched exponential or Weibull distribution (discweib)
	weib.d <- discweib.fit(x=data, threshold=x.min)
	# out: type, shape, scale, loglike, threshold, samples.over.threshold
# Poisson distribution
	pois.d <- pois.tail.fit(x=data, threshold=x.min)
	# out: type, rate, loglike, threshold, samples.over.threshold, full.mean, mean.over.threshold
# Yule-Simon distribution (yule)
	yule.d <- yule.fit(x=data, threshold=x.min)
	# out: type, exponent, loglike, threshold, samples.over.threshold


########################################################
# comparing distributions
########################################################
# init result matrix
	r.names <- c("PowerExp","LogNorm","Exp","StrtExp","Poisson","YuleSimon")
	c.names <- c("LLRatio","pVal")
	comp.results <- matrix(NA,ncollength(c.names),nrow=length(r.names))
	rownames(comp.results) <- r.names
	colnames(comp.results) <- c.names
# pure power law vs. power law with exponential cutoff
	powerexp.res <- power.powerexp.lrt(power.d=power.d, powerexp.d=powerexp.d)
	comp.results["PowerExp","LLRatio"] <- powerexp.res$loglike.ratio
	comp.results["PowerExp","pVal"] <- powerexp.res$p.two.sided
# power law vs. log-normal
	lnorm.res <- vuong(zeta.lnorm.llr(x=data, zeta.d=power.d, lnorm.d=lnorm.d))
	comp.results["LogNorm","LLRatio"] <- lnorm.res$loglike.ratio
	comp.results["LogNorm","pVal"] <- lnorm.res$p.two.sided
# power law vs. exponential
	exp.res <- vuong(zeta.exp.llr(x=data, zeta.d=power.d, exp.d=exp.d))
	comp.results["Exp","LLRatio"] <- exp.res$loglike.ratio
	comp.results["Exp","pVal"] <- exp.res$p.two.sided
# power law vs. stretched exponential
	weib.res <- vuong(zeta.weib.llr(x=data, zeta.d=power.d, weib.d=weib.d))
	comp.results["StrtExp","LLRatio"] <- weib.res$loglike.ratio
	comp.results["StrtExp","pVal"] <- weib.res$p.two.sided
# power law vs. poisson
	pois.res <- vuong(zeta.poisson.llr(x=data, zeta.d=power.d, pois.d=pois.d))
	comp.results["Poisson","LLRatio"] <- pois.res$loglike.ratio
	comp.results["Poisson","pVal"] <- pois.res$p.two.sided
# power law vs. yule
	yule.res <- vuong(zeta.yule.llr(x=data, zeta.d=power.d, yule.d=yule.d))
	comp.results["YuleSimon","LLRatio"] <- yule.res$loglike.ratio
	comp.results["YuleSimon","pVal"] <- yule.res$p.two.sided
# record comparison results
	out.file <- paste(folder,file.name,".comparisons",file.ext,sep="") 
	write.table(x=comp.results, file=out.file, row.names=TRUE, col.names=TRUE)
	
	
########################################################
########################################################
	

