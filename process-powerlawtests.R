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
# source("SystProp/process-powerlawtests.R")
########################################################

#################################
# load dependencies
#################################
library("poweRlaw")	# implements partially the functions of Clauset et al.

source("SystProp/pli/discexp.R")
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


#################################
# setup parameters
#################################
do.cache <- FALSE			# cache table results (series such as degree are always cached)
do.plot <- FALSE			# plot measures
os <- .Platform$OS.type
if(os=="windows")
{	data.folder <- "M:/networks/_cleaned/"
#	data.folder <- "c:/Temp/"
	folders <- 1:611
}else
{	#data.folder <- "/var/data/networks/"
	data.folder <- "/media/Samsung/networks/_cleaned/"
	folders <- c(
		107,34,160,108,571,166,159,568,111,113,569,112,570,
		585,168,164,491,167,152,586,538,177,539,581,580,178,
		583,582,584,576,604,599,531,502,595,530,598,605,606,
		100,601,557,559,558,607,603,602,536,535,546,600,610,
		537,562,596,563,597,609,578,593,93,589,591,567,50,564,
		560,579,608,532,533,561,594,524,556,96,503,588,522,
		555,574,29,566,114,286,565,90,590,554,115,39,86,523,
		592,511,575,501,500,52,552,551,553,81,508,587,101,545,
		529,547,506,573,507,572,509,550,526,525,549,527,510,
		505,520,162,528,57,95,577,548,118,516,519,542,543,131,
		544,534,521,517,540,514,91,515,518,4,5,541,513,145,
		512,142,92,490,51,110,169,30,94,504,31,20,127,106,414,
		283,3,117,124,499,282,44,123,10,9,8,85,33,84,40,83,97,
		204,2,73,120,119,496,116,165,121,130,122,128,451,35,46,
		498,42,89,279,141,87,153,47,179,228,230,132,129,495,180,
		170,45,37,26,497,88,103,242,136,135,236,38,488,255,415,
		243,494,49,266,56,174,276,252,271,489,234,137,138,125,
		161,194,82,12,460,196,226,198,289,48,61,259,356,9,235,
		156,383,384,477,261,263,459,357,75,139,140,144,270,241,
		248,105,240,250,60,232,262,258,247,486,251,254,231,349,
		158,257,268,253,8,264,492,269,32,215,481,134,126,133,171,
		287,239,181,182,358,475,175,217,233,143,227,238,350,277,
		212,273,157,249,244,338,493,256,325,326,267,148,7,348,364,
		347,19,176,208,265,416,15,173,319,260,6,216,346,188,237,
		245,76,246,272,163,411,433,340,104,378,331,476,352,487,
		202,147,210,151,155,351,278,1,102,214,229,207,285,77,432,
		146,13,410,
		379,353,473,382,354,225,368,
		195,206,21,288,23,
		197,199,203,154,78,14,25,213,337,24,339,187,201,11,43,
		205,80,417,336,79,22,332,189,452,62,63,64,53,280,211,74,
		403,426,172,485,404,183,69,70,200,402,296,437,360,209,
		436,361,423,381,467,362,222,463,464,465,281,28,483,363,
		380,218,413,324,186,366,322,193,184,457,484,221,342,41,
		224,284,223,292,
		294,291,67,68,377,
		36,275,71,185,290,16,367,17,431,27,369,370,371,
		304,334,391,318,611,65,66,
		311,312,315,
		479,313,390,372,482,301,192,299,
		373,478,321,343,317,109,314,344,345,293,385,220,428,412,59,
		462,355,453,472,323,
		333,302,480,341,149,320,376,190,466,307,
		365,295,425,72,450,455,150,438,439,298,427,447,407,274,359,
		398,399,400,401,395,421,468,422,406,405,303,434,454,300,397,
		394,429,430,393,388,335,435,396,469,470,389,297,18,58,420,55,
		392,458,99,309,449,375,219,444,374,445,316,418,424,443,446,
		54,408,441,308,471,409,305,386,419,306,442,387,461,329,456,
		440,191,330,448,327,474,328,310
	)
}
folders <- folders[!(folders %in% c(308,310,330,418,474))] 				#remove missing multipartite projections
file.name <- "degree-all"
file.ext <- ".txt"


#################################
# get existing folder list
#################################
paths <- list.files(path=data.folder, pattern="\\d{4}\\..*", all.files=TRUE, full.names=FALSE, recursive=FALSE, ignore.case = FALSE)
paths <- sort(paths)


for(f in folders)
{	gc()
	
	# check for file name existence
	filename <- NA
	prefix0 <- sprintf("%04d",f)
	i <- 1
	while(is.na(filename) && i<=length(paths))
	{	prefix1 <- substr(paths[i], 1, 4)
		if(prefix0==prefix1)
			filename <- paths[i]
		i <- i + 1
	}
	
	# no network available
	if(is.na(filename))
	{	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] WARNING: No network folder could be found for ",filename,"\n",sep="")
		unavailable <- TRUE
		
	# network available
	}else
	{	# setup file name
		start.time <- Sys.time();
		net.folder <- paste(data.folder,filename,"/",sep="")
		data.file <- paste(net.folder,file.name,file.ext,sep="")
		
		# file not found
		if(!file.exists(data.file))
		{	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] WARNING: No data file could be found for ",filename,"\n",sep="")
			unavailable <- TRUE
			
		# normal processing
		}else
		{	# load network
			cat("[",format(start.time,"%a %d %b %Y %X"),"] Loading data file #",f,": '",data.file,"'\n",sep="")
			data <- as.vector(read.table(data.file)[,1])
			end.time <- Sys.time();
			total.time <- end.time - start.time;
			cat("[",format(end.time,"%a %d %b %Y %X"),"] Loading completed in ",format(total.time),"\n",sep="")
			
			# process stats
			start.time <- Sys.time();
			cat("[",format(start.time,"%a %d %b %Y %X"),"] Processing stats\n",sep="")
			########################################################
			# estimating the power law and lower cut-off value
			########################################################
			# estimate the lower cut-off value
				cat("Estimating lower cut-off value\n")
				m <- displ$new(data)
				x.min <- estimate_xmin(m)$xmin
				m$setXmin(x.min)
			# estimate the power law exponent
				cat("Estimating power law exponent\n")
				alpha = estimate_pars(m)$pars
				m$setPars(alpha)
			# evaluate the estimated law
				cat("Evaluating the estimated law\n")
				nb.cores <- parallel::detectCores()
				sig <- bootstrap_p(m, no_of_sims=10, threads=nb.cores) #TODO
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
				out.file <- paste(net.folder,file.name,".powerlaw",file.ext,sep="") 
				write.table(x=pl.results, file=out.file, row.names=TRUE, col.names=FALSE)
			
			########################################################
			# fitting distributions
			########################################################
			# pure discrete Power law a.k.a. Zipf or Zeta (zeta)
				cat("Fitting discrete power law\n")
				power.d <- zeta.fit(x=data, threshold=x.min, method="ml.direct") # ml.approx
				# out: type, exponent, method, loglike, threshold, samples.over.threshold
			# discrete Power law with exponential cutoff (discpowerexp)
				cat("Fitting discrete power law with exponential cut-off\n")
				powerexp.d <- discpowerexp.fit(x=data,threshold=x.min)
				# out: type, exponent, rate, loglike, threshold, samples.over.threshold
			# discrete Log-normal distribution (disclnorm)
				cat("Fitting log-normal distribution\n")
				lnorm.d <- fit.lnorm.disc(x=data, threshold=x.min)
				# out: type, meanlog, sdlog, loglike, threshold, datapoints.over.threshold
			# discrete Exponential distribution (discexp)
				cat("Fitting exponential distribution\n")
				exp.d <- discexp.fit(x=data, threshold=x.min)
				# out: type, lambda, method, loglike, samples.over.threshold, threshold
			# discrete Stretched exponential or Weibull distribution (discweib)
				cat("Fitting stretched exponential distribution\n")
				weib.d <- discweib.fit(x=data, threshold=x.min)
				# out: type, shape, scale, loglike, threshold, samples.over.threshold
			# Poisson distribution
				cat("Fitting poisson distribution\n")
				pois.d <- pois.tail.fit(x=data, threshold=x.min)
				# out: type, rate, loglike, threshold, samples.over.threshold, full.mean, mean.over.threshold
			# Yule-Simon distribution (yule)
				cat("Fitting yule-simon distribution\n")
				yule.d <- yule.fit(x=data, threshold=x.min)
				# out: type, exponent, loglike, threshold, samples.over.threshold
			
			########################################################
			# comparing distributions
			########################################################
			# init result matrix
				r.names <- c("PowerExp","LogNorm","Exp","StrtExp","Poisson","YuleSimon")
				c.names <- c("LLRatio","p1Val","p2Val")
				comp.results <- matrix(NA,ncol=length(c.names),nrow=length(r.names))
				rownames(comp.results) <- r.names
				colnames(comp.results) <- c.names
			# pure power law vs. power law with exponential cutoff
				cat("Comparing power law vs. power law with exponential cutoff\n")
				powerexp.res <- power.powerexp.lrt(power.d=power.d, powerexp.d=powerexp.d)
				comp.results["PowerExp","LLRatio"] <- powerexp.res$log.like.ratio
				comp.results["PowerExp","p1Val"] <- powerexp.res$p_value
				comp.results["PowerExp","p2Val"] <- NA
			# power law vs. log-normal
				cat("Comparing power law vs. log-normal distribution\n")
				lnorm.res <- vuong(zeta.lnorm.llr(x=data, zeta.d=power.d, lnorm.d=lnorm.d))
				comp.results["LogNorm","LLRatio"] <- lnorm.res$loglike.ratio
				comp.results["LogNorm","p1Val"] <- lnorm.res$p.one.sided
				comp.results["LogNorm","p2Val"] <- lnorm.res$p.two.sided
			# power law vs. exponential
				cat("Comparing power law vs. exponential distribution\n")
				exp.res <- vuong(zeta.exp.llr(x=data, zeta.d=power.d, exp.d=exp.d))
				comp.results["Exp","LLRatio"] <- exp.res$loglike.ratio
				comp.results["Exp","p1Val"] <- exp.res$p.one.sided
				comp.results["Exp","p2Val"] <- exp.res$p.two.sided
			# power law vs. stretched exponential
				cat("Comparing power law vs. stretched exponential distribution\n")
				weib.res <- vuong(zeta.weib.llr(x=data, zeta.d=power.d, weib.d=weib.d))
				comp.results["StrtExp","LLRatio"] <- weib.res$loglike.ratio
				comp.results["StrtExp","p1Val"] <- weib.res$p.one.sided
				comp.results["StrtExp","p2Val"] <- weib.res$p.two.sided
			# power law vs. poisson
				cat("Comparing power law vs. poisson distribution\n")
				pois.res <- vuong(zeta.poisson.llr(x=data, zeta.d=power.d, pois.d=pois.d))
				comp.results["Poisson","LLRatio"] <- pois.res$loglike.ratio
				comp.results["Poisson","p1Val"] <- pois.res$p.one.sided
				comp.results["Poisson","p2Val"] <- pois.res$p.two.sided
			# power law vs. yule
				cat("Comparing power law vs. yule-simon distribution\n")
				yule.res <- vuong(zeta.yule.llr(x=data, zeta.d=power.d, yule.d=yule.d))
				comp.results["YuleSimon","LLRatio"] <- yule.res$loglike.ratio
				comp.results["YuleSimon","p1Val"] <- yule.res$p.one.sided
				comp.results["YuleSimon","p2Val"] <- yule.res$p.two.sided
			# record comparison results
				cat("Recording results\n")
				out.file <- paste(net.folder,file.name,".comparisons",file.ext,sep="") 
				write.table(x=comp.results, file=out.file, row.names=TRUE, col.names=TRUE)
			
			end.time <- Sys.time();
			total.time <- end.time - start.time;
			cat("[",format(end.time,"%a %d %b %Y %X"),"] Processing completed in ",format(total.time),"\n",sep="")
		}
	}
}


cat("All done\n")
########################################################
########################################################
