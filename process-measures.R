# Processes all the network measures at once.
#
# setwd("~/eclipse/workspaces/Networks/SystProp")
# setwd("D:/eclipse/workspaces/Networks/SystProp")
#
# source("process-measures.R")
###################################################

#################################
# load dependencies
#################################
library(igraph)

#################################
# setup parameters
#################################
do.cache <- TRUE			# cache table results (series such as degree are always cached)
do.plot <- FALSE			# plot measures
os <- .Platform$OS.type
if(os=="windows")
{	
#	data.folder <- "D:/networks/_cleaned/"
	data.folder <- "H:/networks/_cleaned/"
	folders <- 1:613
#	folders <- c(
#		107,34,160,108,571,166,159,568,111,113,569,112,570,
#		585,168,164,491,167,152,586,538,177,539,581,580,178,
#		583,582,584,576,604,599,531,502,595,530,598,605,606,
#		100,601,557,559,558,607,603,602,536,535,546,600,610,
#		537,562,596,563,597,609,578,593,93,589,591,567,50,564,
#		560,579,608,532,533,561,594,524,556,96,503,588,522,
#		555,574,29,566,114,286,565,90,590,554,115,39,86,523,
#		592,511,575,501,500,52,552,551,553,81,508,587,101,545,
#		529,547,506,573,507,572,509,550,526,525,549,527,510,
#		505,520,162,528,57,95,577,548,118,516,519,542,543,131,
#		544,534,521,517,540,514,91,515,518,4,5,541,513,145,
#		512,142,92,490,51,110,169,30,94,504,31,20,127,106,414,
#		283,3,117,124,499,282,44,123,10,9,8,85,33,84,40,83,97,
#		204,2,73,120,119,496,116,165,121,130,122,128,451,35,46,
#		498,42,89,279,141,87,153,47,179,228,230,132,129,495,180,
#		170,45,37,26,497,88,103,242,136,135,236,38,488,255,415,
#		243,494,49,266,56,174,276,252,271,489,234,137,138,125,
#		161,194,82,12,460,196,226,198,289,48,61,259,356,9,235,
#		156,383,384,477,261,263,459,357,75,139,140,144,270,241,
#		248,105,240,250,60,232,262,258,247,486,251,254,231,349,
#		158,257,268,253,8,264,492,269,32,215,481,134,126,133,171,
#		287,239,181,182,358,475,175,217,233,143,227,238,350,277,
#		212,273,157,249,244,338,493,256,325,326,267,148,7,348,364,
#		347,19,176,208,265,416,15,173,319,260,6,216,346,188,237,
#		245,76,246,272,163,411,433,340,104,378,331,476,352,487,
#		202,147,210,151,155,351,278,1,102,214,229,207,285,77,432,
#		146,13,410,379,353,473,382,354,225,368,195,206,21,288,23,
#		197,199,203,154,78,14,25,213,337,24,339,187,201,11,43,
#		205,80,417,336,79,22,332,189,452,62,63,64,53,280,211,74,
#		403,426,172,485,404,183,69,70,200,402,296,437,360,209,
#		436,361,423,381,467,362,222,463,464,465,281,28,483,363,
#		380,218,413,324,186,366,322,193,184,457,484,221,342,41,
#		224,284,223,292,294,291,67,68,377,36,275,71,185,290,16,
#		367,17,431,27,369,370,371,304,334,391,318,611,65,66,311,
#		312,315,479,313,390,372,482,301,192,299,373,478,321,343,
#		317,109,314,344,345,293,385,220,428,412,59,462,355,453,
#		472,323,333,302,480,341,149,320,376,190,466,307,
#		365,295,425,72,450,455,150,438,439,298,427,447,407,274,359,
#		398,399,400,401,395,421,468,422,406,405,303,434,454,300,397,
#		394,429,430,393,388,335,435,396,469,470,389,297,18,58,420,55,
#		392,458,99,309,449,375,219,444,374,445,316,418,424,443,446,
#		54,408,441,308,471,409,305,386,419,306,442,387,461,329,456,
#		440,191,330,448,327,474,328,310
#		5
#	)
}else
{	data.folder <- "/var/data/networks/_cleaned/"
	#data.folder <- "/media/Samsung/networks/_cleaned/"
	folders <- c(
#		107,34,160,108,571,166,159,568,111,113,569,112,570,
#		585,168,164,491,167,152,586,538,177,539,581,580,178,
#		583,582,584,576,604,599,531,502,595,530,598,605,606,
#		100,601,557,559,558,607,603,602,536,535,546,600,610,
#		537,562,596,563,597,609,578,593,93,589,591,567,50,564,
#		560,579,608,532,533,561,594,524,556,96,503,588,522,
#		555,574,29,566,114,286,565,90,590,554,115,39,86,523,
#		592,511,575,501,500,52,552,551,553,81,508,587,101,545,
#		529,547,506,573,507,572,509,550,526,525,549,527,510,
#		505,520,162,528,57,95,577,548,118,516,519,542,543,131,
#		544,534,521,517,540,514,91,515,518,4,5,541,513,145,
#		512,142,92,490,51,110,169,30,94,504,31,20,127,106,414,
#		283,3,117,124,499,282,44,123,10,9,8,85,33,84,40,83,97,
#		204,2,73,120,119,496,116,165,121,130,122,128,451,35,46,
#		498,42,89,279,141,87,153,47,179,228,230,132,129,495,180,
#		170,45,37,26,497,88,103,242,136,135,236,38,488,255,415,
#		243,494,49,266,56,174,276,252,271,489,234,137,138,125,
#		161,194,82,12,460,196,226,198,289,48,61,259,356,9,235,
#		156,383,384,477,261,263,459,357,75,139,140,144,270,241,
#		248,105,240,250,60,232,262,258,247,486,251,254,231,349,
#		158,257,268,253,8,264,492,269,32,215,481,
		134,126,133,171,
		287,239,181,182,358,475,175,217,233,143,227,238,350,277,
		212,273,157,249,244,338,493,256,325,326,267,148,7,348,364,
		347,19,176,208,265,416,15,173,319,260,6,216,346,188,237,
		245,76,246,272,163,411,433,340,104,378,331,476,352,487,
		202,147,210,151,155,351,278,1,102,214,229,207,285,77,432,
		146,13,410,379,353,473,382,354,225,368,195,206,21,288,23,
		197,199,203,154,78,14,25,213,337,24,339,187,201,11,43,
		205,80,417,336,79,22,332,189,452,62,63,64,53,280,211,74,
		403,426,172,485,404,183,69,70,200,402,296,437,360,209,
		436,361,423,381,467,362,222,463,464,465,281,28,483,363,
		380,218,413,324,186,366,322,193,184,457,484,221,342,41,
		224,284,223,292,294,291,67,68,377,36,275,71,185,290,16,
		367,17,431,27,369,370,371,304,334,391,318,611,65,66,311,
		312,315,479,313,390,372,482,301,192,299,373,478,321,343,
		317,109,314,344,345,293,385,220,428,412,59,462,355,453,
		472,323,333,302,480,341,149,320,376,190,466,307,
		365,295,425,72,450,455,150,438,439,298,427,447,407,274,359,
		398,399,400,401,395,421,468,422,406,405,303,434,454,300,397,
		394,429,430,393,388,335,435,396,469,470,389,297,18,58,420,55,
		392,458,99,309,449,375,219,444,374,445,316,418,424,443,446,
		54,408,441,308,471,409,305,386,419,306,442,387,461,329,456,
		440,191,330,448,327,474,328,310
	)
}
#folders <- folders[!(folders %in% c(308,310,330,418,474))] 				#remove missing multipartite projections
#folders <- folders[!(folders %in% c(4,9,116,163,386,388,420,421,422,469))]	# remove networks with a full density
plot.folder <- paste(data.folder,"plots/",sep="")
missing.folders <- c(182,312,326,399,400,401,439,464,465)
size.limit <- 10^36#1000000000 # only process files whose size is below this limit

#################################
# load measures
#################################
# TODO stdev distance and other distance stats
# TODO stats on the size of communities
# TODO average local value of the neighbors
# TODO hop plots
# TODO p value of the power law distribution test
# TODO stats on embeddedness

# TODO categorize networks depending on : 
#		type of relationships (interaction, hierarchy, etc.) 
#		VS. type of system (biological, artificial, etc.)

measures <- list()
source("measures/check.R")
source("measures/general.R")
source("measures/attribute.R")
source("measures/element.R")
#	source("measures/component.R")
#source("measures/degree.R")
#source("measures/distance.R")
#source("measures/transitivity.R")
#source("measures/betweenness.R")
#source("measures/closeness.R")
#source("measures/edgebetweenness.R")
#source("measures/spectral.R")
##source("measures/connectivity.R")
#source("measures/eccentricity.R")
#source("measures/community.R")


#################################
# init/load data frame
#################################
table.file <- paste(data.folder,"/global.measures.txt",sep="")
prop.names <- names(measures)
if(file.exists(table.file))
{	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Load existing data frame\n",sep="")
	data <- read.table(file=table.file,check.names=FALSE)
	for(p in 1:length(measures))
	{	measure <- measures[[p]]
		if(length(data[[prop.names[p]]])==0)
			data[prop.names[p]] <- NA
	}
}else
{	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Init new data frame\n",sep="")
	data <- data.frame(stringsAsFactors=FALSE)	
	for(p in 1:length(measures))
	{	measure <- measures[[p]]
		data[prop.names[p]] <- measure$type
	}
}
print(data)


#################################
# get existing folder list
#################################
paths <- list.files(path=data.folder, pattern="\\d{4}\\..*", all.files=TRUE, full.names=FALSE, recursive=FALSE, ignore.case = FALSE)
paths <- sort(paths)

#################################
# process measures
#################################
j <- 1
for(f in folders)
{	unavailable <- FALSE
	gc()
	
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
	{	# create cache for this network
		cache <- list()
		
		# setup file name
		start.time <- Sys.time();
		net.folder <- paste(data.folder,filename,"/",sep="")
		data.file <- paste(net.folder,"network.graphml",sep="")
		file.format <- "graphml"
		if(!file.exists(data.file))
		{	data.file <- paste(net.folder,"network.net",sep="")
			file.format <- "pajek"
		}
		
		# network should be ignored
		if(f %in% missing.folders)
		{	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] WARNING: Network folder ",filename," is in the list of missing folders >> ignored \n",sep="")
			unavailable <- TRUE
		# file not found
		}else if(!file.exists(data.file))
		{	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] WARNING: No network file could be found for ",filename,"\n",sep="")
			file.format <- NULL
			unavailable <- TRUE
		# file too large
		}else if(file.info(data.file)$size>size.limit)
		{	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] WARNING: file ",filename," too large, we ignore it for now\n",sep="")
			file.format <- NULL
			unavailable <- TRUE
			
		# normal processing
		}else
		{	# load network
			cat("[",format(start.time,"%a %d %b %Y %X"),"] Loading network #",f,": '",data.file,"'\n",sep="")
			g <- read.graph(data.file,format=file.format)
			end.time <- Sys.time();
			total.time <- end.time - start.time;
			cat("[",format(end.time,"%a %d %b %Y %X"),"] Loading (",vcount(g)," nodes and ",ecount(g)," links) completed in ",format(total.time),"\n",sep="")
			
			# process all required measures
			start.time <- Sys.time();
			cat("[",format(start.time,"%a %d %b %Y %X"),"] Processing measures\n",sep="")
			for(p in 1:length(measures))
			{	measure <- measures[[p]]
				value <- data[as.character(f),prop.names[p]]
				if(do.cache && !is.null(value) && !is.na(value))
					cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ..Measure ",prop.names[p]," (",p,"/",length(measures),") has already been processed before (",data[as.character(f),prop.names[p]],")\n",sep="")
				else
				{	start.time1 <- Sys.time();
					cat("[",format(start.time1,"%a %d %b %Y %X"),"] ..Processing measure ",p,"/",length(measures),": ",prop.names[p],"\n",sep="")
						data[as.character(f),prop.names[p]] <- measure$foo(graph=g)
						if(is.na(data[as.character(f),prop.names[p]]) || is.nan(data[as.character(f),prop.names[p]]))
							data[as.character(f),prop.names[p]] <- Inf
					end.time <- Sys.time();
					total.time <- end.time - start.time1;
					cat("[",format(end.time,"%a %d %b %Y %X"),"] ..Processing completed in ",format(total.time),": ",data[as.character(f),prop.names[p]],"\n",sep="")
					
					# write resulting table
					cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ..Update measure files\n",sep="")
					write.table(x=data, file=table.file)
				}
			}
			end.time <- Sys.time();
			total.time <- end.time - start.time;
			cat("[",format(end.time,"%a %d %b %Y %X"),"] Processing completed in ",format(total.time),"\n",sep="")
		}
	}
	
	# add minimal information for unavailable networks
	if(unavailable)
	{	# update file name and size
		pn <- c("file-name") # "file-size"
		for(propn in pn)
		{	p <- which(prop.names==propn)
			if(length(p)>0)
			{	measure <- measures[[p]]
				data[as.character(f),prop.names[p]] <- measure$foo(graph=g)
			}
		}
		
		# write resulting table
		cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ..Update measure files\n",sep="")
		write.table(x=data, file=table.file)
	}
}

#################################
# plot results
#################################
if(do.plot)
{	if(!file.exists(substr(x=plot.folder, start=1, stop=nchar(plot.folder)-1)))
		dir.create(path=plot.folder)
	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Plot measures\n",sep="")
	for(p1 in 1:(length(measures)-1))
	{	measure1 <- measures[[p1]]
		name1 <- prop.names[p1]
		idx1 <- !is.infinite(data[,name1]) & !is.na(data[,name1]) 
		values1 <- data[idx1,name1]
		
		if(length(values1)>1 && is.numeric(values1))
		{	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ..Plot measure ",name1," as x\n",sep="")
			data <- data[order(data[,name1]),]
			idx1 <- !is.infinite(data[,name1]) & !is.na(data[,name1]) 
			values1 <- data[idx1,name1]
			
			for(p2 in (p1+1):length(measures))
			{	measure2 <- measures[[p2]]
				name2 <- prop.names[p2]
				idx2 <- idx1 & !is.infinite(data[,name2]) & !is.na(data[,name2])
				values2 <- data[idx2,name2]
				values1b <- data[idx2,name1]
				
				if(length(values2)>1 && is.numeric(values2))
				{	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ....Plot measure ",name2," as y\n",sep="")
				
					bounds1 <- measure1$bounds
					if(is.na(bounds1[1]))
						bounds1[1] <- min(values1b)
					if(is.na(bounds1[2]))
						bounds1[2] <- max(values1b)
					
					bounds2 <- measure2$bounds
					if(is.na(bounds2[1]))
						bounds2[1] <- min(values2)
					if(is.na(bounds2[2]))
						bounds2[2] <- max(values2)
					
					plot.file <- paste(plot.folder,name1,".vs.",name2,".png",sep="")
					png(filename=plot.file,width=1000,height=1000,units="px",pointsize=20,bg="white")
					#pdf(file=plot.file,bg="white")
				
					plot(values1b,values2,xlab=name1,ylab=name2,main=paste(name1,"vs",name2),xlim=bounds1,ylim=bounds2)
					dev.off()
				}
				else
				{	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ....WARNING: No usable values for measure ",name2," as x\n",sep="")
				}
			}
		}
		else
		{	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ..WARNING: No usable values for measure ",name1," as x\n",sep="")
		}
	}
}
