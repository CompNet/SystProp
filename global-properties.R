# setwd("~/eclipse/workspaces/Networks")
# setwd("c:/eclipse/workspaces/Networks")
#
# source("SystProp/global-properties.R")
###################################################

#################################
# load dependencies
#################################
library(igraph)

#################################
# setup parameters
#################################
do.cache <- FALSE			# cache table results (series such as degree are always cached)
do.plot <- FALSE			# plot properties
do.normalize <- TRUE		# collapse multiple links, project bipartite graph, etc.
os <- .Platform$OS.type
if(os=="windows")
{	data.folder <- "f:/networks/"
#	data.folder <- "c:/Temp/"
#	folders <- 1:5
	# all possible folders
	folders <- 1:502
	# remove missing files
#	folders <- folders[!(folders %in% c(34,36,41,43,53,54,55,59,74,99,190,191,192,193))]
	# remove large files
#	folders <- folders[!(folders %in% c(18,54:55,58,71:72,99,109,149:150,182,190:192,200,218:221,274:275,293:294:296,298:305,307:318,320,323,326:330,332:333,335,341:343:345,358:359,365,367,369,371:372,374:377,385:387:401,405,406,408,409,412,413,418,419,427,429:431,434:435,438:450,453:456,458,461,463:467,470,472,474))]
}else
{	data.folder <- "/var/data/networks/"
	# all possible folders
	folders <- 1:502
	# remove missing files
	folders <- folders[!(folders %in% c())]
	# remove large files
#	folders <- folders[!(folders %in% c(18,54:55,58,71:72,99,109,149:150,182,190:192,200,218:221,274:275,293:294:296,298:305,307:318,320,323,326:330,332:333,335,341:343:345,358:359,365,367,369,371:372,374:377,385:387:401,405,406,408,409,412,413,418,419,427,429:431,434:435,438:450,453:456,458,461,463:467,470,472,474))]
}
plot.folder <- paste(data.folder,"plots/",sep="")


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
# TODO process only basic properties for all network "as is", and for detailed properties, focus on undirected, selected networks (unipartite projection, multiplex, etc)
# TODO check for 'weights' instead of 'weight'

# TODO design two scripts: a regular one, and another one focusing on undirected, unweighted, etc., network
# 		>loading script allowing to retrieve the appropriate version of a network?
#		>or is it better to use some case-by-case in this script?
#		>> remove isolates
properties <- list()
source("SystProp/global-properties-check.R")
source("SystProp/global-properties-general.R")
source("SystProp/global-properties-attribute.R")
#	source("SystProp/global-properties-element.R")
#	source("SystProp/global-properties-component.R")
#	source("SystProp/global-properties-degree.R")
#source("SystProp/global-properties-distance.R")
#	source("SystProp/global-properties-transitivity.R")
#source("SystProp/global-properties-betweenness.R")
#source("SystProp/global-properties-closeness.R")
#source("SystProp/global-properties-edgebetweenness.R")
#source("SystProp/global-properties-spectral.R")
##source("SystProp/global-properties-connectivity.R")
#source("SystProp/global-properties-eccentricity.R")
#source("SystProp/global-properties-community.R")


#################################
# init/load data frame
#################################
table.file <- paste(data.folder,"/global.properties.txt",sep="")
prop.names <- names(properties)
if(file.exists(table.file))
{	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Load existing data frame\n",sep="")
	data <- read.table(file=table.file,check.names=FALSE)
	for(p in 1:length(properties))
	{	property <- properties[[p]]
		if(length(data[[prop.names[p]]])==0)
			data[prop.names[p]] <- NA
	}
}else
{	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Init new data frame\n",sep="")
	data <- data.frame(stringsAsFactors=FALSE)	
	for(p in 1:length(properties))
	{	property <- properties[[p]]
		data[prop.names[p]] <- property$type
	}
}
print(data)
#################################
# get existing folder list
#################################
paths <- list.files(path=data.folder, pattern="\\d{4}\\..*", all.files=TRUE, full.names=FALSE, recursive=FALSE, ignore.case = FALSE)
paths <- sort(paths)

#################################
# process properties
#################################
j <- 1
for(f in folders)
{	# check for file name existence
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
		cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] WARNING: No network could be found for ",filename,"\n",sep="")
	
	# network available
	else
	{	# create cache for this network
		cache <- list()
		
		# load network
		start.time <- Sys.time();
		net.folder <- paste(data.folder,filename,"/",sep="")
		data.file <- paste(net.folder,"network.graphml",sep="")
		format <- "graphml"
		if(!file.exists(data.file))
		{	data.file <- paste(net.folder,"network.net",sep="")
			format <- "pajek"
		}
		if(!file.exists(data.file))
		{	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] WARNING: No network file could be found for ",filename,"\n",sep="")
			format <- NULL
		}else
		{	cat("[",format(start.time,"%a %d %b %Y %X"),"] Loading network #",f,": '",data.file,"'\n",sep="")
			g <- read.graph(data.file,format=format)
			if(length(E(g)$weight)>0)
				g <- remove.edge.attribute(graph=g, name="weight")
			end.time <- Sys.time();
			total.time <- end.time - start.time;
			cat("[",format(end.time,"%a %d %b %Y %X"),"] Loading (",vcount(g)," nodes and ",ecount(g)," links) completed in ",total.time,"\n",sep="")
			
			# normalize network
			if(do.normalize)
			{	
				
			}
			
			# process all required properties
			start.time0 <- Sys.time();
			cat("[",format(start.time0,"%a %d %b %Y %X"),"] Processing properties\n",sep="")
			for(p in 1:length(properties))
			{	property <- properties[[p]]
				value <- data[as.character(f),prop.names[p]]
				if(do.cache && !is.null(value) && !is.na(value))
					cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ..Property ",prop.names[p]," (",p,"/",length(properties),") has already been processed before (",data[as.character(f),prop.names[p]],")\n",sep="")
				else
				{	start.time <- Sys.time();
					cat("[",format(start.time,"%a %d %b %Y %X"),"] ..Processing property ",p,"/",length(properties),": ",prop.names[p],"\n",sep="")
						data[as.character(f),prop.names[p]] <- property$foo(graph=g)
						if(is.na(data[as.character(f),prop.names[p]]) || is.nan(data[as.character(f),prop.names[p]]))
							data[as.character(f),prop.names[p]] <- Inf
					end.time <- Sys.time();
					total.time <- end.time - start.time;
					cat("[",format(end.time,"%a %d %b %Y %X"),"] ..Processing completed in ",total.time,": ",data[as.character(f),prop.names[p]],"\n",sep="")
					
					# write resulting table
					cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ..Update property files\n",sep="")
					write.table(x=data, file=table.file)
				}
			}
			end.time0 <- Sys.time();
			total.time0 <- end.time0 - start.time0;
			cat("[",format(end.time0,"%a %d %b %Y %X"),"] Processing completed in ",total.time0,"\n",sep="")
		}
	}
}

#################################
# plot results
#################################
if(do.plot)
{	if(!file.exists(substr(x=plot.folder, start=1, stop=nchar(plot.folder)-1)))
		dir.create(path=plot.folder)
	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Plot properties\n",sep="")
	for(p1 in 1:(length(properties)-1))
	{	property1 <- properties[[p1]]
		name1 <- prop.names[p1]
		idx1 <- !is.infinite(data[,name1]) & !is.na(data[,name1]) 
		values1 <- data[idx1,name1]
		
		if(length(values1)>1 && is.numeric(values1))
		{	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ..Plot property ",name1," as x\n",sep="")
			data <- data[order(data[,name1]),]
			idx1 <- !is.infinite(data[,name1]) & !is.na(data[,name1]) 
			values1 <- data[idx1,name1]
			
			for(p2 in (p1+1):length(properties))
			{	property2 <- properties[[p2]]
				name2 <- prop.names[p2]
				idx2 <- idx1 & !is.infinite(data[,name2]) & !is.na(data[,name2])
				values2 <- data[idx2,name2]
				values1b <- data[idx2,name1]
				
				if(length(values2)>1 && is.numeric(values2))
				{	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ....Plot property ",name2," as y\n",sep="")
				
					bounds1 <- property1$bounds
					if(is.na(bounds1[1]))
						bounds1[1] <- min(values1b)
					if(is.na(bounds1[2]))
						bounds1[2] <- max(values1b)
					
					bounds2 <- property2$bounds
					if(is.na(bounds2[1]))
						bounds2[1] <- min(values2)
					if(is.na(bounds2[2]))
						bounds2[2] <- max(values2)
					
					plot.file <- paste(plot.folder,name1,".vs.",name2,".png",sep="")
					png(filename=plot.file,width=1000,height=1000,units="px",pointsize=20,bg="white")
					#pdf(file=plot.file,bg="white")
				
					plot(values1b,values2,xlab=name1,ylab=name2,main=paste(name1,"vs",name2),xlim=bounds1, ylim=bounds2)
					dev.off()
				}
				else
				{	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ....WARNING: No usable values for property ",name2," as x\n",sep="")
				}
			}
		}
		else
		{	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ..WARNING: No usable values for property ",name1," as x\n",sep="")
		}
	}
}
