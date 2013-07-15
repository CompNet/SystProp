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
do.cache <- TRUE
os <- .Platform$OS.type
if(os=="windows")
{	data.folder <- "c:/Temp/"
	folders <- 1:5
}else
{	data.folder <- "/var/data/networks/"
#	folders <- c(1,10,100,101,106,107,108,109,110,112,113,114,115,119,123,124,125,126,127,128,129,13,130,131,132,133,134,135,136,137,138,139,14,140,146,147,148,15,152,153,154,155,159,166,167,168,17,171,172,173,174,175,178,179,180,181,182,2,20,201,202,203,204,205,206,207,208,209,21,210,211,212,213,214,215,216,227,228,229,23,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,26,29,3,30,31,32,33,34,35,36,38,41,43,44,45,46,47,48,49,5,50,51,52,53,54,55,6,7,73,75,76,77,78,79,8,80,81,82,83,84,9,90,91,92,93,94,95,96,97,98,99)
	folders <- c(1:17, 19:57, 59:71, 73, 75:98)	#1:297
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
# TODO whether the graph is bipartite or not
# TODO multiplex, attributed...
properties <- list()
source("SystProp/global-properties-general.R")
source("SystProp/global-properties-element.R")
source("SystProp/global-properties-component.R")
source("SystProp/global-properties-degree.R")
source("SystProp/global-properties-distance.R")
source("SystProp/global-properties-transitivity.R")
source("SystProp/global-properties-betweenness.R")
source("SystProp/global-properties-closeness.R")
source("SystProp/global-properties-edgebetweenness.R")
source("SystProp/global-properties-spectral.R")
#source("SystProp/global-properties-connectivity.R")
#source("SystProp/global-properties-eccentricity.R")
source("SystProp/global-properties-community.R")


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
		cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] No network could be found for ",filename,"\n",sep="")
	
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
		cat("[",format(start.time,"%a %d %b %Y %X"),"] Loading network #",f,": '",data.file,"'\n",sep="")
		g <- read.graph(data.file,format=format)
		if(length(E(g)$weight)>0)
			g <- remove.edge.attribute(graph=g, name="weight")
		end.time <- Sys.time();
		total.time <- end.time - start.time;
		cat("[",format(end.time,"%a %d %b %Y %X"),"] Loading (",vcount(g)," nodes)completed in ",total.time,"\n",sep="")
		
		# process all required properties
		start.time0 <- Sys.time();
		cat("[",format(start.time0,"%a %d %b %Y %X"),"] Processing properties\n",sep="")
		for(p in 1:length(properties))
		{	property <- properties[[p]]
			value <- data[as.character(f),prop.names[p]]
			if(do.cache && !is.null(value) && !is.na(value))
				cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ..Property ",prop.names[p]," (",p,"/",length(properties),") has already been processed before (",data[as.character(f),prop.names[p]],")\n",sep="")
			else{
				start.time <- Sys.time();
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

#################################
# plot results
#################################
cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Plot properties\n",sep="")
for(p1 in 1:(length(properties)-1))
{	property1 <- properties[[p1]]
	name1 <- prop.names[p1]
	values1 <- data[,name1]
	
	if(is.numeric(values1) && !all(is.na(values1)))
	{	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ..Plot property ",name1," as x\n",sep="")
		data <- data[order(data[,name1]),]
		
		bounds1 <- property1$bounds
		if(is.na(bounds1[1]))
			bounds1[1] <- min(values1,na.rm=TRUE)
		if(is.na(bounds1[2]))
			bounds1[2] <- max(values1,na.rm=TRUE)
		
		for(p2 in (p1+1):length(properties))
		{	property2 <- properties[[p2]]
			name2 <- prop.names[p2]
			values2 <- data[,name2]
			
			if(is.numeric(values2) && !all(is.na(values2)))
			{	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ....Plot property ",name2," as y\n",sep="")
			
				bounds2 <- property2$bounds
				if(is.na(bounds2[1]))
					bounds2[1] <- min(values2,na.rm=TRUE)
				if(is.na(bounds2[2]))
					bounds2[2] <- max(values2,na.rm=TRUE)
				
				plot.file <- paste(plot.folder,name1,".vs.",name2,".png",sep="")
				png(filename=plot.file,width=1000,height=1000,units="px",pointsize=20,bg="white")
				#pdf(file=plot.file,bg="white")
			
				plot(values1,values2,xlab=name1,ylab=name2,main=paste(name1,"vs",name2),xlim=bounds1, ylim=bounds2)
				dev.off()
			}
		}
	}
}
