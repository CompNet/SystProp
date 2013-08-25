# setwd("~/eclipse/workspaces/Networks")
# setwd("c:/eclipse/workspaces/Networks")
#
# source("SystProp/cleaning.R")
###################################################

#################################
# load dependencies
#################################
library(igraph)

#################################
# setup parameters
#################################
os <- .Platform$OS.type
if(os=="windows")
{	data.folder <- "f:/networks/"
#	data.folder <- "c:/Temp/"
#	folders <- 1:5
	# all possible folders
	folders <- 1:502
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
out.folder <- paste(data.folder,"cleaned/",sep="")


#################################
# load table for bipartite and multiplex graphs
#################################
simpref.file <- paste(data.folder,"simplification.preferences.txt",sep="")
simplif.pref <- read.table(simpref.file, row.names=1)


#################################
# get existing folder list
#################################
paths <- list.files(path=data.folder, pattern="\\d{4}\\..*", all.files=TRUE, full.names=FALSE, recursive=FALSE, ignore.case = FALSE)
paths <- sort(paths)


#################################
# clean each network
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
	{	# load network
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
			end.time <- Sys.time();
			total.time <- end.time - start.time;
			cat("[",format(end.time,"%a %d %b %Y %X"),"] Loading (",vcount(g)," nodes and ",ecount(g)," links) completed in ",total.time,"\n",sep="")
			
			# possibly project bipartite network
			att.names <- list.vertex.attributes(graph)
			if(any(att.names=="type"))
			{	# retrieve preferred node type 
				pref.type <- simplif.pref[f,1]
				
				# perform both projections
				vals <- rep(TRUE,vcount(g))
				vals[which(V(g)$type==pref.type)] <- FALSE
				res <- bipartite.projection(graph=g, types=type, multiplicity=TRUE)
				
				# keep only the desired one
				g <- res[[1]]
			}
			
			
			# retain only one type of link in multiplex networks
			att.names <- list.edge.attributes(graph)
			if(any(att.names=="type"))
			{	# retrieve preferred link type 
				pref.type <- simplif.pref[f,2]
				
				# remove corresponding links
				idx <- which(E(g)$type==pref.type)
				g <- delete.edges(graph=g, edges=idx)
			}
			
			# remove all isolates
			idx <- which(degree(graph=g, mode="all")<1)
			g <- delete.vertices(graph=g, v=idx)
			
			# remove all node attributes
			att.names <- list.vertex.attributes(graph)
			for(att.name in att.names)
				g <- remove.vertex.attribute(graph=g, name=att.name)
			
			# remove all link attributes (including weights)
			att.names <- list.edge.attributes(graph)
			for(att.name in att.names)
				g <- remove.edge.attribute(graph=g, name=att.name)
			
			# make the graph undirected
			g <- as.undirected(graph=g, mode="collapse")
			
			# remove loops and multiple links
			if(!is.simple(g))
				g <- simplify(graph=g, remove.multiple=TRUE, remove.loops=TRUE)
			
			# record cleaned version of the network
			net.folder <- paste(out.folder,filename,"/",sep="")
			data.file <- paste(net.folder,"network.net",sep="")
			write.graph(g,data.file,format="pajek")
			
			end.time <- Sys.time();
			total.time <- end.time - start.time;
			cat("[",format(end.time,"%a %d %b %Y %X"),"] Processing completed in ",total.time,"\n",sep="")
		}
	}
}
