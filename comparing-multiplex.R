# Compare the projections of multiplex networks, in order to 
# determine which one is the most appropriate. Our criteria are
# (by order of decreasing importance) :
# - Being connected
# - Highest number of nodes
# - Lowest density (projection tends to lead to dense graphs)
#
# setwd("~/eclipse/workspaces/Networks")
# setwd("D:/eclipse/workspaces/Networks")
#
# source("SystProp/comparing-multiplex.R")
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
{	data.folder <- "d:/networks/"
#	data.folder <- "c:/Temp/"
#	folders <- 1:5
	# all possible folders
	folders <- c(
			  2,  6, 11, 27, 28, 32, 56, 57, 59, 60, 62, 63, 64, 65, 66,
			 67, 68, 69, 70, 96, 98,
			102,104,116,141,142,143,144,155,156,157,158,161,162,163,165,
			169,170,360,361,362,363,365,366,452,462,475,476,482,503
	)
	# remove missing files (not converted yet)
	folders <- folders[!(folders %in% c(182,312,326,399,400,401,439,464,465))]
	# remove large files to speed up calculations
#	folders <- folders[!(folders %in% c(18,54:55,58,71:72,99,109,149:150,182,190:192,200,218:221,274:275,293:294:296,298:305,307:318,320,323,326:330,332:333,335,341:343:345,358:359,365,367,369,371:372,374:377,385:387:401,405,406,408,409,412,413,418,419,427,429:431,434:435,438:450,453:456,458,461,463:467,470,472,474))]
}else
{	#data.folder <- "/var/data/networks/"
	data.folder <- "/media/Samsung/networks/"
	# all possible folders
	#folders <- 1:611
	folders <- c(
			2,  6, 11, 27, 28, 32, 56, 57, 59, 60, 62, 63, 64, 65, 66,
			67, 68, 69, 70, 96, 98,
			102,104,116,141,142,143,144,155,156,157,158,161,162,163,165,
			169,170,360,361,362,363,365,366,452,462,475,476,482,503
	)
	# remove missing files (not converted yet)
	folders <- folders[!(folders %in% c(182,312,326,399,400,401,439,464,465))]
	# remove large files to speed up calculations
#	folders <- folders[!(folders %in% c(18,54:55,58,71:72,99,109,149:150,182,190:192,200,218:221,274:275,293:294:296,298:305,307:318,320,323,326:330,332:333,335,341:343:345,358:359,365,367,369,371:372,374:377,385:387:401,405,406,408,409,412,413,418,419,427,429:431,434:435,438:450,453:456,458,461,463:467,470,472,474))]
}
out.folder <- paste(data.folder,"_multiplex/",sep="")
dir.create(out.folder,showWarnings=FALSE)


#################################
# get existing folder list
#################################
paths <- list.files(path=data.folder, pattern="\\d{4}\\..*", all.files=TRUE, full.names=FALSE, recursive=FALSE, ignore.case = FALSE)
paths <- sort(paths)


#################################
# clean each network
#################################
prop.names <- c("Nodes","Links","Density","Connected")
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
			cat("[",format(end.time,"%a %d %b %Y %X"),"] Loading (",vcount(g)," nodes and ",ecount(g)," links) completed in ",format(total.time),"\n",sep="")
			
			# output folder
			net.folder <- paste(out.folder,filename,"/",sep="")
			dir.create(net.folder,showWarnings=FALSE)
			
			# remove all node attributes
			att.names <- list.vertex.attributes(graph=g)
			for(att.name in att.names)
				g <- remove.vertex.attribute(graph=g, name=att.name)
						
			# remove all link attributes (including weights)
			att.names <- list.edge.attributes(graph=g)
			for(att.name in att.names)
			{	if(att.name!="type")
					g <- remove.edge.attribute(graph=g, name=att.name)
			}
			
			# simplify multiplex network
			att.names <- list.edge.attributes(graph=g)
			prop <- NA
			if(any(att.names=="type"))
			{	# init
				types <- sort(unique(E(g)$type))
				prop <- matrix(ncol=length(prop.names),nrow=length(types))
				rownames(prop) <- types
				colnames(prop) <- prop.names
				
				# process and record all simplifications
				for(type in types)
				{	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Processing type ",type," (",length(which(E(g)$type==type)),")\n",sep="")
					g2 <- delete.edges(g,which(E(g)$type!=type))
					cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] #nodes=",vcount(g2)," #links=",ecount(g2),"\n",sep="")
					g2 <- remove.edge.attribute(graph=g2, name="type")
					
					# remove all isolates
					idx <- which(degree(graph=g2, mode="all")<1)
					g2 <- delete.vertices(graph=g2, v=idx)
					
					# update property matrix
					prop[type,] <- c(vcount(g2),ecount(g2),graph.density(g2),all(degree(g2)>0))
					cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] #nodes=",vcount(g2)," #links=",ecount(g2),"\n",sep="")
					
					# record simplified version
					type <- gsub(":","-",type)
					type <- gsub("/","-",type)
					data.file <- paste(net.folder,"network.",type,".net",sep="")
					write.graph(g2,data.file,format="pajek")
					
					g2 <- NULL; gc();
				}
			}else
			{	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] WARNING: Network not detected as multiplex\n",sep="")
			}
			
			# record property matrix
			if(!is.na(prop))
			{	data.file <- paste(net.folder,"properties.txt",sep="")
				write.table(prop,data.file)
			}
			
			end.time <- Sys.time();
			total.time <- end.time - start.time;
			cat("[",format(end.time,"%a %d %b %Y %X"),"] Processing completed in ",format(total.time),"\n",sep="")
		}
	}
}
