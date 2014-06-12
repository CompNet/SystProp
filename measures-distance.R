#################################
# Processes distance-related measures.
#################################
# processes distances
process.distance <- function(graph)
{	if(length(cache$distance)==0)
	{	prop.file <- paste(net.folder,"distance.txt",sep="")
		if(file.exists(prop.file))
			cache$distance <<- as.matrix(read.table(prop.file))
		else
		{	cache$distance <<- shortest.paths(graph=graph, 
				v=V(graph), to=V(graph),
				mode="all") #TODO could use a sample instead, if needed
			write.table(cache$distance,prop.file,row.names=FALSE,col.names=FALSE)
		}
	}
}

measures[["distance-average"]] <- list(
	type=numeric(),
	bounds=c(1,NA),
	foo=function(graph) 
	{	# exact process
		#average.path.length(graph=graph, directed=FALSE, unconnected=TRUE)
		#mean(cache$distance[!is.infinite(cache$distance)],na.rm=TRUE)
		
#		# approximation 1 - node pairs
#		sample.size <- 10^6 # number of distances to consider
#		ss <- min(vcount(g)/2,as.integer(sqrt(sample.size)))
#		nodes <- sample(1:vcount(g),ss*2)
#		sp <- shortest.paths(graph=g, v=nodes[1:ss], to=nodes[(ss+1):(ss*2)], mode="all", weights=NULL)
#		sp <- sp[!is.infinite(sp)]
#		mean(sp)
		
		# approximation 2 - nodes vs. rest
		sample.size <- 10^8 # number of distances to consider
		ss <- min(vcount(g),max(1,as.integer(sample.size/vcount(g))))
		nodes <- sample(1:vcount(g),ss)
		sp <- shortest.paths(graph=g, v=nodes, to=V(g),mode="all", weights=NULL)
		sp <- sp[!is.infinite(sp)]
		mean(sp)
	}
)
#measures[["girth"]] <- list(		# cycle of maximal length
#	type=integer(),
#	bounds=c(1,NA),
#	foo=function(graph) 
#	{	girth(graph=graph, circle=FALSE)$girth
#	}
#)
