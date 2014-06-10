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
	{	average.path.length(graph=graph, directed=FALSE, unconnected=TRUE)
		# approximation
		#process.distance(graph)
		#mean(cache$distance[!is.infinite(cache$distance)],na.rm=TRUE)
	}
)
#measures[["girth"]] <- list(		# cycle of maximal length
#	type=integer(),
#	bounds=c(1,NA),
#	foo=function(graph) 
#	{	girth(graph=graph, circle=FALSE)$girth
#	}
#)
