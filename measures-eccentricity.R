#################################
# Processes eccentricity-related measures.
#################################
# processes eccentricities
process.eccentricity <- function(graph)
{	if(length(cache$eccentricity)==0)
	{	prop.file <- paste(net.folder,"eccentricity.txt",sep="")
		if(file.exists(prop.file))
			cache$eccentricity <<- as.matrix(read.table(prop.file))
		else
		{	cache$eccentricity <<- eccentricity(graph=graph, mode="all")
			write.table(cache$eccentricity,prop.file,row.names=FALSE,col.names=FALSE)
		}
	}
}

measures[["eccentricity-average"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	process.eccentricity(graph)
		mean(cache$eccentricity,na.rm=TRUE)
	}
)
measures[["eccentricity-stdev"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	process.eccentricity(graph)
		sd(cache$eccentricity,na.rm=TRUE)
	}
)
measures[["radius"]] <- list(		# aka minimal eccentricity
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	## old version >> not efficient
		##radius(graph=graph, mode="all")
		process.eccentricity(graph)
		max(cache$eccentricity,na.rm=TRUE)
	}
)
measures[["diameter"]] <- list(	# aka maximal eccentricity, maximal distance
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	## old version >> not efficient
		##diameter(graph=graph, directed=FALSE, unconnected=TRUE, weights=NULL)
		process.eccentricity(graph)
		min(cache$eccentricity,na.rm=TRUE)
	}
)
measures[["eccentricity-assortativity"]] <- list(
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	process.eccentricity(graph)
		assortativity(graph=graph, types1=cache$eccentricity, types2=NULL, directed=FALSE)
	}
)
