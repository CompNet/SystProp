#################################
# Processes closeness-related measures.
#################################
# limit used to approximate closeness
closeness.cutoff <- -1	# <0 = no limit

# processes a normalized version of the closeness
normalize.closeness <- function(graph, values)
{	n <- vcount(graph)
	
	result <- (n-1) * values
	return(result)
}

# processes the raw closeness
process.closeness <- function(graph)
{	if(length(cache$closeness)==0)
	{	prop.file <- paste(net.folder,"closeness-centrality.txt",sep="")
		if(file.exists(prop.file))
			cache$closeness <<- as.matrix(read.table(prop.file))
		else
		{	# we don't use igraph normalization, because some future processes might require raw values
			cache$closeness <<- closeness.estimate(graph=graph, mode="all", weights=NULL, cutoff=closeness.cutoff)
			write.table(cache$closeness,prop.file,row.names=FALSE,col.names=FALSE)
		}
	}
}

measures[["closeness-centralization"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph)
	{	process.closeness(graph)
		# we're not using the igraph centralization.closeness function, because we want to cache centrality values
		# formula taken from Freeman L. C., "Centrality in Social Networks I: Conceptual Clarification", Social Networks, 1(3):215-239, 1978.
		n <- vcount(graph)
		(max(cache$closeness)*n - sum(cache$closeness))*(2*n - 3) / (n^2 - 3*n + 2)
	}
)
measures[["closeness-centrality-average"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	process.closeness(graph)
		normalized <- normalize.closeness(graph=graph,values=cache$closeness)
		mean(normalized,na.rm=TRUE)
	}
)
measures[["closeness-centrality-stdev"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	process.closeness(graph)
		normalized <- normalize.closeness(graph=graph,values=cache$closeness)
		sd(normalized,na.rm=TRUE)
	}
)
measures[["closeness-centrality-min"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	process.closeness(graph)
		normalized <- normalize.closeness(graph=graph,values=cache$closeness)
		min(normalized,na.rm=TRUE)
	}
)
measures[["closeness-centrality-max"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	process.closeness(graph)
		normalized <- normalize.closeness(graph=graph,values=cache$closeness)
		max(normalized,na.rm=TRUE)
	}
)
measures[["closeness-centrality-assortativity"]] <- list(
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	process.closeness(graph)
		assortativity(graph=graph, types1=cache$closeness, types2=NULL, directed=FALSE)
	}
)
