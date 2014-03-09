#################################
# Processes betweenness-related measures.
#################################
# limit used to approximate betweenness
betweenness.cutoff <- -1	# <0 = no limit

# processes a normalized version of the betweenness
normalize.betweenness <- function(graph, values, directed=TRUE)
{	n <- vcount(graph)
	
	# modif: undirected and directed nets should be treated differently
	# http://projects.skewed.de/graph-tool/changeset/36982c7a278d361111d7fa18ca55a52a129b33d2
	if(directed)
		norm <- (n^2 - 3*n + 2)
	else
		norm <- (n^2 - 3*n + 2) / 2
	
	result <- values / norm
	return(result)
}

# processes the raw betweenness
process.betweenness <- function(graph)
{	if(length(cache$betweenness)==0)
	{	prop.file <- paste(net.folder,"betweenness-centrality.txt",sep="")
		if(file.exists(prop.file))
			cache$betweenness <<- as.matrix(read.table(prop.file))
		else
		{	# we don't use igraph normalization, because some future processes might require raw values
			cache$betweenness <<- betweenness.estimate(graph=graph, directed=FALSE, weights=NULL, nobigint=TRUE, cutoff=betweenness.cutoff)
			write.table(cache$betweenness,prop.file,row.names=FALSE,col.names=FALSE)
		}
	}
	#print(cache$betweenness)
}

measures[["betweenness-centralization"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph)
	{	process.betweenness(graph=graph)
		# we're not using the igraph centralization.betweenness function, because we want to cache centrality values
		# formula taken from Freeman L. C., "Centrality in Social Networks I: Conceptual Clarification", Social Networks, 1(3):215-239, 1978.
		n <- vcount(graph)
		2 * (max(cache$betweenness)*n - sum(cache$betweenness)) / (n^3 - 4*n^2 + 5*n - 2)
	}
)
measures[["betweenness-centrality-average"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	process.betweenness(graph=graph)
		normalized <- normalize.betweenness(graph=graph,values=cache$betweenness,directed=FALSE)
		mean(normalized,na.rm=TRUE)
	}
)
measures[["betweenness-centrality-stdev"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph)
	{	process.betweenness(graph=graph)
		normalized <- normalize.betweenness(graph=graph,values=cache$betweenness,directed=FALSE)
		sd(normalized,na.rm=TRUE)
	}
)
measures[["betweenness-centrality-min"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph)
	{	process.betweenness(graph=graph)
		normalized <- normalize.betweenness(graph=graph,values=cache$betweenness,directed=FALSE)
		min(normalized,na.rm=TRUE)
	}
)
measures[["betweenness-centrality-max"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph)
	{	process.betweenness(graph=graph)
		normalized <- normalize.betweenness(graph=graph,values=cache$betweenness,directed=FALSE)
		max(normalized,na.rm=TRUE)
	}
)
measures[["betweenness-centrality-assortativity"]] <- list(
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph)
	{	process.betweenness(graph=graph)
		assortativity(graph=graph, types1=cache$betweenness, types2=NULL, directed=FALSE)
	}
)
