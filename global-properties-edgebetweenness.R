#################################
# edgebetweeness-related
#################################
# limit used to approximate edgebetweenness
edgebetweenness.cutoff <- -1	# <0 = no limit

# processes a normalized version of the edge-betweenness
normalize.edge.betweenness <- function(graph, values, directed=TRUE)
{	n <- vcount(graph)
	
	# undirected and directed nets should be treated differently
	# http://projects.skewed.de/graph-tool/changeset/36982c7a278d361111d7fa18ca55a52a129b33d2
	if(directed)
		norm <- n * (n-1)
	else
		norm <- n * (n-1) / 2
	
	result <- values / norm
	return(result)
}

# processes the raw edgebetweenness
process.edgebetweenness <- function(graph)
{	if(length(cache$edgebetweenness)==0)
	{	prop.file <- paste(net.folder,"edgebetweenness-centrality.txt",sep="")
		if(file.exists(prop.file))
			cache$edgebetweenness <<- as.matrix(read.table(prop.file))
		else
		{	cache$edgebetweenness <<- edge.betweenness.estimate(graph=graph, directed=FALSE, weights=NULL, cutoff=edgebetweenness.cutoff)
			write.table(cache$edgebetweenness,prop.file,row.names=FALSE,col.names=FALSE)
		}
	}
	#print(cache$edgebetweenness)
}

properties[["edgebetweenness-centrality-average"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	process.edgebetweenness(graph)
		normalized <- normalize.edge.betweenness(graph=graph,values=cache$edgebetweenness,directed=FALSE)
		mean(normalized,na.rm=TRUE)
	}
)
properties[["edgebetweenness-centrality-stdev"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	process.edgebetweenness(graph)
		normalized <- normalize.edge.betweenness(graph=graph,values=cache$edgebetweenness,directed=FALSE)
		sd(normalized,na.rm=TRUE)
	}
)
properties[["edgebetweenness-centrality-min"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	process.edgebetweenness(graph)
		normalized <- normalize.edge.betweenness(graph=graph,values=cache$edgebetweenness,directed=FALSE)
		min(normalized,na.rm=TRUE)
	}
)
properties[["edgebetweenness-centrality-max"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	process.edgebetweenness(graph)
		normalized <- normalize.edge.betweenness(graph=graph,values=cache$edgebetweenness,directed=FALSE)
		max(normalized,na.rm=TRUE)
	}
)
