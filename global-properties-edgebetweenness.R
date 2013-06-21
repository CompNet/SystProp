#################################
# edgebetweeness-related
#################################
# processes a normalized version of the edge-betweenness
edge.betweenness.norm <- function(graph, directed=TRUE, ...)
{	bet <- edge.betweenness(graph, directed, ...)
	n <- vcount(graph)
	
	# undirected and directed nets should be treated differently
	# http://projects.skewed.de/graph-tool/changeset/36982c7a278d361111d7fa18ca55a52a129b33d2
	if(directed)
		norm <- n * (n-1)
	else
		norm <- n * (n-1) / 2
	
	bet / norm
}

properties[["edgebetweenness-centrality-average"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	if(length(cache$edgebetweennesscentrality)==0)
			cache$edgebetweennesscentrality <<- edge.betweenness.norm(graph=graph, directed=FALSE, weights=NULL)
		mean(cache$edgebetweennesscentrality,na.rm=TRUE)
	}
)
properties[["edgebetweenness-centrality-stdev"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	if(length(cache$edgebetweennesscentrality)==0)
			cache$edgebetweennesscentrality <<- edge.betweenness.norm(graph=graph, directed=FALSE, weights=NULL)
		sd(cache$edgebetweennesscentrality,na.rm=TRUE)
	}
)
properties[["edgebetweenness-centrality-min"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	if(length(cache$edgebetweennesscentrality)==0)
			cache$edgebetweennesscentrality <<- edge.betweenness.norm(graph=graph, directed=FALSE, weights=NULL)
		min(cache$edgebetweennesscentrality,na.rm=TRUE)
	}
)
properties[["edgebetweenness-centrality-max"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	if(length(cache$edgebetweennesscentrality)==0)
			cache$edgebetweennesscentrality <<- edge.betweenness.norm(graph=graph, directed=FALSE, weights=NULL)
		max(cache$edgebetweennesscentrality,na.rm=TRUE)
	}
)
