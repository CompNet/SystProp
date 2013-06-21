#################################
# eigenvector-related
#################################
properties[["eigenvector-centralization"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	centralization.evcent(graph=graph, directed=FALSE, scale=TRUE, normalized=TRUE)$centralization
	}
)
properties[["eigenvector-centrality-average"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	if(length(cache$eigenvectorcentrality)==0)
			cache$eigenvectorcentrality <<- evcent(graph=graph, directed=FALSE, scale=TRUE, weights=NULL)$vector
		mean(cache$eigenvectorcentrality,na.rm=TRUE)
	}
)
properties[["eigenvector-centrality-stdev"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	if(length(cache$eigenvectorcentrality)==0)
			cache$eigenvectorcentrality <<- evcent(graph=graph, directed=FALSE, scale=TRUE, weights=NULL)$vector
		sd(cache$eigenvectorcentrality,na.rm=TRUE)
	}
)
properties[["eigenvector-centrality-min"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	if(length(cache$eigenvectorcentrality)==0)
			cache$eigenvectorcentrality <<- evcent(graph=graph, directed=FALSE, scale=TRUE, weights=NULL)$vector
		#print(cache$eigenvectorcentrality)		
		min(cache$eigenvectorcentrality,na.rm=TRUE)
	}
)
#properties[["eigenvector-centrality-max"]] <- list(
#	type=numeric(),
#	bounds=c(0,1), # TODO normalized to be 1 at most in a graph, so not numericly usefull...
#	foo=function(graph) 
#	{	if(length(cache$eigenvectorcentrality)==0)
#			cache$eigenvectorcentrality <<- evcent(graph=graph, directed=FALSE, scale=TRUE, weights=NULL)$vector
#		max(cache$eigenvectorcentrality,na.rm=TRUE)
#	}
#)
properties[["eigenvector-centrality-assortativity"]] <- list(
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	if(length(cache$eigenvectorcentrality)==0)
			cache$eigenvectorcentrality <<- evcent(graph=graph, directed=FALSE, scale=TRUE, weights=NULL)$vector
		assortativity(graph=graph, types1=cache$eigenvectorcentrality, types2=NULL, directed=FALSE)
	}
)
