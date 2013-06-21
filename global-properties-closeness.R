#################################
# closeness-related
#################################
properties[["closeness-centralization"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph)
	{	centralization.closeness(graph=graph, mode="all",normalized=TRUE)$centralization
	}
)
properties[["closeness-centrality-average"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	if(length(cache$closeness)==0)
			cache$closeness <<- closeness(graph=graph, mode="all", weights=NULL, normalized=TRUE)
		mean(cache$closeness,na.rm=TRUE)
	}
)
properties[["closeness-centrality-stdev"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	if(length(cache$closeness)==0)
			cache$closeness <<- closeness(graph=graph, mode="all", weights=NULL, normalized=TRUE)
		sd(cache$closeness,na.rm=TRUE)
	}
)
properties[["closeness-centrality-min"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	if(length(cache$closeness)==0)
			cache$closeness <<- closeness(graph=graph, mode="all", weights=NULL, normalized=TRUE)
		min(cache$closeness,na.rm=TRUE)
	}
)
properties[["closeness-centrality-max"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	if(length(cache$closeness)==0)
			cache$closeness <<- closeness(graph=graph, mode="all", weights=NULL, normalized=TRUE)
		max(cache$closeness,na.rm=TRUE)
	}
)
properties[["closeness-centrality-assortativity"]] <- list(
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	if(length(cache$closeness)==0)
			cache$closeness <<- closeness(graph=graph, mode="all", weights=NULL, normalized=TRUE)
		assortativity(graph=graph, types1=cache$closeness, types2=NULL, directed=FALSE)
	}
)
