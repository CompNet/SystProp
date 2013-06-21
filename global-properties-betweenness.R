#################################
# betweenness-related
#################################
properties[["betweenness centralization"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph)
	{	centralization.betweenness(graph=graph, directed=FALSE, nobigint=TRUE, normalized=TRUE)$centralization
	}
)
properties[["betweenness centrality average"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	if(length(cache$betweenness)==0)
			cache$betweenness <<- betweenness(graph=graph, directed=FALSE, weights=NULL, nobigint=TRUE, normalized=TRUE)
		mean(cache$betweenness,na.rm=TRUE)
	}
)
properties[["betweenness centrality stdev"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph)
	{	if(length(cache$betweenness)==0)
			cache$betweenness <<- betweenness(graph=graph, directed=FALSE, weights=NULL, nobigint=TRUE, normalized=TRUE)
		sd(cache$betweenness,na.rm=TRUE)
	}
)
properties[["betweenness centrality min"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph)
	{	if(length(cache$betweenness)==0)
			cache$betweenness <<- betweenness(graph=graph, directed=FALSE, weights=NULL, nobigint=TRUE, normalized=TRUE)
		min(cache$betweenness,na.rm=TRUE)
	}
)
properties[["betweenness centrality max"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph)
	{	if(length(cache$betweenness)==0)
			cache$betweenness <<- betweenness(graph=graph, directed=FALSE, weights=NULL, nobigint=TRUE, normalized=TRUE)
		max(cache$betweenness,na.rm=TRUE)
	}
)
properties[["betweenness centrality assortativity"]] <- list(
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph)
	{	if(length(cache$betweenness)==0)
			cache$betweenness <<- betweenness(graph=graph, directed=FALSE, weights=NULL, nobigint=TRUE, normalized=TRUE)
		assortativity(graph=graph, types1=cache$betweenness, types2=NULL, directed=FALSE)
	}
)
