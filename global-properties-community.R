#################################
# community-related
#################################
properties[["modularity"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph)
	{	if(length(cache$communities)==0)
			cache$communities <<- multilevel.community(graph=as.undirected(graph), weights=NULL)
		max(cache$communities$modularity,na.rm=TRUE)
	}
)
properties[["community number"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph)
	{	if(length(cache$communities)==0)
			cache$communities <<- multilevel.community(graph=as.undirected(graph), weights=NULL)
		length(unique(cache$communities$membership))
	}
)
