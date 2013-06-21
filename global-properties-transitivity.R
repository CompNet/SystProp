#################################
# transitivity-related
#################################
properties[["transitivity-global"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	transitivity(graph, type="globalundirected",weights=NULL,isolates="zero")
	}
)
properties[["transitivity-local-average"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	if(length(cache$localtransitivity)==0)
			cache$localtransitivity <<- transitivity(graph, type="localundirected",weights=NULL,isolates="zero")
		mean(cache$localtransitivity,na.rm=TRUE)
	}
)
properties[["transitivity-local-stdev"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	if(length(cache$localtransitivity)==0)
			cache$localtransitivity <<- transitivity(graph, type="localundirected",weights=NULL,isolates="zero")
		sd(cache$localtransitivity,na.rm=TRUE)
	}
)
properties[["transitivity-local-min"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	if(length(cache$localtransitivity)==0)
			cache$localtransitivity <<- transitivity(graph, type="localundirected",weights=NULL,isolates="zero")
		min(cache$localtransitivity,na.rm=TRUE)
	}
)
properties[["transitivity-local-max"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	if(length(cache$localtransitivity)==0)
			cache$localtransitivity <<- transitivity(graph, type="localundirected",weights=NULL,isolates="zero")
		max(cache$localtransitivity,na.rm=TRUE)
	}
)
properties[["transitivity-assortativity"]] <- list(
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	if(length(cache$localtransitivity)==0)
			cache$localtransitivity <<- transitivity(graph, type="localundirected",weights=NULL,isolates="zero")
		assortativity(graph=graph, types1=cache$localtransitivity, types2=NULL, directed=FALSE)
	}
)
