#################################
# connectivity-related
#################################
#params[["link connectivity average"]] <- list(
#	type=numeric(),
#	bounds=c(0,NA),
#	foo=function(graph) 
#	{	if(length(cache$linkconnectivity)==0)
#			cache$linkconnectivity <<- edge.connectivity(graph=graph, source=V(g), target=V(g), checks=TRUE)
#		mean(cache$linkconnectivity,na.rm=TRUE)
#	}
#)
#params[["link connectivity stdev"]] <- list(
#	type=numeric(),
#	bounds=c(0,NA),
#	foo=function(graph) 
#	{	if(length(cache$linkconnectivity)==0)
#			cache$linkconnectivity <<- edge.connectivity(graph=graph, source=V(g), target=V(g), checks=TRUE)
#		sd(cache$linkconnectivity,na.rm=TRUE)
#	}
#)
properties[["adhesion"]] <- list(		# aka minimal link connectivity
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	graph.adhesion(graph=graph, checks=TRUE)
	}
)
#params[["link connectivity max"]] <- list(
#	type=integer(),
#	bounds=c(0,NA),
#	foo=function(graph) 
#	{	if(length(cache$linkconnectivity)==0)
#			cache$linkconnectivity <<- edge.connectivity(graph=graph, source=V(g), target=V(g), checks=TRUE)
#		max(cache$linkconnectivity,na.rm=TRUE)
#	}
#)
#params[["node connectivity average"]] <- list(
#	type=numeric(),
#	bounds=c(0,NA),
#	foo=function(graph) 
#	{	if(length(cache$nodeconnectivity)==0)
#			cache$nodeconnectivity <<- vertex.connectivity(graph=graph, source=V(g), target=V(g), checks=TRUE)
#		mean(cache$nodeconnectivity,na.rm=TRUE)
#	}
#)
#params[["node connectivity stdev"]] <- list(
#	type=numeric(),
#	bounds=c(0,NA),
#	foo=function(graph) 
#	{	if(length(cache$nodeconnectivity)==0)
#			cache$nodeconnectivity <<- vertex.connectivity(graph=graph, source=V(g), target=V(g), checks=TRUE)
#		sd(cache$nodeconnectivity,na.rm=TRUE)
#	}
#)
properties[["cohesion"]] <- list(		# aka minimal node connectivity
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	graph.cohesion(graph=graph, checks=TRUE)
	}
)
#params[["node connectivity max"]] <- list(
#	type=integer(),
#	bounds=c(0,NA),
#	foo=function(graph) 
#	{	if(length(cache$nodeconnectivity)==0)
#			cache$nodeconnectivity <<- vertex.connectivity(graph=graph, source=V(g), target=V(g), checks=TRUE)
#		max(cache$nodeconnectivity,na.rm=TRUE)
#	}
#)
